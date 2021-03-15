
/*jshint mootools:true*/
/*global signals:false*/
/*global AGJ:false*/
;(function (that) {
	"use strict";

	// Imports.
	var trace = AGJ.trace;
	var Class = AGJ.Class;
	var spwords = AGJ.projects.spwords;
	var Signal = signals.Signal;

	AGJ.loggingIsEnabled = true;

	var cfg = {
		winPoints: 3,
		trimLength: 100,
		mode: "single",
		language: "en",
		time: {
			depletionRate: 0.05, // fraction per second
			depletionInterval: 100 // ms
		},
		computer: {
			key: {
				delay: {
					min: 20,
					max: 150
				},
				errorProbability: 0.005
			},
			word: {
				delay: {
					min: 200,
					max: 2500
				},
				findAttempts: 20
			}
		},

		letters: null,
		names: null,
		comment: {
			keyDelay: 80,
			lines: null
		},

		container: null,
		words: null,
		wordsByLetter: null,
		playedWords: ""
	};

	var signal = {
		enterPressed: new Signal()
	};

	var names = {
		one: null,
		two: null
	};
	var contestants = {};
	var points = {};
	var commentator;
	var options;

	var title; // Element
	var request;

	var includeLetter; // String
	var startLetter; // String
	var ignoreKeys = false;

	cfg.time.depletionRate = cfg.time.depletionRate * (cfg.time.depletionInterval / 1000);

	window.addEvent("domready", onDOMReady);

	function onDOMReady(e) {
		cfg.container = $("game");
		title = $("title");

		var lang = getLanguageFromHash();
		if (!lang)
			lang = cfg.language;
		setLanguage(lang);
		setMode(cfg.mode);

		commentator = new spwords.Commentator(cfg);
		options = new spwords.Options(title);
		options.getSelected().add(onOptionSelected);

		// Events.
		// window.addEvent("hashchange", onHashChange);
		window.addEvent(AGJ.event.key.press, onKeyDown);

		load();
	}

	function getLanguageFromHash() { // String
		var hash = window.location.hash;
		var lang;
		if (hash) {
			lang = (hash.charAt(0) === "#" ? hash.substr(1) : hash).substr(0, 2);
		}
		if (lang) {
			var languages = AGJ.object.getKeys(spwords.languages);
			if (languages.indexOf(lang) < 0)
				lang = null;
		}
		trace(hash, lang);
		return lang;
	}

	/////

	function reset() {
		request.removeEvent("success", onWordsObtained);
		commentator.interrupt();
		cfg.container.empty();
	}

	function load() {
		// Get words.
		commentator.comment(cfg.comment.lines.loading, true);
		request = new Request({ url: "data/words-" + cfg.language + ".txt" });
		request.addEvent("success", onWordsObtained);
		request.get();
	}

	function ready() {
		signal.enterPressed.addOnce(start);
		commentator.comment(fixNames(cfg.comment.lines.toStart));
	}

	function start() {
		if (cfg.mode === "double") {
			setupContestant("one", cfg.names.right, spwords.Player);
			setupContestant("two", cfg.names.left, spwords.Player);
		} else {
			setupContestant("one", cfg.names.computer, spwords.Computer);
			setupContestant("two", cfg.names.player, spwords.Player);
		}

		title.removeClass("active");
		title.getElements(".options").removeClass("active");
		title.getElements("#language-select").removeClass("active");
		commentator.comment(fixNames(cfg.comment.lines.start));
		commentator.comment(fixNames(cfg.comment.lines.rules));
		commentator.getAllFinished().addOnce(startRound);
	}

	function startRound(previousWinner) {
		trim();

		includeLetter = null;
		contestants.one.reset();
		contestants.two.reset();

		var startID = "one";
		if (cfg.mode !== "single") {
			if (previousWinner)
				startID = getOtherID(previousWinner);
			else
				startID = ["one", "two"].getRandom();
		}
		startLetter = getRandomLetter();
		commentator.getAllFinished().addOnce(AGJ.getCallback(contestants[startID].startInput, [startLetter, includeLetter], contestants[startID]));
		// commentator.comment(cfg.comment.lines.turn.getRandom().replace("{turn}", names[startID]), true);
		// commentator.comment(cfg.comment.lines.letterAnnounce.getRandom().replace("{letter}", startLetter), true);
		commentator.comment(cfg.comment.lines.turnAndLetter.getRandom().replace("{turn}", names[startID]).replace("{letter}", startLetter), true);
	}

	function getOtherID(id) { // String
		return id === "one" ? "two" : "one";
	}

	function finishRound(winner, error) {
		var loser = getOtherID(winner);
		points[winner]++;

		var mistakeComment = cfg.comment.lines.mistake[error].getRandom();
		if (error === spwords.mistake.startLetter)
			mistakeComment = mistakeComment.replace("{letter}", startLetter);
		if (error === spwords.mistake.includeLetter)
			mistakeComment = mistakeComment.replace("{letter}", includeLetter);
		commentator.comment(mistakeComment, true);

		if (points[winner] < cfg.winPoints && points[loser] < cfg.winPoints) {
			// Round end, but game continues.
			var pointComment = cfg.comment.lines.roundEnd.point.getRandom();
			pointComment = pointComment.replace("{winner}", names[winner]).replace("{loser}", names[loser]);
			commentator.comment(pointComment);
			commentator.comment(getScoreComment());
			if (points[winner] !== points[loser]) {
				var winningComment = cfg.comment.lines.roundEnd.winning.getRandom();
				winningComment = winningComment.replace("{winner}", names[winner]);
				winningComment = winningComment.replace("{loser}", names[loser]);
				commentator.comment(winningComment);
			} else {
				commentator.comment(cfg.comment.lines.roundEnd.tie.getRandom().replace("{points}", points[winner]));
			}
			commentator.comment(cfg.comment.lines.roundStart.getRandom());
			startRound(winner);

		} else {
			// Game end.
			var gameEndComment = cfg.comment.lines.gameEnd;
			gameEndComment = gameEndComment.replace("{winner}", names[winner]).replace("{loser}", names[loser]);
			gameEndComment = gameEndComment.replace("{winnerPoints}", points[winner]).replace("{loserPoints}", points[loser]);
			ignoreKeys = true;
			commentator.comment(gameEndComment, true);

			commentator.getAllFinished().addOnce(onGameEnd);
		}
	}

	function setupContestant(id, name, type) {
		names[id] = name;
		contestants[id] = new type(cfg, new spwords.Bar(cfg, $$("." + id + ".time-bar")[0]), id);
		points[id] = 0;
		contestants[id].getFinished().add(onContestantFinished);
	}

	function setMode(name) {
		var body = $$("body")[0];
		var classes = body.get("class").split(" ");
		for (var i = classes.length - 1; i >= 0; i--) {
			if (classes[i].endsWith("-mode"))
				classes.splice(i, 1);
		}
		body.set("class", name + "-mode " + classes.join(" "));

		cfg.mode = name;
	}

	function changeLanguage(id) {
		window.location.hash = "#" + id;
		reset();
		setLanguage(id);
		load();
	}

	function setLanguage(id) {
		trace("Setting language:", id);

		cfg.comment.lines = spwords.languages[id].comments;
		cfg.letters = spwords.languages[id].letters;
		cfg.names = spwords.languages[id].names;

		var body = $$("body")[0];
		var classes = body.get("class").split(" ");
		for (var i = classes.length - 1; i >= 0; i--) {
			if (classes[i].startsWith("language-"))
				classes.splice(i, 1);
		}
		body.set("class", "language-" + id + " " + classes.join(" "));

		cfg.language = id;
	}

	function addPlayedWord(thisWord) {
		if (thisWord) {
			cfg.playedWords += "\n" + thisWord;
			includeLetter = thisWord.substr(-1);
		}
	}

	function trim() {
		var nodes = cfg.container.childNodes;
		var count = 0;
		for (var i = nodes.length - 1; i >= 0; --i) {
			var node = $(nodes[i]);
			if (count < cfg.trimLength) {
				count += node.textContent.length;
			} else {
				cfg.container.removeChild(node);
			}
		}
	}

	function getRandomLetter() { // String
		return cfg.letters.charAt(Math.randomInt(cfg.letters.length));
	}

	function getScoreComment() { // String
		var text = cfg.comment.lines.scoreTally.getRandom();
		return fixNames(text);
	}

	function fixNames(text) { // String
		return text.replace("{one}", names.one).
			replace("{two}", names.two).
			replace("{pointsOne}", points.one).
			replace("{pointsTwo}", points.two);
	}


	///// Events.

	function onWordsObtained(contents, xml) {
		cfg.words = contents;

		cfg.wordsByLetter = {};
		while (contents.length > 0) {
			var letter = contents.charAt(0);
			var regex = new RegExp("(^" + letter + ".*$\\s*)+[^" + letter + "]", "m");
			var groups = contents.match(regex);
			if (groups) {
				var group = groups[0];
				var index = contents.search(regex) + group.length - 1;
				contents = contents.substr(index);
				group = group.split("\n");
				group.pop();
				cfg.wordsByLetter[letter] = group;
			} else {
				break;
			}
		}

		ready();
	}

	function onKeyDown(e) {
		if (e.meta || e.control || e.alt)
			return;
		
		e.preventDefault();

		if (!commentator.isCommenting() && e.key === "enter") {
			signal.enterPressed.dispatch();
		}
	}

	function onOptionSelected(type, name) {
		trace("Option selected:", type, name);
		if (type === "mode")
			setMode(name);
		else if (type === "language")
			changeLanguage(name);
	}

	function onCommentInterrupted() {
		signal.enterPressed.dispatch();
	}

	function onContestantFinished(cont, word, error) {
		var id = AGJ.object.getKeyFromValue(contestants, cont);
		var other = getOtherID(id);
		if (error) {
			finishRound(other, error);
		} else {
			addPlayedWord(word);
			commentator.getAllFinished().addOnce(AGJ.getCallback(contestants[other].startInput, [startLetter, includeLetter], contestants[other]));
			commentator.comment(cfg.comment.lines.interjection.getRandom());
		}
	}

	function onGameEnd() {
		title.addClass("active");
	}


})(this);

