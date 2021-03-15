
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
		letters: "abcdefghijklmnopqrstuvwxyz",
		trimLength: 100,
		mode: "single",
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

		comment: {
			keyDelay: 80,
			loading: "(loading...)",
			toStart: "(done. press enter.)",
			start: "welcome to tonight's exciting match! " +
				"it's menacing {one} against favorite {two}! ",
			rules: "remember the rules: " +
				"in turns, contestants type words that start with the assigned letter, " +
				"and which contain the last letter of the previous word, then press enter. " +
				"no repeats, and watch the time limit! " +
				"first to seize three rounds is the victor. " +
				"now, let the match begin!",
			roundStart: [
				"let's see who comes out victorious in the next round!",
				"now for another round full of suspense!",
				"who will make the best of this round?"
			],
			turn: [
				"{turn}'s turn!",
				"{turn} starts!",
				"{turn} speaks!",
				"{turn} serves!"
			],
			letterAnnounce: [
				"featuring letter \"{letter}\"!",
				"time for letter \"{letter}\"!",
				"watch for letter \"{letter}\"!",
				"here we go, letter \"{letter}\"!",
				"it's letter \"{letter}\"!",
				"we want letter \"{letter}\"!",
				"show us letter \"{letter}\"!"
			],
			turnAndLetter: [
				"starting, it's {turn} with \"{letter}\"!",
				"it's {turn} with \"{letter}\"!",
				"serving is {turn}, with \"{letter}\"!",
				"here we go, turn for {turn}, with \"{letter}\"!",
				"watch for that \"{letter}\", {turn}!",
				"time for \"{letter}\", {turn}!",
				"featuring \"{letter}\", speaks {turn}!",
				"we want letter \"{letter}\", {turn}!",
				"show us that \"{letter}\", {turn}!"
			],
			interjection: [
				"ooh!",
				"nice!",
				"good!",
				"safe!",
				"wow!",
				"works!",
				"fair!"
			],
			mistake: {
				startLetter: [
					"did not start with \"{letter}\"!"
				],
				includeLetter: [
					"does not contain previous word's \"{letter}\"!"
				],
				alreadyPlayed: [
					"we've seen that word before!",
					"that one's a repeat!"
				],
				doesntExist: [
					"is that english?",
					"no such word in my dictionary!",
					"what does that even mean?"
				],
				timeOut: [
					"time is up!",
					"time ran out!",
					"no more time!"
				]
			},
			roundEnd: {
				point: [
					"brilliant point for {winner}!",
					"{loser} wastes a chance!",
					"tough luck!",
					"what a shock!",
					"{winner} scores!",
					"too bad for {loser}!",
					"close, but no dice!",
					"it's {loser}'s miss!",
					"{winner} takes this point!"
				],
				winning: [
					"{winner} has the lead!",
					"{winner} is ahead!",
					"{loser} needs to step up!",
					"{loser} is not looking good!"
				],
				tie: [
					"it's tied!",
					"{points} all!",
					"it's a battle of noses!",
					"both sides equal!",
					"both with {points}!"
				]
			},
			scoreTally: [
				"we have our game at {one} {pointsOne}, {two} {pointsTwo}!",
				"the panel reads {one} {pointsOne} versus {two} {pointsTwo}.",
				"{one} at {pointsOne}, {two} at {pointsTwo}!"
			],
			gameEnd: "and it's settled! {winner} defeats {loser} {winnerPoints} to {loserPoints} in a match to remember! " +
				"we look forward to when these two titans have another face-off. " +
				"see you next time!"
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

	var includeLetter; // String
	var startLetter; // String
	var ignoreKeys = false;

	cfg.time.depletionRate = cfg.time.depletionRate * (cfg.time.depletionInterval / 1000);

	window.addEvent("domready", onDOMReady);

	function onDOMReady(e) {
		cfg.container = $("game");
		title = $("title");

		setMode(cfg.mode);

		commentator = new spwords.Commentator(cfg);
		options = new spwords.Options(title);
		options.getSelected().add(onOptionSelected);

		// Events.
		window.addEvent(AGJ.event.key.down, onKeyDown);

		// Get words.
		commentator.comment(cfg.comment.loading, true);
		var request = new Request({ url: "data/words.txt" });
		request.addEvent("success", onWordsObtained);
		request.get();
	}

	/////

	function ready() {
		signal.enterPressed.addOnce(start);
		commentator.comment(fixNames(cfg.comment.toStart));
	}

	function start() {
		if (cfg.mode === "double") {
			setupContestant("one", "right", spwords.Player);
			setupContestant("two", "left", spwords.Player);
		} else {
			setupContestant("one", "computer", spwords.Computer);
			setupContestant("two", "player", spwords.Player);
		}

		title.removeClass("active");
		title.getElements(".options").removeClass("active");
		commentator.comment(fixNames(cfg.comment.start));
		commentator.comment(fixNames(cfg.comment.rules));
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
		// commentator.comment(cfg.comment.turn.getRandom().replace("{turn}", names[startID]), true);
		// commentator.comment(cfg.comment.letterAnnounce.getRandom().replace("{letter}", startLetter), true);
		commentator.comment(cfg.comment.turnAndLetter.getRandom().replace("{turn}", names[startID]).replace("{letter}", startLetter), true);
	}

	function getOtherID(id) { // String
		return id === "one" ? "two" : "one";
	}

	function finishRound(winner, error) {
		var loser = getOtherID(winner);
		points[winner]++;

		var mistakeComment = cfg.comment.mistake[error].getRandom();
		if (error === spwords.mistake.startLetter)
			mistakeComment = mistakeComment.replace("{letter}", startLetter);
		if (error === spwords.mistake.includeLetter)
			mistakeComment = mistakeComment.replace("{letter}", includeLetter);
		commentator.comment(mistakeComment, true);

		if (points[winner] < cfg.winPoints && points[loser] < cfg.winPoints) {
			// Round end, but game continues.
			var pointComment = cfg.comment.roundEnd.point.getRandom();
			pointComment = pointComment.replace("{winner}", names[winner]).replace("{loser}", names[loser]);
			commentator.comment(pointComment);
			commentator.comment(getScoreComment());
			if (points[winner] !== points[loser]) {
				var winningComment = cfg.comment.roundEnd.winning.getRandom();
				winningComment = winningComment.replace("{winner}", names[winner]);
				winningComment = winningComment.replace("{loser}", names[loser]);
				commentator.comment(winningComment);
			} else {
				commentator.comment(cfg.comment.roundEnd.tie.getRandom().replace("{points}", points[winner]));
			}
			commentator.comment(cfg.comment.roundStart.getRandom());
			startRound(winner);

		} else {
			// Game end.
			var gameEndComment = cfg.comment.gameEnd;
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
		trace("setting mode", name);
		var body = $$("body")[0];
		body.set("class", name + "-mode");
		cfg.mode = name;
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
		var text = cfg.comment.scoreTally.getRandom();
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

	function onOptionSelected(name) {
		setMode(name);
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
			commentator.comment(cfg.comment.interjection.getRandom());
		}
	}

	function onGameEnd() {
		title.addClass("active");
	}


})(this);

