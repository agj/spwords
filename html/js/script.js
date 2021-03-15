
/*jshint mootools:true*/
/*global signals:false*/
/*global AGJ:false*/
;(function (that) {
	"use strict";

	// Imports.
	var trace = AGJ.trace;
	var Class = AGJ.Class;
	var Signal = signals.Signal;

	AGJ.loggingIsEnabled = false;

	var cfg = {
		winPoints: 3,

		comment: {
			keyDelay: 80,
			loading: "(loading...)",
			toStart: "(done. press enter.)",
			start: "welcome to tonight's exciting match! " +
				"it's menacing computer against favorite player! ",
			rules: "remember the rules: " +
				"contestants type a word starting with the round's assigned letter, " +
				"and containing the last letter of their opponent's play, then press enter. " +
				"no repeats, and watch the time limit! " +
				"first to seize three rounds is the victor. " +
				"computer has the first say. now, let the match begin!",
			roundStart: [
				"let's see who comes out victorious in the next round!",
				"now for another round full of suspense!",
				"who will make the best of this round?"
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
				startLetter: "did not start with \"{letter}\"!",
				lastLetter: "does not contain previous word's \"{letter}\"!",
				alreadyPlayed: "we've seen that word before!",
				doesntExist: "is that english?",
				timeOut: "time is up!"
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
				"we have our game at computer {computer}, player {player}!",
				"the panel reads computer {computer} versus player {player}.",
				"computer at {computer}, player at {player}!"
			],
			gameEnd: "and it's settled! {winner} defeats {loser} {winnerPoints} to {loserPoints} in a match to remember! " +
				"we look forward to the next time these two titans have a face-off. " +
				"see you next time!"
		},
		computer: {
			key: {
				delay: {
					min: 20,
					max: 150
				}
			},
			word: {
				delay: {
					min: 200,
					max: 2500
				},
				findAttempts: 10
			}
		},
		letters: "abcdefghijklmnopqrstuvwxyz",
		trimLength: 100,
		time: {
			depletionRate: 0.05, // fraction per second
			depletionInterval: 100 // ms
		}
	};

	var signal = {
		commentFinished: new Signal(),
		allCommentsFinished: new Signal(),
		playerFinished: new Signal(),
		computerFinished: new Signal(),
		enterPressed: new Signal()
	};

	var mistake = {
		startLetter: "startLetter",
		lastLetter: "lastLetter",
		alreadyPlayed: "alreadyPlayed",
		doesntExist: "doesntExist",
		timeOut: "timeOut"
	};

	var point = {
		computer: 0,
		player: 0
	};
	var time = {
		computer: 1,
		player: 1
	};
	var counter = {
		computer: null,
		player: null
	};

	var container; // Element
	var title; // Element

	var words; // String
	var playedWords = ""; // String
	var wordsByLetter; // Object
	var lastLetter; // String
	var startLetter; // String
	var timerInterval;
	var ignoreKeys = false;

	cfg.time.depletionRate = cfg.time.depletionRate * (cfg.time.depletionInterval / 1000);

	window.addEvent("domready", onDOMReady);

	function onDOMReady(e) {
		container = $("game");
		title = $("title");

		counter.computer = new Bar(cfg, $$(".computer.time-bar")[0]);
		counter.player = new Bar(cfg, $$(".player.time-bar")[0]);

		// Events.
		window.addEvent(AGJ.event.key.down, onKeyDown);

		signal.playerFinished.add(onPlayerFinished);
		signal.computerFinished.add(onComputerFinished);

		// Get words.
		insertComment(cfg.comment.loading);
		ignoreKeysDuringComment();
		var request = new Request({ url: "data/words.txt" });
		request.addEvent("success", onWordsObtained);
		request.get();
	}

	/////

	function start() {
		signal.enterPressed.addOnce(start2);
		insertComment(cfg.comment.toStart);
	}

	function start2() {
		title.addClass("hidden");
		insertComment(cfg.comment.start);
		insertComment(cfg.comment.rules);
		signal.allCommentsFinished.addOnce(startRound);
	}

	function startRound() {
		trim();

		lastLetter = null;
		time.player = 1;
		time.computer = 1;
		updateCounters();

		startLetter = getRandomLetter();
		signal.allCommentsFinished.addOnce(startComputerInput);
		insertComment(cfg.comment.letterAnnounce.getRandom().replace("{letter}", startLetter));
	}

	function finishRound(playerWon, error) {
		var winner = playerWon ? "player" : "computer";
		var loser = playerWon ? "computer" : "player";
		if (playerWon) {
			point.player++;
		} else {
			point.computer++;
		}

		var mistakeComment = cfg.comment.mistake[error];
		if (error === mistake.startLetter)
			mistakeComment = mistakeComment.replace("{letter}", startLetter);
		if (error === mistake.lastLetter)
			mistakeComment = mistakeComment.replace("{letter}", lastLetter);
		insertComment(mistakeComment);

		if (point.player < cfg.winPoints && point.computer < cfg.winPoints) {
			// Round end, but game continues.
			var pointComment = cfg.comment.roundEnd.point.getRandom();
			pointComment = pointComment.replace("{winner}", winner);
			pointComment = pointComment.replace("{loser}", loser);
			insertComment(pointComment);
			insertComment(getScoreComment());
			if (point.player !== point.computer) {
				var winningComment = cfg.comment.roundEnd.winning.getRandom();
				winningComment = winningComment.replace("{winner}", winner);
				winningComment = winningComment.replace("{loser}", loser);
				insertComment(winningComment);
			} else {
				insertComment(cfg.comment.roundEnd.tie.getRandom().replace("{points}", point.player));
			}
			insertComment(cfg.comment.roundStart.getRandom());
			ignoreKeysDuringComment();
			startRound();

		} else {
			// Game end.
			var gameEndComment = cfg.comment.gameEnd;
			gameEndComment = gameEndComment.replace("{winner}", winner).replace("{loser}", loser);
			gameEndComment = gameEndComment.replace("{winnerPoints}", playerWon ? point.player : point.computer).replace("{loserPoints}", playerWon ? point.computer : point.player);
			ignoreKeys = true;
			insertComment(gameEndComment);

			signal.allCommentsFinished.addOnce(onGameEnd);
		}
	}

	function finish(element, error, word) {
		element.removeClass("active");
		if (!error) {
			element.addClass("right");
			if (word)
				lastLetter = word.substr(-1);
		} else {
			element.addClass("wrong");
			// lastLetter = null;
		}
		clearInterval(timerInterval);
	}

	function startInput() {
		timerInterval = setInterval(onTimer, cfg.time.depletionInterval);
		counter.player.setActive(false);
		counter.computer.setActive(false);
		setTimeout(fixWebkitBug, 0);
	}

	function updateCounters() {
		counter.player.setValue(time.player);
		counter.computer.setValue(time.computer);
	}

	function addInputLetter(element, wordSoFar, letter) { // String
		wordSoFar += letter;
		element.set("html", wordSoFar);
		return wordSoFar;
	}

	function addPlayedWord(thisWord) {
		trace("Adding played word:", thisWord, playedWords);
		playedWords += "\n" + thisWord;
	}

	function insertInput(classes) {
		var el = new Element("span", { "class": classes, html: "&nbsp;" } );
		container.appendText(" ");
		container.grab(el);
		return el;
	}

	function trim() {
		var nodes = container.childNodes;
		var count = 0;
		for (var i = nodes.length - 1; i >= 0; --i) {
			var node = $(nodes[i]);
			if (count < cfg.trimLength) {
				count += node.textContent.length;
			} else {
				container.removeChild(node);
			}
		}
	}

	function ignoreKeysDuringComment() {
		ignoreKeys = true;
		signal.commentFinished.addOnce(stopIgnoringKeys);
	}
	function stopIgnoringKeys() {
		ignoreKeys = false;
	}

	function getWordStartError(wordStart) { // String
		wordStart = wordStart.toLowerCase();
		if (wordStart.charAt(0) !== startLetter)
			return mistake.startLetter;
		var regex = new RegExp("^" + wordStart, "m");
		if (!regex.test(words))
			return mistake.doesntExist;
		return null;
	}

	function getWordError(word) { // String
		word = word.toLowerCase();
		if (word.charAt(0) !== startLetter) //lastLetter)
			return mistake.startLetter;
		if (lastLetter && word.indexOf(lastLetter) < 0)
			return mistake.lastLetter;
		var regex = new RegExp("^" + word + "$", "m");
		if (regex.test(playedWords))
			return mistake.alreadyPlayed;
		if (!regex.test(words))
			return mistake.doesntExist;
		return null;
	}

	function getRandomLetter() { // String
		return cfg.letters.charAt(Math.randomInt(cfg.letters.length));
	}

	function getRandomBetween(min, max) { // number
		return Math.randomInt(max - min) + min;
	}

	function fixWebkitBug() {
		// Webkit graphic bug workaround
		container.style.display = "none";
		container.offsetHeight;
		container.style.display = "block";
	}

	// Player input.

	var playerInput; // Element
	var playerWordSoFar = ""; // String

	function startPlayerInput() {
		startInput();
		playerInput = insertInput("input player active");
		playerWordSoFar = "";
		counter.player.setActive(true);
	}

	function addPlayerLetter(letter) {
		playerWordSoFar = addInputLetter(playerInput, playerWordSoFar, letter);
		var error = getWordStartError(playerWordSoFar);
		if (error) {
			finishPlayerInput(error);
		}
	}

	function checkPlayerInput() {
		var error = getWordError(playerWordSoFar);
		if (!error)
			addPlayedWord(playerWordSoFar);
		trace("Checking player input:", error);
		finishPlayerInput(error);
	}

	function finishPlayerInput(error) {
		finish(playerInput, error, playerWordSoFar);
		playerInput = null;
		counter.player.setActive(false);
		signal.playerFinished.dispatch(error);
	}

	// Computer input.

	var computerInput; // Element
	var computerWordSoFar = "";
	var computerWord; // String
	var computerIndex; // Number
	var computerTimeout;

	function startComputerInput() {
		startInput();
		computerInput = insertInput("input computer active");
		computerWordSoFar = "";
		computerWord = wordsByLetter[startLetter].getRandom();
		if (lastLetter) {
			for (var i = 0; i < cfg.computer.word.findAttempts && computerWord.indexOf(lastLetter) < 0; i++) {
				computerWord = wordsByLetter[startLetter].getRandom();
			}
		}
		computerIndex = 0;
		computerTimeout = setTimeout(onComputerTimeout, getRandomBetween(cfg.computer.word.delay.min, cfg.computer.word.delay.max));
		counter.computer.setActive(true);
	}

	function addComputerLetter(letter) {
		computerWordSoFar = addInputLetter(computerInput, computerWordSoFar, letter);
		var error = getWordStartError(computerWordSoFar);
		if (error) {
			finishComputerInput(error);
		}
	}

	function checkComputerInput() {
		var error = getWordError(computerWordSoFar);
		if (!error)
			addPlayedWord(computerWordSoFar);
		finishComputerInput(error);
	}

	function finishComputerInput(error) {
		finish(computerInput, error, computerWordSoFar);
		computerInput = null;
		clearTimeout(computerTimeout);
		counter.computer.setActive(false);
		signal.computerFinished.dispatch(error);
	}

	function getComputerDelay() { // Number
		return getRandomBetween(cfg.computer.key.delay.min, cfg.computer.key.delay.max);
	}

	function onComputerTimeout() {
		if (computerIndex <= computerWord.length) {
			if (AGJ.util.tossCoin(0.99)) {
				addComputerLetter(computerWord.charAt(computerIndex));
			} else {
				addComputerLetter(getRandomLetter());
			}
			computerIndex++;
			if (computerInput)
				computerTimeout = setTimeout(onComputerTimeout, getComputerDelay());
		} else {
			checkComputerInput();
		}
	}

	// Comments.

	var commentElement; // Element
	var comment; // String
	var commentIndex; // Number
	var commentQueue = [];

	var commentInterval;

	function insertComment(text) {
		if (!commentElement) {
			executeComment(text);
		} else {
			commentQueue.push(text);
		}
	}

	function executeComment(text) {
		clearInterval(commentInterval);
		comment = text;
		commentIndex = 0;
		commentElement = new Element("span", { "class": "comment" });
		container.appendText(" ");
		container.grab(commentElement);
		commentInterval = setInterval(onCommentInterval, cfg.comment.keyDelay);
	}

	function stopComment() {
		if (commentElement) {
			if (commentIndex < comment.length)
				commentElement.appendText("\u2014");
			commentElement = null;
			clearInterval(commentInterval);
			signal.commentFinished.dispatch();

			if (commentQueue.length > 0) {
				executeComment(commentQueue.shift());
			} else {
				signal.allCommentsFinished.dispatch();
			}
		}
	}

	function getScoreComment() { // String
		var text = cfg.comment.scoreTally.getRandom();
		text = text.replace("{player}", point.player);
		text = text.replace("{computer}", point.computer);
		return text;
	}

	function updateComment(index) {
		commentElement.set("text", comment.substr(0, index));
	}

	function onCommentInterval() {
		commentIndex++;
		updateComment(commentIndex);
		if (commentIndex > comment.length) {
			stopComment();
		}
	}


	///// Events.

	function onWordsObtained(contents, xml) {
		words = contents;

		wordsByLetter = {};
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
				wordsByLetter[letter] = group;
			} else {
				break;
			}
		}
		trace(wordsByLetter);

		start();
	}

	function onKeyDown(e) {
		if (e.meta || e.control || e.alt)
			return;
		e.preventDefault();
		if (ignoreKeys)
			return;
		var char = e.key;
		if (playerInput) {
			if (char === "enter") {
				if (playerWordSoFar.length > 0)
					checkPlayerInput();
			} else if (char.length === 1 && cfg.letters.indexOf(char) >= 0) {
				addPlayerLetter(char);
			}
		} else if (char === "enter") {
			if (commentElement)
				stopComment();
			signal.enterPressed.dispatch();
		}
	}

	function onTimer() {
		if (playerInput) {
			time.player = Math.max(time.player - cfg.time.depletionRate, 0);
			if (time.player <= 0)
				finishPlayerInput(mistake.timeOut);
		} else if (computerInput) {
			time.computer = Math.max(time.computer - cfg.time.depletionRate, 0);
			if (time.computer <= 0)
				finishComputerInput(mistake.timeOut);
		} else {
			clearInterval(timerInterval);
		}
		updateCounters();
	}

	function onPlayerFinished(error) {
		if (error) {
			finishRound(false, error);
		} else {
			signal.allCommentsFinished.addOnce(startComputerInput);
			insertComment(cfg.comment.interjection.getRandom());
		}
	}

	function onComputerFinished(error) {
		if (error) {
			finishRound(true, error);
		} else {
			signal.allCommentsFinished.addOnce(startPlayerInput);
			insertComment(cfg.comment.interjection.getRandom());
		}
	}

	function onGameEnd() {
		title.removeClass("hidden");
	}


	/////
	///// Classes.

	var Bar = Class.extend({
		init: function (config, element) {
			this._cfg = config;
			this._element = element;

			this._value = 1;
			this._update();
		},

		setValue: function (value) {
			this._value = value;
			this._update();
		},
		getValue: function () { // Number
			return this._value;
		},

		setActive: function (value) {
			this._active = value;
			this._update();
		},
		getActive: function (value) {
			return this._active;
		},

		_update: function () {
			this._element.getElements(".remaining")[0].setStyle("width", this._getPercent(this._value));
			if (this._active)
				this._element.addClass("active");
			else
				this._element.removeClass("active");
		},

		_getPercent: function (value) {
			return (value * 100) + "%";
		}
	});


})(this);

