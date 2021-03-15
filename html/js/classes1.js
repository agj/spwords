
/*jshint mootools:true*/
/*global signals:false*/
/*global AGJ:false*/
;(function (g) {
	"use strict";

	// Imports.
	var trace = AGJ.trace;
	var defineModule = AGJ.defineModule;
	var is = AGJ.is;
	var Class = AGJ.Class;
	var Destroyable = AGJ.Destroyable;
	var Signal = signals.Signal;

	var spwords = defineModule(AGJ, "projects.spwords");

	/////

	var mistake = defineModule(spwords, "mistake", {
		startLetter: "startLetter",
		includeLetter: "includeLetter",
		alreadyPlayed: "alreadyPlayed",
		doesntExist: "doesntExist",
		timeOut: "timeOut"
	});

	var Contestant = defineModule(spwords, "Contestant", Destroyable.extend({
		init: function (config, bar, id) {
			this._super("init")();

			this._cfg = config;
			this._bar = bar;
			this._id = id;

			this.finished = new Signal(); // self:Contestant, word:String, error:String

			this._time = 1;
			this._timerInterval = null;
			this._timerIntervalCallback = this._onTimer.bind(this);
			this._element = null;
			this._wordSoFar = null;
			this._startLetter = null;
			this._includeLetter = null;
		},

		startInput: function (startLetter, includeLetter) {
			this._startLetter = startLetter;
			this._includeLetter = includeLetter;
			clearInterval(this._timerInterval);
			this._timerInterval = setInterval(this._timerIntervalCallback, this._cfg.time.depletionInterval);
			this._insertElement(this._cfg.container);
			this._wordSoFar = "";
			this._bar.setActive(true);
		},

		reset: function () {
			this._time = 1;
			clearInterval(this._timerInterval);
			this._bar.setValue(this._time);
			this._bar.setActive(false);
		},

		getFinished: function () { // Signal
			return this._finished;
		},

		_addLetter: function (letter) {
			this._wordSoFar += letter;
			this._element.set("html", this._wordSoFar);
			var error = this._getWordStartError(this._wordSoFar);
			if (error) {
				this._finishInput(error);
			}
		},

		_checkInput: function () {
			var error = this._getWordError(this._wordSoFar);
			this._finishInput(error);
		},

		_finishInput: function (error) {
			this._element.removeClass("active");
			this._element.addClass(error ? "wrong" : "right");
			clearInterval(this._timerInterval);
			this._element = null;
			this._bar.setActive(false);
			this.finished.dispatch(this, this._wordSoFar, error);
		},

		_insertElement: function (container) {
			var el = new Element("span", { "class": "input active " + this._id, html: "&nbsp;" } );
			container.appendText(" ");
			container.grab(el);
			this._element = el;
		},

		_getWordStartError: function (wordStart) { // String
			wordStart = wordStart.toLowerCase();
			if (wordStart.charAt(0) !== this._startLetter)
				return mistake.startLetter;
			var regex = new RegExp("^" + wordStart, "m");
			if (!regex.test(this._cfg.words))
				return mistake.doesntExist;
			return null;
		},
		_getWordError: function (word) { // String
			word = word.toLowerCase();
			var regex = new RegExp("^" + word + "$", "m");
			if (!regex.test(this._cfg.words))
				return mistake.doesntExist;
			if (word.charAt(0) !== this._startLetter)
				return mistake.startLetter;
			if (this._includeLetter && word.indexOf(this._includeLetter) < 0)
				return mistake.includeLetter;
			if (regex.test(this._cfg.playedWords))
				return mistake.alreadyPlayed;
			return null;
		},

		_onTimer: function () {
			if (this._element) {
				this._time = Math.max(this._time - this._cfg.time.depletionRate, 0);
				this._bar.setValue(this._time);
				if (this._time <= 0)
					this._finishInput(mistake.timeOut);
			} else {
				clearInterval(this._timerInterval);
			}
		},

		destroy: function () {
			this._super("destroy")();
			clearInterval(this._timerInterval);
			this._finished.removeAll();

			this._cfg = null;
			this._bar = null;
			this._element = null;
			this._finished = null;
		}
	}));

	var Player = defineModule(spwords, "Player", Contestant.extend({
		init: function (config, bar, name) {
			this._super("init")(config, bar, name);

			this._onKeyDownCB = this._onKeyDown.bind(this);

			g.addEvent(AGJ.event.key.press, this._onKeyDownCB);
		},

		_fixChar: function (char) { // String
			var index = "áéíóúàèìòùâêîôûäëïöü".indexOf(char);
			if (index >= 0) {
				return  "aeiouaeiouaeiouaeiou".charAt(index);
			}
			return char;
		},

		_onKeyDown: function (e) {
			if (e.meta || e.control || e.alt)
				return;
			if (this._element) {
				var char = this._fixChar(e.key);
				if (char === "enter") {
					if (this._wordSoFar.length > 0)
						this._checkInput();
				} else if (char.length === 1 && this._cfg.letters.indexOf(char) >= 0) {
					this._addLetter(char);
				}
			}
		},

		destroy: function () {
			this._super("destroy")();
			g.removeEvent(AGJ.event.key.press, this._onKeyDownCB);
		}
	}));

	var Computer = defineModule(spwords, "Computer", Contestant.extend({
		init: function (config, bar, name) {
			this._super("init")(config, bar, name);

			this._word = null;
			this._wordIndex = NaN;
			this._keyTimeout = null;
			this._keyTimeoutCallback = this._onKeyTimeout.bind(this);
		},

		startInput: function (startLetter, includeLetter) {
			this._super("startInput")(startLetter, includeLetter);

			this._word = this._cfg.wordsByLetter[startLetter].getRandom();
			if (includeLetter) {
				for (var i = 0; i < this._cfg.computer.word.findAttempts && this._word.indexOf(includeLetter) < 0; i++) {
					this._word = this._cfg.wordsByLetter[startLetter].getRandom();
				}
			}
			this._wordIndex = 0;
			var delay = this._cfg.computer.word.delay;
			this._keyTimeout = setTimeout(this._keyTimeoutCallback, Math.randomInt(delay.max - delay.min) + delay.min);
		},

		_finishInput: function (error) {
			this._super("_finishInput")(error);
			clearTimeout(this._keyTimeout);
		},

		_onKeyTimeout: function () {
			while (this._wordIndex < this._word.length && this._cfg.letters.indexOf(this._word.charAt(this._wordIndex)) < 0) {
				this._wordIndex++;
			}

			if (this._wordIndex <= this._word.length) {
				if (!AGJ.util.tossCoin(this._cfg.computer.key.errorProbability)) {
					this._addLetter(this._word.charAt(this._wordIndex));
				} else {
					this._addLetter(this._cfg.letters.charAt(Math.randomInt(this._cfg.letters.length)));
				}
				this._wordIndex++;
				if (this._element) {
					var delay = this._cfg.computer.key.delay;
					this._keyTimeout = setTimeout(this._keyTimeoutCallback, Math.randomInt(delay.max - delay.min) + delay.min);
				}
			} else {
				this._checkInput();
			}
		},

		destroy: function () {
			this._super("destroy")();
			clearTimeout(this._keyTimeout);
		}
	}));

	var NetworkPlayer = defineModule(spwords, "NetworkPlayer", Contestant.extend({
		init: function (config, bar, name, socket) {
			this._super("init")(config, bar, name);

			this._socket = socket;
		},

		startInput: function (startLetter, includeLetter) {
			this._super("startInput")(startLetter, includeLetter);
		},

		_finishInput: function () {
			// Do nothing. This is to prevent timeouts automatically ending turns. We want to wait for network confirmation.
		}
	}));


	/////

	var Bar = defineModule(spwords, "Bar", Class.extend({
		init: function (element) {
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
	}));

	/////

	var Commentator = defineModule(spwords, "Commentator", Class.extend({
		init: function (config, socket) {
			this._cfg = config;
			this._socket = socket;

			this.online = false;

			this.finished = new Signal();
			this.allFinished = new Signal();
			this.interrupted = new Signal();

			this._element = null;
			this._queue = [];
			this._interval = null;
			this._intervalCallback = this._onInterval.bind(this);
			this._current = null;
			this._index = NaN;
			this._ignoreKeys = false;

			g.addEvent(AGJ.event.key.down, this._onKeyDown.bind(this));
		},

		comment: function (text, ignoreKeys, dontSendOnline) {
			trace("Commenting.", this.online, dontSendOnline);
			if (!this.online || dontSendOnline) {
				if (!this._element) {
					this._executeComment(text, ignoreKeys);
				} else {
					this._queue.push( { text: text, ignoreKeys: ignoreKeys } );
				}
			} else {
				this._socket.sendComments( { text: text, ignoreKeys: ignoreKeys } );
			}
		},

		interrupt: function (allComments) {
			if (allComments)
				this._queue = [];

			clearInterval(this._interval);

			if (this._element) {
				if (this._index < this._current.length)
					this._element.appendText("\u2014");
				this._element = null;
				this.finished.dispatch();

				if (this._queue.length > 0) {
					var cmt = this._queue.shift();
					this._executeComment(cmt.text, cmt.ignoreKeys);
				} else {
					this.allFinished.dispatch();
				}
			}
		},

		getIsCommenting: function () { // Boolean
			return !!this._element;
		},

		_executeComment: function (text, ignoreKeys) {
			this._ignoreKeys = ignoreKeys;
			clearInterval(this._interval);
			this._current = text;
			this._index = 0;
			this._element = new Element("span", { "class": "comment" });
			this._cfg.container.appendText(" ");
			this._cfg.container.grab(this._element);
			this._interval = setInterval(this._intervalCallback, this._cfg.comment.keyDelay);
		},

		_updateComment: function (index) {
			var newText = this._current.substr(0, index);
			if (newText.substr(-1) === " ") {
				newText = newText.substr(0, newText.length - 1) + "\xA0";
			}
			this._element.set("text", newText);
		},

		_onInterval: function () {
			this._index++;
			this._updateComment(this._index);
			if (this._index > this._current.length) {
				this.interrupt();
			}
		},

		_onKeyDown: function (e) {
			if (e.meta || e.control || e.alt)
				return;
			if (this._ignoreKeys)
				return;
			if (this._element && this._index > 0 && e.key === "enter") {
				this.interrupt();
				this.interrupted.dispatch();
			}
		}
	}));

	/////

	defineModule(spwords, "Options", Class.extend({
		init: function (container) {
			this._container = container;

			this.selected = new Signal(); // type:String, name:String

			var options = container.getElements(".option");
			options.addEvent(AGJ.event.mouse.click, this._onOptionClicked.bind(this));
		},

		_onOptionClicked: function (e) {
			var regexResult = /(\w+)-(\w+)-option/.exec(e.target.get("class"));
			var type = regexResult[2];
			var name = regexResult[1];
			this.selected.dispatch(type, name);
		}
	}));

	/////

	defineModule(spwords, "Socket", Destroyable.extend({
		init: function (config) {
			this._super("init")();

			trace("Creating Socket object.");

			this.idObtained = new Signal();
			this.commentsArrived = new Signal(); // comments:Array
			this.gotReady = new Signal();
			this.characterReceived = new Signal(); // char:String

			this._cfg = config;
			this._socket = g.io.connect("http://lmn2.us.to:3742");
			this._roomID = null;
			this._lang = null;
			this._joinedRoom = null;
			this._localID = null;

			this._socket.on("connect", this._onConnected.bind(this));
			this._socket.on("comment", this._onComments.bind(this));
			this._socket.on("player number", this._onLocalID.bind(this));
			this._socket.on("players", this._onPlayers.bind(this));
			this._socket.on("char", this._onCharacter.bind(this));
		},

		/**
		 * @param  {array} arrayComments List of comment objects: { text: "comment", ignoreKeys: false }
		 */
		sendComments: function (arrayComments) {
			this._socket.emit("comment", arrayComments);
		},

		requestID: function () {
			this._socket.emit("get random id", {}, (function (id) {
				this._roomID = id;
				this._checkEnterRoom();
				this.idObtained.dispatch();
			}).bind(this));
		},

		getID: function () { // String
			return this._roomID;
		},
		setID: function (id) {
			this._roomID = id;
			this._checkEnterRoom();
		},

		setLanguage: function (lang) {
			this._language = lang;
			this._checkEnterRoom();
		},

		_checkEnterRoom: function () {
			var room = this._language + this._roomID;
			if (is.set(this._id) && is.set(this._language) && this._joinedRoom !== room) {
				this._joinedRoom = room;
				this._socket.emit("set room", { room: room });
			}
		},

		_onConnected: function () {
			trace("Connected.");
		},

		_onComments: function (comments) {
			trace("Comments arrived.", comments);
			this.commentsArrived.dispatch(comments);
		},

		_onLocalID: function (id) {
			trace("Local ID:", id);
			this._localID = id;
		},

		_onPlayers: function (players) {
			trace("On players.", players);
			if (players.length === 2)
				this.gotReady.dispatch();
		},

		_onCharacter: function (data) {
			if (data.player_num !== this._playerID)
				this.characterReceived.dispatch(data.char);
		},

		destroy: function () {
			this._super("destroy")();
			this._socket.disconnect();
			AGJ.destroy([
				this.idObtained,
				this.commentsArrived,
				this.gotReady
			]);
			this._socket = null;
			this.idObtained = null;
			this.commentsArrived = null;
			this.gotReady = null;
		}
	}));

})(this);
