
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

	defineModule(spwords, "View", Class.extend({
		init: function (config, model) {
			this._cfg = config;
			this._model = model;

			this._bars = {};
			this._cfg.ids.forEach(function (id) {
				this._bars[id] = new spwords.Bar($$("." + id + ".time-bar")[0]);
			}, this);
		},

		update: function () {
			var m = this._model;
			this._cfg.ids.forEach(function (id) {
				this._bars[id].setValue(m.contestants[id].time);
			}, this);
		}
	}));

	/////

	defineModule(spwords, "Model", Class.extend({
		init: function (config) {
			this._cfg = config;

			this.contestants = {};
			this.userSkipped = new Signal();

			this._cfg.ids.forEach(function (id) {
				this.contestants[id] = new spwords.ContestantData();
			}, this);

			this._localInput = new inputmanagers.LocalInputManager();

			this._localInput.finished.add(this._onSkipped.bind(this));
		},

		_onSkipped: function () {
			this.userSkipped.dispatch();
		}
	}));

	/////

	defineModule(spwords, "Controller", Class.extend({
		init: function (config, model, view) {
			this._cfg = config;
			this._model = model;
			this._view = view;

			model.userSkipped.add(this._onUserSkipped.bind(this));
		},

		_onUserSkipped: function () {
			
		}
	}));



	/////
	/////
	
	defineModule(spwords, "ContestantData", Class.extend({
		init: function () {
			this.time = 1;
			this.inputManager = null;
		}
	}));

	/////

	var inputmanagers = defineModule(spwords, "inputmanagers");
	
	var InputManager = defineModule(inputmanagers, "InputManager", Class.extend({
		init: function () {
			this.pressed = new Signal(); // self:InputManager, char:String
			this.finished = new Signal(); // self:InputManager
		},

		_fixChar: function (char) { // String
			var index = "áéíóúàèìòùâêîôûäëïöü".indexOf(char);
			if (index >= 0) {
				return  "aeiouaeiouaeiouaeiou".charAt(index);
			}
			return char;
		}
	}));

	defineModule(inputmanagers, "LocalInputManager", InputManager.extend({
		init: function () {
			this._super("init")();

			g.addEvent(AGJ.event.key.press, this._onKeyDown.bind(this));
		},

		_onKeyDown: function (e) {
			if (e.meta || e.control || e.alt)
				return;
			var char = e.key;
			if (char.length === 1) {
				char = this._fixChar(e.key);
				this.pressed.dispatch(this, char);
			} else if (char === "enter") {
				this.finished.dispatch(this);
			}
		}
	}));

	defineModule(inputmanagers, "RemoteInputManager", InputManager.extend({
		init: function (socket) {
			this._super("init")();

			this._socket = socket;

			this._socket.characterReceived.add(this._onCharacter.bind(this));
		},

		_onCharacter: function (character) {
			var char = char.length > 1 ? char : this._fixChar(char);
			this.pressed.dispatch(this, char);
		}
	}));


})(this);
