
define(["agj/core"], function (AGJ) {
	"use strict";

	var defineModule = AGJ.defineModule;
	var defineModules = AGJ.defineModules;

	var module = {};

	defineModules(module, {
		mouse: defineModules({}, {
			click: "click",
			doubleClick: "dblclick",
			down: "mousedown",
			up: "mouseup",
			move: "mousemove",
			over: "mouseover",
			out: "mouseout",
			enter: "mouseenter",
			leave: "mouseleave",
			wheel: "wheel",
			contextMenu: "contextmenu",
			contextMenuShow: "show"
		}),
		key: defineModules({}, {
			down: "keydown",
			up: "keyup",
			press: "keypress"
		}),
		composition: defineModules({}, {
			start: "compositionstart",
			update: "compositionupdate",
			end: "compositionend"
		}),
		ui: defineModules({}, {
			input: "input",
			change: "change",
			focus: "focus",
			blur: "blur",
			focusIn: "focusin",
			focusOut: "focusout",
			invalid: "invalid",
			reset: "reset",
			submit: "submit",
			select: "select",
			viewResize: "resize",
			scroll: "scroll",
			hashChange: "hashchange"
		}),
		motion: defineModules({}, {
			orientationChange: "orientationchange",
			gyroscope: "deviceorientation",
			accelerometer: "devicemotion"
		}),
		load: defineModules({}, {
			readyStateChange: "readystatechange",
			load: "load",
			unload: "unload",
			abort: "abort",
			error: "error"
		}),
		touch: defineModules({}, {
			start: "touchstart",
			end: "touchend",
			move: "touchmove",
			cancel: "touchcancel",
			enter: "touchenter",
			leave: "touchleave"
		}),
		mootools: defineModules({}, {
			domReady: "domready"
		})
	});

	return module;

});