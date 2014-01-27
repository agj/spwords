
/**
 * Created by agj (www.agj.cl).
 */

define([], function () {
	"use strict";

	var defineModule = function (obj, stringName, value) {
		var i, len, module, setValue, canDefineProperties;
		var modules = stringName.split(".");

		if (!obj)
			throw new TypeError("Invalid object passed.");

		canDefineProperties = "defineProperty" in Object && "defineProperties" in Object;

		for (i = 0, len = modules.length; i < len; i++) {
			module = modules[i];
			if (!(module in obj)) {
				setValue = typeof value !== "undefined" && i === len - 1;
				if (canDefineProperties) {
					Object.defineProperty(obj, module, {
						writable: false, enumerable: true, configurable: false,
						value: (setValue) ? value : {}
					} );
				} else {
					obj[module] = (setValue) ? value : {};
				}
			}
			obj = obj[module];
		}

		return typeof value !== "undefined" ? value : obj;
	};

	var defineModules = function (obj, objectProperties) {
		for (var prop in objectProperties) {
			if (!objectProperties.hasOwnProperty(prop))
				continue;
			defineModule(obj, prop, objectProperties[prop]);
		}
		return obj;
	};


	// GENERAL

	var AGJ = defineModules({}, {
		defineModule: defineModule,
		defineModules: defineModules,

		trace: function () {
			if (AGJ.loggingIsEnabled && console && console.log)
				console.log.apply(console, Array.prototype.slice.call(arguments));
		},
		warn: function () {
			if (AGJ.loggingIsEnabled && console) {
				if (console.warn)
					console.warn.apply(console, Array.prototype.slice.call(arguments));
				else if (console.log)
					console.log.apply(console, Array.prototype.slice.call(arguments));
			}
		},

		mixin: function (objectTarget, objectProperties) {
			var canDefineProperties = "defineProperty" in Object && "defineProperties" in Object;

			for (var prop in objectProperties) {
				if (!(prop in objectTarget) || is.undefined(objectTarget[prop])) {
					if (canDefineProperties) {
						Object.defineProperty(objectTarget, prop, {
							writable: true, enumerable: false, configurable: true,
							value: objectProperties[prop]
						});
					} else {
						objectTarget[prop] = objectProperties[prop];
					}
				} else {
					var target = objectTarget.constructor ? objectTarget.constructor : objectTarget;
					target = target.name ? target.name : typeof target;
					AGJ.warn("agj: Could not mixin '" + prop + "' to '" + target + "' because it is already defined.");
				}
			}
		},

		destroy: function (arrayOfDestroyables) {
			for (var i = 0, len = arrayOfDestroyables.length; i < len; i++) {
				var current = arrayOfDestroyables[i];
				if (!current)
					continue;
				if (is.array(current))
					AGJ.destroy(current);
				else if (current.destroy)
					current.destroy();
				else if (current.removeAll)
					current.removeAll();
			}
		},

		getCallback: function (fun, scope, arrayArgs) { // Function
			return function () {
				var args = arrayArgs ? arrayArgs.concat(Array.prototype.slice(arguments, 0)) : arguments;
				fun.apply(scope, args);
			};
		}
	});

	AGJ.loggingIsEnabled = true;


	// UTIL

	defineModules(defineModule(AGJ, "util"), {
		tossCoin: function (probability) { // Boolean
			if (isNaN(probability))
				probability = 0.5;
			return Math.random() < probability;
		},
		toArray: function (object) { // Array
			return Array.prototype.slice.call(object);
		}
	});


	// IS

	var is = defineModules(defineModule(AGJ, "is"), {
		/**
		 * Returns whether the object is neither null, undefined, or NaN.
		 */
		set: function (object) { // Boolean
			return typeof object !== "undefined" && object !== null && (typeof object !== "number" || !isNaN(object));
		},

		undefined: function (object) { // Boolean
			return typeof object === "undefined";
		},

		boolean: function (object) { // Boolean
			return typeof object === "boolean";
		},

		number: function (object) { // Boolean
			return typeof object === "number";
		},

		string: function (object) { // Boolean
			return typeof object === "string";
		},

		array: function (object) { // Boolean
			return typeof object === "array";
		},

		fn: function (object) { // Boolean
			return typeof object === "function";
		}
	});


	return AGJ;
});

