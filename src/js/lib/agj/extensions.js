/**
 * Warning! This code modifies the prototypes of certain global objects.
 * Some are just polyfills, others are extensions.
 */

define(["../agj"], function (AGJ) {
	"use strict";

	var module = {};

	// OBJECT

	defineModules(defineModule(module, "object"), {
		getKeyFromValue: function (obj, value) { // String
			for (var key in obj) {
				if (!obj.hasOwnProperty(key))
					continue;
				if (obj[key] === value) {
					return key;
				}
			}
			return null;
		},

		isEmpty: function (obj) { // Boolean
			for (var key in obj) {
				if (!obj.hasOwnProperty(key))
					continue;
				return false;
			}
			return true;
		}
	});

	AGJ.mixin(Object, {
		keys: function (obj) { // Array
			var result = [];
			for (var key in obj) {
				if (!obj.hasOwnProperty(key))
					continue;
				result.push(key);
			}
			return result;
		}
	});


	// FUNCTION

	AGJ.mixin(Function.prototype, {
		bind: function (scope) { // Function
			if (!is.fn(this))
				throw new TypeError("What is trying to be bound is not callable.");
			var fn = this;
			var args = Array.prototype.slice.call(arguments, 1);
			return function () {
				var joinedArgs = args.concat(Array.prototype.slice.call(arguments));
				fn.apply(scope, joinedArgs);
			};
		}
	});


	// NUMBER

	AGJ.mixin(Number.prototype, {
		degToRad: function () { // Number
			return this * Math.TAU / 360;
		},
		radToDeg: function () { // Number
			return this * 360 / Math.TAU;
		},

		toBase: function (base, pad) { // String
			var result = this.toString(base);
			if (!isNaN(pad)) {
				while (result.length < pad) {
					result = "0" + result;
				}
			}
			return result;
		},

		toHex: function (pad) { // String
			return this.toBase(16, pad);
		},

		logBase: function (base) {
			return Math.log(this) / Math.log(base);
		}
	});


	// MATH

	defineModules(defineModule(module, "math"), {
		curve: function (numberValue, numberGentleness, numberPeak, numberValueSubtract) { // Number
			if (numberValueSubtract)
				numberValue = Math.max(numberValue - numberValueSubtract, 0);
			if (numberGentleness < 1)
				numberGentleness = 1;
			return numberPeak / ((numberValue / numberGentleness) + 1);
		}
	});

	AGJ.mixin(Math, {
		TAU: Math.PI * 2,

		randomInt: function (numberMaxValue) { // Number
			return Math.floor(Math.random() * numberMaxValue);
		},

		cartesianToRadians: function (x, y) { // Number
			return Math.atan2(y, x);
		}
	});


	// STRING

	AGJ.mixin(String.prototype, {
		startsWith: function (stringStart, caseSensitive) { // Boolean
			var string = this;
			if (caseSensitive !== true) {
				string = string.toLowerCase();
				stringStart = stringStart.toLowerCase();
			}
			return string.substr(0, stringStart.length) === stringStart;
		},
		endsWith: function (stringEnd, caseSensitive) { // Boolean
			var string = this;
			if (caseSensitive !== true) {
				string = string.toLowerCase();
				stringEnd = stringEnd.toLowerCase();
			}
			return string.substr(-stringEnd.length) === stringEnd;
		}
	});


	// ARRAY

	AGJ.mixin(Array.prototype, {
		clone: function () { // Array
			return this.concat();
		},

		getLast: function (amount) { // *|Array
			if (isNaN(amount)) {
				var index = this.length - 1;
				return index >= 0 ? this[index] : undefined;
			} else {
				var startIndex = Math.max(this.length - amount, 0);
				return this.slice(startIndex);
			}
		},
		getFirst: function (amount) { // *|Array
			if (isNaN(amount)) {
				return this[0];
			} else {
				var endIndex = Math.min(amount, this.length - 1);
				return this.slice(0, endIndex);
			}
		},

		getRandom: function () { // *
			var len = this.length;
			if (len <= 0)
				return null;
			return this[Math.floor(Math.random() * len)];
		},

		getRandomIndexExcept: function (arrayIndexExceptions) { // Number
			var exc, i, j, nums, random, total;

			exc = arrayIndexExceptions.concat();
			exc.sort();
			nums = [];

			total = this.length;
			for (j = 0; j < total; j++) {
				nums.push(j);
			}
			for (i = exc.length - 1; i >= 0; i--) {
				nums.splice(exc[i], 1);
			}

			random = Math.randomInt(nums.length);
			return nums[random];
		},

		overlaps: function (array) { // Boolean
			var len = this.length;
			for (var i = 0; i < len; i++) {
				if (array.indexOf(this[i]) >= 0)
					return true;
			}
			return false;
		},

		getDifference: function (array) { // Array
			return this.filter(function (item) {
				return array.indexOf(item) < 0;
			});
		},

		subtract: function (array) { // self
			for (var i = this.length - 1; i >= 0; i--) {
				if (array.indexOf(this[i]) >= 0)
					this.splice(i, 1);
			}
			return this;
		},

		getIntersection: function (array) { // Array
			return this.filter(function (item) {
				return array.indexOf(item) >= 0;
			});
		},

		remove: function () { // self
			var index;
			for (var i = 0, len = arguments.length; i < len; i++) {
				var item = arguments[i];
				while (true) {
					index = this.indexOf(item);
					if (index >= 0)
						this.splice(index, 1);
					else
						break;
				}
			}
			return this;
		},

		shuffle: function () { // self
			for (var i = this.length - 1; i >= 0; i--) {
				var temp = this[i];
				var r = Math.randomInt(i + 1);
				this[i] = this[r];
				this[r] = temp;
			}
			return this;
		},

		getNextTo: function (item, dontWrapAround) { // *
			var index = module.array.getNextOrPrevIndex(this, this.indexOf(item), true, dontWrapAround);
			if (index >= 0)
				return this[index];
			return null;
		},
		getPrevTo: function (item, dontWrapAround) { // *
			var index = module.array.getNextOrPrevIndex(this, this.indexOf(item), false, dontWrapAround);
			if (index >= 0)
				return this[index];
			return null;
		},
		getNextIndex: function (index, dontWrapAround) { // Number
			return module.array.getNextOrPrevIndex(this, index, true, dontWrapAround);
		},
		getPrevIndex: function (index, dontWrapAround) { // Number
			return module.array.getNextOrPrevIndex(this, index, false, dontWrapAround);
		},
		getNextIndexTo: function (item, dontWrapAround) { // Number
			return module.array.getNextOrPrevIndex(this, this.indexOf(item), true, dontWrapAround);
		},
		getPrevIndexTo: function (item, dontWrapAround) { // Number
			return module.array.getNextOrPrevIndex(this, this.indexOf(item), false, dontWrapAround);
		},

		get2D: function (width, x, y) { // *
			return this[x + y * width];
		},
		set2D: function (width, x, y, value) {
			this[x + y * width] = value;
		}
	});

	defineModule(module, "array.getNextOrPrevIndex", function (array, index, getNext, dontWrapAround) { // Number
		index += getNext ? 1 : -1;
		var len = array.length;
		if (index >= len) {
			if (!dontWrapAround)
				index = index % len;
			else
				index = -1;
		} else if (index < 0) {
			if (!dontWrapAround)
				index += len;
			else
				index = -1;
		}
		return index;
	});

	// ARRAY.SORT

	defineModule(module, "array.sort.descending", function (a, b) {
		return b - a;
	});


	// DATE

	AGJ.mixin(Date, {
		now: function () {
			return +(new Date());
		}
	});


	return module;

});

