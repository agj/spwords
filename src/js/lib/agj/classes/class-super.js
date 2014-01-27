
define( function () {

	var module = {};

	// This method of creating classes is a modification of John Resig's: http://ejohn.org/blog/simple-javascript-inheritance/
	// My changes make inheritance behave more like actionscript 3, and allow for easy statics declaration.
	var classInitializing = false;
	var classUsesSuperTest = /xyz/.test(function () { "xyz"; }) ? /\b_super\b/ : null;

	module.extend = function (properties) {
		classInitializing = true;
		var prototype = new this();
		classInitializing = false;

		var _super = this.prototype;
		var superFn = function (name) {
			return getSuperFn(_super[name], this);
		};
		var getSuperFn = function (fn, scope) {
			return function () {
				fn.apply(scope, arguments);
			};
		};

		var wrapFunction = function (fn) {
			return function () {
				var originalSuper = this._super;
				this._super = superFn;
				var result = fn.apply(this, arguments);
				this._super = originalSuper;

				return result;
			};
		};

		for (var name in properties) {
			if (name === "statics")
				continue;

			if (typeof properties[name] === "function" && (!classUsesSuperTest || classUsesSuperTest.test(properties[name]))) {
				prototype[name] = wrapFunction(properties[name]);
			} else {
				prototype[name] = properties[name];
			}
		}

		function Class() {
			// All construction is actually done in the init method
			if (!classInitializing && this.init)
				this.init.apply(this, arguments);
		}

		if (properties.statics) {
			for (name in properties.statics) {
				if (properties.statics.hasOwnProperty(name))
					Class[name] = properties.statics[name];
			}
		}

		properties = null;

		Class.prototype = prototype;
		Class.prototype.constructor = Class;

		Class.extend = module.extend;
		return Class;
	};

	return module;

});
