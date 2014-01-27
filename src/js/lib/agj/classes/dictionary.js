

define(["./destroyable"], function (Destroyable) {

	return Destroyable.extend({
		init: function () {
			this._super("init")();

			this._keys = [];
			this._values = [];
		},

		set: function (key, value) {
			var index = this._keys.indexOf(key);
			if (index >= 0) {
				this._values[index] = value;
				return;
			}
			this._keys.push(key);
			this._values.push(value);
		},

		get: function (key) { // Object
			var index = this._keys.indexOf(key);
			if (index === -1)
				return null;
			return this._values[index];
		},

		remove: function (key) { // Boolean
			var index = this._keys.indexOf(key);
			if (index === -1)
				return false;
			this._keys.splice(index, 1);
			this._values.splice(index, 1);
			return true;
		},

		clear: function () {
			this._keys.splice(0, this._keys.length);
			this._values.splice(0, this._values.length);
		},

		getKeys: function () {
			return this._keys.concat();
		},

		getValues: function () {
			return this._values.concat();
		},

		destroy: function () {
			this._super("destroy")();
			this._keys = null;
			this._values = null;
		}
	});

});

