/**
 * 'Abstract' class meant for extending. Override 'destroy()' to clean up.
 * Remember to call 'this._super("destroy")()'.
 */
define(["./class-super"], function (Class) {
	return Class.extend({
		init: function () {
			this._isDestroyed = false;
		},

		destroy: function () {
			this._isDestroyed = true;
		},

		getIsDestroyed: function () { // Boolean
			return this._isDestroyed;
		}
	});
});

