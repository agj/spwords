
define( function () {
	
	/**
	 * A simple way to check if an object has a set of matching properties.
	 * Use like so:
	 *    var myInterface = Interface.extend({ aProp: String, aMethod: Function });
	 *    var objectMatchesMyInterface = myInterface.matches(someObject);
	 *    var myExtendedInterface = myInterface.extend({ anotherProp: SomeClass });
	 *    var objectMatchesMyExtendedInterface = myExtendedInterface.matches(someObject);
	 */
	var Interface = {
		/**
		 * Extends this interface by adding new properties.
		 * @param  {object} properties An object whose properties' values are used to match the type of the matched object's properties.
		 * @return {object} The new interface.
		 */
		extend: function extend(properties) {
			if (this.properties) {
				for (var prop in this.properties) {
					if (!this.properties.hasOwnProperty(prop) || is.set(properties[prop]))
						continue;
					properties[prop] = this.properties[prop];
				}
			}
			return {
				properties: properties,
				extend: Interface.extend,
				matches: Interface.matches
			};
		},
		/**
		 * Returns whether this interface matches the passed object.
		 * @param  {object} object
		 * @return {boolean}
		 */
		matches: function (object) { // Boolean
			for (var prop in this.properties) {
				if (!this.properties.hasOwnProperty(prop))
					continue;
				if (!(object[prop] instanceof this.properties[prop]))
					return false;
			}
			return true;
		}
	};

	return Interface;

	
});
