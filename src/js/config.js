
define(function (require) {
	"use strict";
	
	var AGJ = require("agj/core");

	return AGJ.defineModules({}, {
		winPoints: 3,
		trimLength: 100,
		time: {
			depletionRate: 0.05, // fraction per second
			depletionInterval: 100 // ms
		},
		computer: {
			key: {
				delay: {
					min: 20,
					max: 150
				},
				errorProbability: 0.005
			},
			word: {
				delay: {
					min: 200,
					max: 2500
				},
				findAttempts: 20
			}
		},
		comment: {
			keyDelay: 80
		}
	});

});

