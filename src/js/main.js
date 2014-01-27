
require.config({
	baseUrl: "js/lib",
	shim: {
		angular: {
			exports: "angular"
		}
	}
});

require(["jquery", "angular", "agj/core", "signals", "../lines" ,"../config", "agj/utils/event-constants"], function ($, angular, AGJ, signals, lines, config, events) {
	"use strict";

	var trace = AGJ.trace;
	var is = AGJ.is;

	//////////
	
	var spwords = angular.module("spwords", [], function ($locationProvider) {

	});

	spwords.value("GameState", {
		language: "en",
		isPlaying: false,
		mode: "single",
		contestant: {
			one: {
				time: 1
			},
			two: {
				time: 1
			}
		}
	});

	spwords.factory("Requester", function () {
		return {
			getDictionary: function (language) {
				return $http.get("data/words-" + language + ".txt");
			}
		};
	});

	spwords.directive("spwDisplay", function ($document) {
		var link = function (scope, element, attrs) {

			$document.on(events.key.press, function (e) {
				if (e.meta || e.control || e.alt)
					return;
				e.preventDefault();
				if (e.which === 13) { // Enter
					scope.enter();
				} else if (e.which) {
					var letter = String.fromCharCode(e.which);
					scope.addLetter(letter);
				}
			});

		};

		return {
			link: link
		};
	});

	//////////

	spwords.controller("AppCtrl", function ($scope, GameState) {
		$scope.game = GameState;
	});

	spwords.controller("TitleCtrl", function ($scope, GameState) {
		$scope.game = GameState;
	});

	spwords.controller("DisplayCtrl", function ($scope, GameState) {
		$scope.game = GameState;

		$scope.addLetter = function (player, letter) {

		};
		$scope.enter = function (player) {

		};

	});

	angular.element(document).ready( function () {
		angular.bootstrap(angular.element("body"), ["spwords"]);

		var game = $("#game");
		$.get("data/words-en.txt").then( function (list) {
			
		});
	});

	//////////
	
	// $( function () {

	// 	var game = $("#game");
	// 	$.get("data/words-en.txt").then( function (a) {
	// 		trace(a);
	// 	});

	// });


});

