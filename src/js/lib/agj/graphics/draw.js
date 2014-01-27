
/**
 * Utilities for drawing on a canvas object.
 *
 * Created by agj (www.agj.cl).
 */

define(["agj/agj", "agj/classes/class-super", "agj/classes/geom"], function (AGJ, Class, geom) {
	"use strict";

	// Imports.
	var Point = geom.Point;
	var Rectangle = geom.Rectangle;
	var defineModule = AGJ.defineModule;

	/////

	var graphics = {};

	var draw = defineModules(defineModule(graphics, "draw"), {
		line: function (context2D, drawStyle, fromX, fromY, toX, toY) {
			if ((fromX !== toX) || (fromY !== toY)) {
				context2D.save();
				context2D.beginPath();
				graphics.setLine(context2D, drawStyle);
				context2D.moveTo(fromX, fromY);
				context2D.lineTo(toX, toY);
				graphics.endLine(context2D);
				context2D.restore();
			} else {
				if (drawStyle.lineCapsStyle === "round") {
					draw.circle(context2D, drawStyle, fromX, fromY, drawStyle.lineWeight * 0.5);
				} else {
					var halfLT = drawStyle.lineWeight / 2;
					draw.rectangle(context2D, drawStyle, fromX - halfLT, fromY - halfLT, drawStyle.lineWeight, drawStyle.lineWeight);
				}
			}
		},

		curve: function (context2D, drawStyle, fromX, fromY, midX, midY, toX, toY) {
			if ((fromX !== toX) || (fromY !== toY)) {
				context2D.save();
				context2D.beginPath();
				graphics.setLine(context2D, drawStyle);
				context2D.moveTo(fromX, fromY);
				context2D.quadraticCurveTo(midX, midY, toX, toY);
				graphics.endLine(context2D);
				context2D.restore();
			} else {
				if (drawStyle.lineCapsStyle === "round") {
					draw.circle(context2D, drawStyle, fromX, fromY, drawStyle.lineWeight * 0.5);
				} else {
					var halfLT = drawStyle.lineWeight / 2;
					draw.rectangle(context2D, drawStyle, fromX - halfLT, fromY - halfLT, drawStyle.lineWeight, drawStyle.lineWeight);
				}
			}
		},

		circle: function (context2D, drawStyle, x, y, radius) {
			context2D.save();
			context2D.beginPath();
			graphics.setFill(context2D, drawStyle);
			context2D.arc(x, y, radius, 0, Math.PI * 2);
			graphics.endFill(context2D);
			context2D.restore();
		},

		rectangle: function (context2D, drawStyle, x, y, width, height) {
			context2D.save();
			context2D.beginPath();
			graphics.setFill(context2D, drawStyle);
			context2D.fillRect(x, y, width, height);
			graphics.endFill(context2D);
			context2D.restore();
		}
	});

	defineModules(graphics, {
		setLine: function (context2D, drawStyle) {
			if (drawStyle.getDefinesLine()) {
				context2D.lineWidth = drawStyle.lineWeight;
				context2D.strokeStyle = "#" + drawStyle.lineColor.toHex(6);
				context2D.lineCap = drawStyle.lineCapsStyle;
			}
		},
		setFill: function (context2D, drawStyle) {
			if (drawStyle.getDefinesFill()) {
				context2D.fillStyle = "#" + drawStyle.lineColor.toHex(6);
			}
		},
		endLine: function (context2D) {
			context2D.stroke();
		},
		endFill: function (context2D) {
			context2D.fill();
		}
	});


	/////

	var DrawStyle = defineModule(graphics, "DrawStyle", Class.extend({
		init: function (fillColor, fillAlpha, lineColor, lineWeight, lineAlpha) {
			this.setFillColor(fillColor).
				setFillAlpha(fillAlpha).
				setLineColor(lineColor).
				setLineWeight(lineWeight).
				setLineAlpha(lineAlpha).

				setLineCapsStyle(null).
				setLineJointStyle(null).
				setLineMiterLimit(null);
		},

		setFillColor: function (value) { // self
			this.fillColor = this._checkColor(value);
			return this;
		},

		setFillAlpha: function (value) { // self
			this.fillAlpha = this._checkFraction(value, 1);
			return this;
		},

		setLineColor: function (value) { // self
			this.lineColor = this._checkColor(value);
			return this;
		},

		setLineAlpha: function (value) { // self
			this.lineAlpha = this._checkFraction(value);
			return this;
		},

		setLineWeight: function (value) { // self
			if (!isNaN(value))
				this.lineWeight = Math.max(0, value);
			else
				this.lineWeight = 0;
			return this;
		},

		_lineCapsStyles: ["butt", "round", "square"],
		setLineCapsStyle: function (value) { // self
			if (this._lineCapsStyles.indexOf(value) >= 0)
				this.lineCapsStyle = value;
			else
				this.lineCapsStyle = this._lineCapsStyles[0];
			return this;
		},

		_lineJointStyles: ["round", "miter", "bevel"],
		setLineJointStyle: function (value) { // self
			if (this._lineJointStyles.indexOf(value) >= 0)
				this.lineJointStyle = value;
			else
				this.lineJointStyle = this._lineJointStyles[0];
			return this;
		},

		setLineMiterLimit: function (value) { // self
			if (!isNaN(value))
				this.lineMiterLimit = Math.max(0, value);
			else
				this.lineMiterLimit = 3;
			return this;
		},

		getDefinesLine: function () { // Boolean
			return (this.lineAlpha > 0 && this.lineWeight > 0);
		},

		getDefinesFill: function () { // Boolean
			return (this.fillAlpha > 0);
		},

		_checkColor: function (value) { // Number
			return value & 0xffffff;
		},
		_checkFraction: function (number, defaultValue) { // Number
			if (isNaN(number))
				return defaultValue;
			return Math.max(0, Math.min(1, number));
		},

		statics: {
			makeLineStyle: function (lineColor, lineWeight, lineAlpha, lineCapsStyle, lineJointStyle, lineMiterLimit) { // DrawStyle
				var ds = new DrawStyle(null, null, lineColor, lineWeight, lineAlpha);
				ds.setLineCapsStyle(lineCapsStyle).
					setLineJointStyle(lineJointStyle).
					setLineMiterLimit(lineMiterLimit);
				return ds;
			}
		}
	}));

	return module;

});

