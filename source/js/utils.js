const R = require("ramda");
require("dot-into").install();

const log = R.tap(console.log);
const prepend = R.curry((prep, text) => prep + text);
const toJson = (data) => JSON.stringify(data, null, "\t");

module.exports = {
  log,
  prepend,
  toJson,
};
