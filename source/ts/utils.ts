import R from "ramda";
import("dot-into").then((di) => di.install());

const log = R.tap(console.log);
const prepend = R.curry((prep, text) => prep + text);
const toJson = (data) => JSON.stringify(data, null, "\t");

export { log, prepend, toJson };
