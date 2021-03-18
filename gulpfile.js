const gulp = require("gulp");
const { promisify } = require("util");
const exec = promisify(require("child_process").exec);

const cfg = require("./source/js/config.js");

// Elm compilation

const doElm = (debug) =>
  exec(
    `npx elm make ` +
      `${cfg.elmDir}Main.elm ` +
      `--output="${cfg.outputDir}js/script.js" ` +
      (debug ? "--debug" : "")
  );

const buildElm = () => doElm(false);

const debugElm = () => doElm(true);

const developElm = () =>
  exec(
    `npx elm-live ` +
      `${cfg.elmDir}Main.elm ` +
      `--path-to-elm="./node_modules/.bin/elm" ` +
      `--dir="output/" ` +
      `--open ` +
      `--hot ` +
      `-- ` +
      `--output="${cfg.outputDir}js/script.js" ` //+
    //`--debug`
  );

const watchElm = () =>
  gulp.watch(`${cfg.elmDir}**/*.elm`, gulp.series(formatElm, debugElm));

// Static files copy

const copy = () =>
  gulp.src(`${cfg.copyDir}**`).pipe(gulp.dest(`${cfg.outputDir}`));

const watchCopy = () => gulp.watch(`${cfg.copyDir}**`, copy);

// Formatting

const formatElm = () => exec(`npx elm-format ${cfg.elmDir} --yes`);

const watchFormatElm = () => gulp.watch(`${cfg.elmDir}**/*.elm`, formatElm);

const formatOther = () => exec(`npx prettier . --write`);

const watchFormatOther = () =>
  gulp.watch(
    [
      "./*.(js|html|css|json|md|yaml)",
      "./source/**/*.(js|html|css|json|md|yaml)",
    ],
    formatOther
  );

// Combined tasks

const build = gulp.parallel(copy, buildElm);

const debug = gulp.parallel(copy, debugElm);

const watch = gulp.parallel(watchCopy, watchElm);

const format = gulp.parallel(formatElm, formatOther);

const watchFormat = gulp.parallel(watchFormatElm, watchFormatOther);

const develop = gulp.series(
  format,
  gulp.parallel(watchCopy, watchFormat, developElm)
);

module.exports = {
  default: build,
  develop,
  build,
  debug,
  watch,
  format,
};
