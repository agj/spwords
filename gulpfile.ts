import gulp from "gulp";
import cfg from "./source/ts/config";
import { run } from "./source/ts/utils";

// Elm compilation

const doElm = (debug: boolean) =>
  run(`npx elm make ${cfg.elmDir}Main.elm`, {
    output: `${cfg.outputDir}js/script.js`,
    debug,
  });

const buildElm = () => doElm(false);

const debugElm = () => doElm(true);

const developElm = () =>
  run(
    `npx elm-go ${cfg.elmDir}Main.elm `,
    {
      "path-to-elm": "./node_modules/.bin/elm",
      dir: "output/",
      open: false,
      hot: true,
    },
    {
      output: `${cfg.outputDir}js/script.js`,
      debug: false,
    }
  );

const watchElm = () =>
  gulp.watch(`${cfg.elmDir}**/*.elm`, gulp.series(formatElm, debugElm));

// Static files copy

const copy = () =>
  gulp.src(`${cfg.copyDir}**`).pipe(gulp.dest(`${cfg.outputDir}`));

const watchCopy = () => gulp.watch(`${cfg.copyDir}**`, copy);

// Formatting

const formatElm = () => run(`npx elm-format ${cfg.elmDir}`, { yes: true });

const watchFormatElm = () => gulp.watch(`${cfg.elmDir}**/*.elm`, formatElm);

const formatOther = () => run("npx prettier .", { write: true });

const watchFormatOther = () =>
  gulp.watch(
    [
      "./*.(js|html|css|json|md|yaml)",
      "./source/**/*.(js|html|css|json|md|yaml)",
    ],
    formatOther
  );

// Combined tasks

export const build = gulp.parallel(copy, buildElm);

export const debug = gulp.parallel(copy, debugElm);

export const watch = gulp.parallel(watchCopy, watchElm);

export const format = gulp.parallel(formatElm, formatOther);

const watchFormat = gulp.parallel(watchFormatElm, watchFormatOther);

export const develop = gulp.series(
  format,
  gulp.parallel(watchCopy, watchFormat, developElm)
);

export default build;
