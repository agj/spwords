import gulp from "gulp";
import del from "del";
import cfg from "./source/ts/config";
import { run } from "./source/ts/utils";

// Elm compilation

const doElm = (debug: boolean) =>
  run(`npx elm make ${cfg.elmDir}Main.elm`, {
    output: `${cfg.outputDir}js/${cfg.elmFileName}.js`,
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
      output: `${cfg.outputDir}js/${cfg.elmFileName}.js`,
      debug: false,
    }
  );

// Static files copy

const copy = () =>
  gulp.src(`${cfg.copyDir}**`).pipe(gulp.dest(`${cfg.outputDir}`));

const watchCopy = () => gulp.watch(`${cfg.copyDir}**`, copy);

// Elm formatting

const runElmFormat = (path: string) =>
  run(`npx elm-format "${path}"`, { yes: true });

const formatElm = () => runElmFormat(cfg.elmDir);

const watchFormatElm = () => {
  const watcher = gulp.watch(`${cfg.elmDir}**/*.elm`);
  watcher.on("change", (path) => {
    runElmFormat(path);
  });
  return watcher;
};

// Other file formatting

const runPrettier = (path: string) =>
  run(`npx prettier ${path}`, { write: true });

const formatOther = () => runPrettier(".");

const watchFormatOther = () => {
  const watcher = gulp.watch([
    "./*.(js|ts|html|css|json|md|yaml)",
    "./source/**/*.(js|ts|html|css|json|md|yaml)",
  ]);
  watcher.on("change", (path) => {
    runPrettier(path);
  });
  return watcher;
};

// Maintenance

const cleanUp = () => del(`${cfg.outputDir}**`);

// Combined tasks

export const build = gulp.series(cleanUp, gulp.parallel(copy, buildElm));

export const debug = gulp.series(cleanUp, gulp.parallel(copy, debugElm));

export const format = gulp.parallel(formatElm, formatOther);

const watchFormat = gulp.parallel(watchFormatElm, watchFormatOther);

export const develop = gulp.series(
  cleanUp,
  format,
  gulp.parallel(watchCopy, watchFormat, developElm)
);

export default build;
