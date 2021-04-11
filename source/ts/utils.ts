import { tap, curry } from "ramda";
import { promisify } from "util";
import childProcess from "child_process";
const exec = promisify(childProcess.exec);

export const log = tap(console.log);

export const prepend = curry((prep, text) => prep + text);

export const toJson = (data: any) => JSON.stringify(data, null, "\t");

export const run = (
  program: string,
  options: Record<string, string | boolean> = {},
  furtherOptions?: Record<string, string | boolean>
) => {
  const end = furtherOptions ? " -- " + optionsToString(furtherOptions) : "";
  return exec(program + " " + optionsToString(options) + end);
};

// Internal

const optionsToString = (options: Record<string, string | boolean>): string =>
  Object.keys(options)
    .map((opt) => {
      const value = options[opt];
      if (value === false) return "";
      const start = opt.length === 1 ? `-${opt}` : `--${opt}`;
      if (value === true) {
        return start;
      }
      return `${start}="${value}"`;
    })
    .join(" ");
