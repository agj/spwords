import { tap, curry, append } from "ramda";
import { spawn } from "child_process";

export const log = tap(console.log);

export const prepend = curry((prep, text) => prep + text);

export const toJson = (data: any) => JSON.stringify(data, null, "\t");

export const run = (
  program: string,
  options: Record<string, string | boolean> = {},
  furtherOptions?: Record<string, string | boolean>
) => {
  const [cmd, ...cmds] = program.split(" ");
  const opts = cmds.concat(optionsToArray(options));
  const allOpts = furtherOptions
    ? append("--", opts).concat(optionsToArray(furtherOptions))
    : opts;

  const proc = spawn(cmd ?? "echo", allOpts, {
    shell: true,
    stdio: "inherit",
  });

  return proc;
};

// Internal

const optionsToArray = (
  options: Record<string, string | boolean>
): Array<string> =>
  Object.keys(options).map((opt) => {
    const value = options[opt];
    if (value === false) return "";
    const start = opt.length === 1 ? `-${opt}` : `--${opt}`;
    if (value === true) {
      return start;
    }
    return `${start}="${value}"`;
  });
