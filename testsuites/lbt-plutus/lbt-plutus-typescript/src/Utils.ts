import { it } from "node:test";
import * as assert from "node:assert/strict";
import * as Fs from "node:fs/promises";
import * as Path from "node:path";

// WARNING(jaredponn): {@link findGoldens} and  {@link fromToGoldenTest} are
// essentially duplicated code (well minor generalizations) of the same testing functions in

/**
/* @param goldenDir: directory for the golden files
/* @param regexFileFilter: see {@link RegExpFileFilter}.
 */
export async function findGoldens(
  goldenDir: string,
  regexFileFilter: RegExpFileFilter,
): Promise<[string, string][]> {
  const files: string[] = await Fs.readdir(goldenDir, { recursive: true });

  const filteredFiles: [string, string][] = [];
  for (const file of files) {
    const filtered = regexFileFilter.match(file);
    if (typeof filtered === "string") {
      filteredFiles.push(
        [Path.join(goldenDir, file), filtered],
      );
    }
  }
  return filteredFiles;
}

/**
 * Wraps a regular expression s.t. when matching strings, we match
 * `basename(<str>).match(<regex>)`
 */
export class RegExpFileFilter {
  #regexp: RegExp;

  /**
   * @param `<str>.match(regexp)` must either match with exactly one string, or
   * not match at all.
   */
  constructor(regexp: Readonly<RegExp>) {
    this.#regexp = regexp;
  }

  match(filepath: string): string | null {
    const matches = Path.basename(filepath).match(this.#regexp);
    if (matches === null) {
      return null;
    } else if (matches.length === 1) {
      return matches[0];
    } else {
      throw new Error(
        `RegExpFileFilter: regex \`${this.#regexp}\` produced matches \`${matches}\` for \`${filepath}\`, but should only produce exactly one match`,
      );
    }
  }

  toString(): string {
    return this.#regexp.toString();
  }
}

export async function fromToGoldenTest<A>(
  goldenDir: string,
  regexFileFilter: RegExpFileFilter,
  // assertGolden: function which asserts whether the test is valid.
  // index: golden test file name (excluding extension)
  // content: the contents of the golden test file (assumed to be UTF8 encoded
  // file).
  assertGolden: (index: string, content: string) => void,
  goldens: A[],
): Promise<void> {
  const foundGoldens: [string, string][] = await findGoldens(
    goldenDir,
    regexFileFilter,
  );

  if (foundGoldens.length !== goldens.length) {
    const errMsg =
      `lbt-plutus-typescript: warning: expected to find ${goldens.length} golden files for ${regexFileFilter} in ${goldenDir}, but got ${foundGoldens.length}. Forgot to (re)generate the goldens? Or there is a mismatch between the TS goldens and generated Haskell goldens`;
    console.warn(errMsg); // TODO(jaredponn): apparently there is a mismatch between the TS goldens
    // and the HS goldens.. The HS script apparently only likes outputting 10
    // golden tests when there are clearly more for things like e.g.
    // PlutusData.
    // One day, add this back in the future:
    // ```
    // assert.fail(errMsg)
    // ```
  }

  for (const [filepath, index] of foundGoldens) {
    const contents = await Fs.readFile(filepath, { encoding: "utf8" });

    it(`${index}: at ${filepath}`, () => {
      assertGolden(index, contents);
    });
  }
}

export function mkFromToAssertGolden<A, B>(
  deserialise: (contents: string) => A,
  from: (a: A) => B,
  to: (b: B) => A,
  serialise: (a: A) => string,
): (index: string, contents: string) => void {
  return (index: string, contents: string) => {
    try {
      const fromTo = serialise(to(from(deserialise(contents))));

      if (contents !== fromTo) {
        assert.fail(
          `Golden test failed for ${index}. Expected:\n\`${contents}\`\nbut got\n\`${fromTo}\``,
        );
      }
    } catch (err) {
      assert.fail(
        `Golden test failed for ${index} since an error was thrown: \`${err}\`.`,
      );
    }
  };
}
