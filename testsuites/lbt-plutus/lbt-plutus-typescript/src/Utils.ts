import { it } from "node:test";
import * as assert from "node:assert/strict";
import * as Fs from "node:fs/promises";
import * as Path from "node:path";

// WARNING(jaredponn): {@link findGoldens} and  {@link fromToGoldenTest} are
// essentially duplicated code (well minor generalizations) of the same testing
// functions in the lbt-prelude testsuite.

/**
/* @param goldenDir: directory for the golden files
/* @param regexFileFilter: see {@link RegExpFileFilter}.
 *
/* @returns Tuple of [path to golden file, the resulting "regexFileFilter"ed file]
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

/**
 * Runs golden tests in the provided `goldenDir` which satisfy the
 * `regexFileFilter` where the test passes if `assertGolden` does not throw an
 * exception. Note that `goldens` is essentially unused and is only used to
 * warn if the number of the TS representation of equivalent HS generated tests
 * match.
 */
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
      `lbt-plutus-typescript: warning: expected to find ${goldens.length} golden files for ${regexFileFilter} in ${goldenDir}, but got ${foundGoldens.length}. Forgot to (re)generate the goldens? Or there is a mismatch in the number of TS goldens and generated Haskell goldens`;
    console.warn(errMsg); // TODO(jaredponn): apparently there is a mismatch between the TS goldens
    // and the HS goldens.. The HS script apparently only likes outputting at most 10
    // golden tests when there are clearly more tests (e.g.
    // PlutusData has many more tests than just 10)
    // One day, add this back in the future:
    // ```
    // assert.fail(errMsg)
    // ```
    // Note this doesn't actually effect the correctness of the tests.
  }

  for (const [filepath, index] of foundGoldens) {
    await it(`${index} at ${filepath}`, async () => {
      const contents = await Fs.readFile(filepath, { encoding: "utf8" });
      assertGolden(index, contents);
    });
  }
}

/**
 * @returns a function which throws an exception if
 * `serialise(to(from(deserialise(contents)))) != contents` (or any of
 * `serialise`, `to`, `from`, `deserialise` throws an exception)
 */
export function mkFromToAssertGolden<A, B>(
  deserialise: (contents: string) => A,
  from: (a: A) => B,
  to: (b: B) => A,
  serialise: (a: A) => string,
): (index: string, contents: string) => void {
  return (index: string, contents: string) => {
    try {
      const fromTo = JSON.stringify(
        JSON.parse(serialise(to(from(deserialise(contents))))),
        null,
        2,
      );
      const jsonContents = JSON.stringify(JSON.parse(contents), null, 2);

      if (jsonContents !== fromTo) {
        assert.fail(
          `Golden test failed for ${index}. Expected:\n\`${jsonContents}\`\nbut got\n\`${fromTo}\``,
        );
      }
    } catch (err) {
      assert.fail(
        `Golden test failed for ${index} since an error was thrown: \`${err}\`.`,
      );
    }
  };
}
