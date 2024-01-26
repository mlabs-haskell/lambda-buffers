// Unit tests for functionality in `src/Prelude/Runtime/Json.ts`
import { describe, it } from "node:test";
import * as assert from "node:assert/strict";
import * as Goldens from "./Goldens.js";
import * as Fs from "node:fs/promises";
import * as Path from "node:path";
import * as Prelude from "prelude";
import * as PreludeJson from "prelude/Json.js";

import * as LbfFoo from "lbf-prelude-golden-api/LambdaBuffers/Foo.mjs";
// import * as LbfFooBar from "lbf-prelude-golden-api/LambdaBuffers/Foo/Bar.mjs"
import * as LbfDays from "lbf-prelude-golden-api/LambdaBuffers/Days.mjs";

import * as LbrPrelude from "lbr-prelude";

export async function findGoldens(
  goldenDir: string,
  title: string,
): Promise<[string, string][]> {
  const files: string[] = await Fs.readdir(goldenDir, { recursive: true });

  const filteredFiles: [string, string][] = [];
  for (const file of files) {
    if (
      // `title` is a prefix of `file`'s basename
      Path.basename(file).startsWith(title) &&
      // AND it ends with `.json`
      Path.extname(file) === ".json"
    ) {
      filteredFiles.push(
        [Path.join(goldenDir, file), Path.basename(file, ".json")],
      );
    }
  }
  return filteredFiles;
}

export async function fromToGoldenTest<A>(
  jsonDict: Prelude.Json<A>,
  goldenDir: string,
  title: string,
  goldens: A[],
): Promise<void> {
  const foundGoldens: [string, string][] = await findGoldens(goldenDir, title);

  if (foundGoldens.length !== goldens.length) {
    assert.fail(
      `Expected to find ${goldens.length} .json golden files for ${title}, but got ${foundGoldens.length}. Forgot to (re)generate the goldens?`,
    );
  }

  for (const [filepath, index] of foundGoldens) {
    const contents = await Fs.readFile(filepath, { encoding: "utf8" });

    const fromToJson = PreludeJson.stringify(
      jsonDict.toJson(
        jsonDict.fromJson(
          PreludeJson.parseJson(contents),
        ),
      ),
    );

    // TODO(jaredponn). This fails on the _first_ test instead of
    // accumulating all of them.. Perhaps we want to accumulate all of
    // them by e.g. wrapping this with `it`.
    if (contents !== fromToJson) {
      assert.fail(
        `Golden test failed for ${index}. Expected:\n\`${contents}\`\nbut got\n\`${fromToJson}\``,
      );
    }
  }
}

describe("JSON tests", () => {
  const goldenDir = `data/lbt-prelude-golden-data`;
  it(`Foo.A from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.A],
      goldenDir,
      `Foo.A`,
      Goldens.aGoldens(),
    );
  });

  it(`Foo.B from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.B],
      goldenDir,
      `Foo.B`,
      Goldens.bGoldens(),
    );
  });

  it(`Foo.C from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.C],
      goldenDir,
      `Foo.C`,
      Goldens.cGoldens(),
    );
  });

  it(`Foo.D from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.D],
      goldenDir,
      `Foo.D`,
      Goldens.dGoldens(),
    );
  });

  it(`Foo.FInt from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.FInt],
      goldenDir,
      `Foo.FInt`,
      Goldens.fIntGoldens(),
    );
  });

  it(`Foo.GInt from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfFoo.GInt],
      goldenDir,
      `Foo.GInt`,
      Goldens.gIntGoldens(),
    );
  });

  it(`Days.Day from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfDays.Day],
      goldenDir,
      `Days.Day`,
      Goldens.dayGoldens(),
    );
  });

  it(`Days.WorkDay from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfDays.WorkDay],
      goldenDir,
      `Days.WorkDay`,
      Goldens.workDayGoldens(),
    );
  });

  it(`Days.FreeDay from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbfDays.FreeDay],
      goldenDir,
      `Days.FreeDay`,
      Goldens.freeDayGoldens(),
    );
  });

  it(`Prelude.Bool from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Bool],
      goldenDir,
      `Prelude.Bool`,
      Goldens.boolGoldens(),
    );
  });

  it(`Prelude.Char from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Char],
      goldenDir,
      `Prelude.Char`,
      Goldens.charGoldens(),
    );
  });

  it(`Prelude.Integer from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Integer],
      goldenDir,
      `Prelude.Integer`,
      Goldens.integerGoldens(),
    );
  });

  it(`Prelude.Text from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Text],
      goldenDir,
      `Prelude.Text`,
      Goldens.textGoldens(),
    );
  });

  it(`Prelude.Bytes from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Bytes],
      goldenDir,
      `Prelude.Bytes`,
      Goldens.bytesGoldens(),
    );
  });

  it(`Prelude.Maybe from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Maybe](LbrPrelude.Json[LbrPrelude.Bool]),
      goldenDir,
      `Prelude.Maybe`,
      Goldens.maybeGoldens(),
    );
  });

  it(`Prelude.Either from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Either](
        LbrPrelude.Json[LbrPrelude.Bool],
        LbrPrelude.Json[LbrPrelude.Text],
      ),
      goldenDir,
      `Prelude.Either`,
      Goldens.eitherGoldens(),
    );
  });

  it(`Prelude.List from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.List](LbrPrelude.Json[LbrPrelude.Bool]),
      goldenDir,
      `Prelude.List`,
      Goldens.listGoldens(),
    );
  });

  it(`Prelude.Set from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Set](
        Prelude.ordBool,
        LbrPrelude.Json[LbrPrelude.Bool],
      ),
      goldenDir,
      `Prelude.Set`,
      Goldens.setGoldens(),
    );
  });

  it(`Prelude.Map from to golden tests`, async () => {
    await fromToGoldenTest(
      LbrPrelude.Json[LbrPrelude.Map](
        Prelude.ordBool,
        LbrPrelude.Json[LbrPrelude.Bool],
        LbrPrelude.Json[LbrPrelude.Bool],
      ),
      goldenDir,
      `Prelude.Map`,
      Goldens.mapGoldens(),
    );
  });
});
