// Tests for the instances for `Integer`
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../LambdaBuffers/Runtime/Prelude/Instances.js";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

describe("Integer tests", () => {
  describe("Eq Integer tests", () => {
    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fc.bigInt(),
          fc.bigInt(),
          (l, r) => {
            TestUtils.negationTest(LbPrelude.eqInteger, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Integer tests", () => {
    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fc.bigInt(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbPrelude.jsonInteger, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("IsPlutusData Integer tests", () => {
    TestUtils.isPlutusDataIt(
      LbPreludeInstances.isPlutusDataInteger,
      69n,
      { name: "Integer", fields: 69n },
    );
    TestUtils.isPlutusDataIt(
      LbPreludeInstances.isPlutusDataInteger,
      -69n,
      { name: "Integer", fields: -69n },
    );

    TestUtils.isPlutusDataIt(
      LbPreludeInstances.isPlutusDataInteger,
      0n,
      { name: "Integer", fields: 0n },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fc.bigInt(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbPreludeInstances.isPlutusDataInteger,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
