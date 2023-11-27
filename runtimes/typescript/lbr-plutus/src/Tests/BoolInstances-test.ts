// // Tests for the instances for `Bool`
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../LambdaBuffers/Runtime/Prelude/Instances.js";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

describe("Bool tests", () => {
  describe("Eq Bool tests", () => {
    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fc.boolean(),
          fc.boolean(),
          (l, r) => {
            TestUtils.negationTest(LbPrelude.eqBool, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Bool tests", () => {
    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fc.boolean(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbPrelude.jsonBool, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Bool tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbPreludeInstances.toDataBool,
      LbPreludeInstances.fromDataBool,
      false,
      { name: "Constr", fields: [0n, []] },
    );

    TestUtils.toDataAndFromDataIt(
      LbPreludeInstances.toDataBool,
      LbPreludeInstances.fromDataBool,
      true,
      { name: "Constr", fields: [1n, []] },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fc.boolean(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbPreludeInstances.toDataBool,
              LbPreludeInstances.fromDataBool,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
