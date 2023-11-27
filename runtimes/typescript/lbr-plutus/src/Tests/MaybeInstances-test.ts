// // Tests for the instances for `Maybe`
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../LambdaBuffers/Runtime/Prelude/Instances.js";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

export function fcMaybe<A>(
  arb: fc.Arbitrary<A>,
): fc.Arbitrary<LbPrelude.Maybe<A>> {
  const { maybe } = fc.letrec((tie) => ({
    maybe: fc.oneof({}, tie("Just"), tie("Nothing")),
    Just: fc.record({
      name: fc.constant("Just"),
      fields: arb,
    }),
    Nothing: fc.record({
      name: fc.constant("Nothing"),
    }),
  }));

  return maybe as fc.Arbitrary<LbPrelude.Maybe<A>>;
}

describe("Maybe tests", () => {
  // fc.statistics( fcMaybe(fc.bigint()),
  //                 (maybe) => {
  //                     return `${maybe.name}`
  //                 }
  //                     ,
  //                     { numRuns: 100 },
  //              );

  describe("Eq Maybe tests", () => {
    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcMaybe(fc.bigInt()),
          fcMaybe(fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(
              LbPrelude.eqMaybe(LbPrelude.eqInteger),
              l,
              r,
            );
          },
        ),
        { examples: [] },
      );
    });

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcMaybe(fc.array(fc.string())),
          fcMaybe(fc.array(fc.string())),
          (l, r) => {
            TestUtils.negationTest(
              LbPrelude.eqMaybe(LbPrelude.eqList(LbPrelude.eqText)),
              l,
              r,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Maybe tests", () => {
    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcMaybe(fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbPrelude.jsonMaybe(LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcMaybe(fc.array(fc.string())),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbPrelude.jsonMaybe(LbPrelude.jsonList(LbPrelude.jsonText)),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Maybe tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbPreludeInstances.toDataMaybe(LbPreludeInstances.toDataInteger),
      LbPreludeInstances.fromDataMaybe(LbPreludeInstances.fromDataInteger),
      { name: "Nothing" },
      { name: "Constr", fields: [1n, []] },
    );
    TestUtils.toDataAndFromDataIt(
      LbPreludeInstances.toDataMaybe(LbPreludeInstances.toDataInteger),
      LbPreludeInstances.fromDataMaybe(LbPreludeInstances.fromDataInteger),
      { name: "Just", fields: 69n },
      {
        name: "Constr",
        fields: [0n, [LbPreludeInstances.toDataInteger.toData(69n)]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcMaybe(fc.bigInt()),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbPreludeInstances.toDataMaybe(LbPreludeInstances.toDataInteger),
              LbPreludeInstances.fromDataMaybe(
                LbPreludeInstances.fromDataInteger,
              ),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
