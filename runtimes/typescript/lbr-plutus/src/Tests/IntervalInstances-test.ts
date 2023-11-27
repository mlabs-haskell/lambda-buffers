// Tests for the instances for various types in the `V1/Interval.js` file
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

export function fcExtended<A>(
  arb: fc.Arbitrary<A>,
): fc.Arbitrary<LbV1.Extended<A>> {
  const { extended } = fc.letrec((tie) => ({
    extended: fc.oneof({}, tie("NegInf"), tie("PosInf"), tie("Finite")),
    NegInf: fc.record({
      name: fc.constant("NegInf"),
    }),
    PosInf: fc.record({
      name: fc.constant("PosInf"),
    }),
    Finite: fc.record({
      name: fc.constant("Finite"),
      fields: arb,
    }),
  }));

  return extended as fc.Arbitrary<LbV1.Extended<A>>;
}

export function fcLowerBound<A>(
  arb: fc.Arbitrary<A>,
): fc.Arbitrary<LbV1.LowerBound<A>> {
  return fc.tuple(fcExtended(arb), fc.boolean());
}

export function fcUpperBound<A>(
  arb: fc.Arbitrary<A>,
): fc.Arbitrary<LbV1.UpperBound<A>> {
  return fc.tuple(fcExtended(arb), fc.boolean());
}

export function fcInterval<A>(
  arb: fc.Arbitrary<A>,
): fc.Arbitrary<LbV1.Interval<A>> {
  return fc.record({
    ivFrom: fcLowerBound(arb),
    ivTo: fcUpperBound(arb),
  });
}

describe("Extended tests", () => {
  describe("Eq Extended tests", () => {
    const dict = LbV1.eqExtended(LbPrelude.eqInteger);

    // Same Extended
    TestUtils.eqIt(dict, { name: "NegInf" }, { name: "NegInf" }, true);
    TestUtils.neqIt(dict, { name: "NegInf" }, { name: "NegInf" }, false);
    TestUtils.eqIt(dict, { name: "PosInf" }, { name: "PosInf" }, true);
    TestUtils.neqIt(dict, { name: "PosInf" }, { name: "PosInf" }, false);

    TestUtils.eqIt(dict, { name: "Finite", fields: 0n }, {
      name: "Finite",
      fields: 0n,
    }, true);
    TestUtils.neqIt(dict, { name: "Finite", fields: 0n }, {
      name: "Finite",
      fields: 0n,
    }, false);

    TestUtils.eqIt(dict, { name: "Finite", fields: 69n }, {
      name: "Finite",
      fields: 69n,
    }, true);
    TestUtils.neqIt(dict, { name: "Finite", fields: 69n }, {
      name: "Finite",
      fields: 69n,
    }, false);

    // Different Extended
    TestUtils.eqIt(dict, { name: "NegInf" }, { name: "PosInf" }, false);
    TestUtils.neqIt(dict, { name: "NegInf" }, { name: "PosInf" }, true);

    TestUtils.eqIt(dict, { name: "PosInf" }, { name: "NegInf" }, false);
    TestUtils.neqIt(dict, { name: "PosInf" }, { name: "NegInf" }, true);

    TestUtils.eqIt(dict, { name: "Finite", fields: -69n }, {
      name: "Finite",
      fields: 0n,
    }, false);
    TestUtils.neqIt(dict, { name: "Finite", fields: -69n }, {
      name: "Finite",
      fields: 0n,
    }, true);

    TestUtils.eqIt(
      dict,
      { name: "Finite", fields: 69n },
      { name: "PosInf" },
      false,
    );
    TestUtils.neqIt(
      dict,
      { name: "Finite", fields: 69n },
      { name: "PosInf" },
      true,
    );

    TestUtils.eqIt(
      dict,
      { name: "Finite", fields: 69n },
      { name: "NegInf" },
      false,
    );
    TestUtils.neqIt(
      dict,
      { name: "Finite", fields: 69n },
      { name: "NegInf" },
      true,
    );

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcExtended(fc.bigInt()),
          fcExtended(fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Extended tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonExtended(LbPrelude.jsonInteger), {
      name: "NegInf",
    }, { name: "NegInf", fields: [] });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonExtended(LbPrelude.jsonInteger), {
      name: "PosInf",
    }, { name: "PosInf", fields: [] });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonExtended(LbPrelude.jsonInteger), {
      name: "Finite",
      fields: 69n,
    }, { name: "Finite", fields: [new LbPrelude.Scientific(69n)] });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcExtended(fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbV1.jsonExtended(LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Address tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataExtended(LbV1.toDataInteger),
      LbV1.fromDataExtended(LbV1.fromDataInteger),
      { name: "NegInf" },
      { name: "Constr", fields: [0n, []] },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataExtended(LbV1.toDataInteger),
      LbV1.fromDataExtended(LbV1.fromDataInteger),
      { name: "Finite", fields: 69n },
      { name: "Constr", fields: [1n, [LbV1.toDataInteger.toData(69n)]] },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataExtended(LbV1.toDataInteger),
      LbV1.fromDataExtended(LbV1.fromDataInteger),
      { name: "PosInf" },
      { name: "Constr", fields: [2n, []] },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcExtended(fc.bigInt()),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataExtended(LbV1.toDataInteger),
              LbV1.fromDataExtended(LbV1.fromDataInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});

describe("LowerBound tests", () => {
  describe("Eq LowerBound tests", () => {
    const dict = LbV1.eqLowerBound(LbPrelude.eqInteger);

    // Same LowerBound
    TestUtils.eqIt(
      dict,
      [{ name: "NegInf" }, true],
      [{ name: "NegInf" }, true],
      true,
    );
    TestUtils.neqIt(dict, [{ name: "NegInf" }, true], [
      { name: "NegInf" },
      true,
    ], false);

    TestUtils.eqIt(
      dict,
      [{ name: "PosInf" }, true],
      [{ name: "PosInf" }, true],
      true,
    );
    TestUtils.neqIt(dict, [{ name: "PosInf" }, true], [
      { name: "PosInf" },
      true,
    ], false);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 69n,
    }, true], true);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 69n,
    }, true], false);

    TestUtils.eqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      false,
    ], true);
    TestUtils.neqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      false,
    ], false);

    TestUtils.eqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      false,
    ], true);
    TestUtils.neqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      false,
    ], false);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, false], [{
      name: "Finite",
      fields: 69n,
    }, false], true);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, false], [{
      name: "Finite",
      fields: 69n,
    }, false], false);

    // Different LowerBound
    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 70n,
    }, true], false);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 70n,
    }, true], true);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 70n }, false], [{
      name: "Finite",
      fields: 70n,
    }, true], false);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 70n }, false], [{
      name: "Finite",
      fields: 70n,
    }, true], true);

    TestUtils.eqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      true,
    ], false);
    TestUtils.neqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      true,
    ], true);

    TestUtils.eqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      true,
    ], false);
    TestUtils.neqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      true,
    ], true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcLowerBound(fc.bigInt()),
          fcLowerBound(fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json LowerBound tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "NegInf",
    }, true], { bound: { name: "NegInf", fields: [] }, closed: true });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "PosInf",
    }, true], { bound: { name: "PosInf", fields: [] }, closed: true });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "NegInf",
    }, false], { bound: { name: "NegInf", fields: [] }, closed: false });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "PosInf",
    }, false], { bound: { name: "PosInf", fields: [] }, closed: false });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "Finite",
      fields: 69n,
    }, true], {
      bound: { name: "Finite", fields: [new LbPrelude.Scientific(69n)] },
      closed: true,
    });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonLowerBound(LbPrelude.jsonInteger), [{
      name: "Finite",
      fields: 69n,
    }, false], {
      bound: { name: "Finite", fields: [new LbPrelude.Scientific(69n)] },
      closed: false,
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcLowerBound(fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbV1.jsonLowerBound(LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Address tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "NegInf" }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "NegInf" }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "NegInf" }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "NegInf" }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "Finite", fields: 69n }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({
            name: "Finite",
            fields: 69n,
          }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "Finite", fields: 69n }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({
            name: "Finite",
            fields: 69n,
          }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "PosInf" }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "PosInf" }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLowerBound(LbV1.toDataInteger),
      LbV1.fromDataLowerBound(LbV1.fromDataInteger),
      [{ name: "PosInf" }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "PosInf" }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcLowerBound(fc.bigInt()),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataLowerBound(LbV1.toDataInteger),
              LbV1.fromDataLowerBound(LbV1.fromDataInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});

// Mostly duplicated from LowerBound
describe("UpperBound tests", () => {
  describe("Eq UpperBound tests", () => {
    const dict = LbV1.eqUpperBound(LbPrelude.eqInteger);

    // Same UpperBound
    TestUtils.eqIt(
      dict,
      [{ name: "NegInf" }, true],
      [{ name: "NegInf" }, true],
      true,
    );
    TestUtils.neqIt(dict, [{ name: "NegInf" }, true], [
      { name: "NegInf" },
      true,
    ], false);

    TestUtils.eqIt(
      dict,
      [{ name: "PosInf" }, true],
      [{ name: "PosInf" }, true],
      true,
    );
    TestUtils.neqIt(dict, [{ name: "PosInf" }, true], [
      { name: "PosInf" },
      true,
    ], false);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 69n,
    }, true], true);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 69n,
    }, true], false);

    TestUtils.eqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      false,
    ], true);
    TestUtils.neqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      false,
    ], false);

    TestUtils.eqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      false,
    ], true);
    TestUtils.neqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      false,
    ], false);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, false], [{
      name: "Finite",
      fields: 69n,
    }, false], true);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, false], [{
      name: "Finite",
      fields: 69n,
    }, false], false);

    // Different UpperBound
    TestUtils.eqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 70n,
    }, true], false);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 69n }, true], [{
      name: "Finite",
      fields: 70n,
    }, true], true);

    TestUtils.eqIt(dict, [{ name: "Finite", fields: 70n }, false], [{
      name: "Finite",
      fields: 70n,
    }, true], false);
    TestUtils.neqIt(dict, [{ name: "Finite", fields: 70n }, false], [{
      name: "Finite",
      fields: 70n,
    }, true], true);

    TestUtils.eqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      true,
    ], false);
    TestUtils.neqIt(dict, [{ name: "PosInf" }, false], [
      { name: "PosInf" },
      true,
    ], true);

    TestUtils.eqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      true,
    ], false);
    TestUtils.neqIt(dict, [{ name: "NegInf" }, false], [
      { name: "NegInf" },
      true,
    ], true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcUpperBound(fc.bigInt()),
          fcUpperBound(fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json UpperBound tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "NegInf",
    }, true], { bound: { name: "NegInf", fields: [] }, closed: true });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "PosInf",
    }, true], { bound: { name: "PosInf", fields: [] }, closed: true });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "NegInf",
    }, false], { bound: { name: "NegInf", fields: [] }, closed: false });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "PosInf",
    }, false], { bound: { name: "PosInf", fields: [] }, closed: false });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "Finite",
      fields: 69n,
    }, true], {
      bound: { name: "Finite", fields: [new LbPrelude.Scientific(69n)] },
      closed: true,
    });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonUpperBound(LbPrelude.jsonInteger), [{
      name: "Finite",
      fields: 69n,
    }, false], {
      bound: { name: "Finite", fields: [new LbPrelude.Scientific(69n)] },
      closed: false,
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcUpperBound(fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbV1.jsonUpperBound(LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Address tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "NegInf" }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "NegInf" }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "NegInf" }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "NegInf" }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "Finite", fields: 69n }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({
            name: "Finite",
            fields: 69n,
          }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "Finite", fields: 69n }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({
            name: "Finite",
            fields: 69n,
          }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "PosInf" }, true],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "PosInf" }),
          LbV1.toDataBool.toData(true),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataUpperBound(LbV1.toDataInteger),
      LbV1.fromDataUpperBound(LbV1.fromDataInteger),
      [{ name: "PosInf" }, false],
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataExtended(LbV1.toDataInteger).toData({ name: "PosInf" }),
          LbV1.toDataBool.toData(false),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcUpperBound(fc.bigInt()),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataUpperBound(LbV1.toDataInteger),
              LbV1.fromDataUpperBound(LbV1.fromDataInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});

describe("Interval tests", () => {
  describe("Eq Interval tests", () => {
    const dict = LbV1.eqInterval(LbPrelude.eqInteger);

    // Same Interval
    TestUtils.eqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      true,
    );
    TestUtils.neqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      false,
    );

    TestUtils.eqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      true,
    );
    TestUtils.neqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      false,
    );

    TestUtils.eqIt(dict, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, true);
    TestUtils.neqIt(dict, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, false);

    // Different Interval
    TestUtils.eqIt(dict, {
      ivFrom: [{ name: "NegInf" }, true],
      ivTo: [{ name: "NegInf" }, true],
    }, {
      ivFrom: [{ name: "NegInf" }, true],
      ivTo: [{ name: "NegInf" }, false],
    }, false);
    TestUtils.neqIt(dict, {
      ivFrom: [{ name: "NegInf" }, true],
      ivTo: [{ name: "NegInf" }, true],
    }, {
      ivFrom: [{ name: "NegInf" }, true],
      ivTo: [{ name: "NegInf" }, false],
    }, true);

    TestUtils.eqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      false,
    );
    TestUtils.neqIt(
      dict,
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "PosInf" }, true] },
      { ivFrom: [{ name: "NegInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      true,
    );

    TestUtils.eqIt(dict, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, {
      ivFrom: [{ name: "Finite", fields: 68n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, false);
    TestUtils.neqIt(dict, {
      ivFrom: [{ name: "Finite", fields: 69n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, {
      ivFrom: [{ name: "Finite", fields: 68n }, true],
      ivTo: [{ name: "PosInf" }, true],
    }, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcInterval(fc.bigInt()),
          fcInterval(fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Interval tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonInterval(LbPrelude.jsonInteger), {
      ivFrom: [{ name: "PosInf" }, true],
      ivTo: [{ name: "NegInf" }, true],
    }, {
      from: LbV1.jsonLowerBound(LbPrelude.jsonInteger).toJson([{
        name: "PosInf",
      }, true]),
      to: LbV1.jsonUpperBound(LbPrelude.jsonInteger).toJson([
        { name: "NegInf" },
        true,
      ]),
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcInterval(fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbV1.jsonInterval(LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData Address tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataInterval(LbV1.toDataInteger),
      LbV1.fromDataInterval(LbV1.fromDataInteger),
      { ivFrom: [{ name: "PosInf" }, true], ivTo: [{ name: "NegInf" }, true] },
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataLowerBound(LbV1.toDataInteger).toData([
            { name: "PosInf" },
            true,
          ]),
          LbV1.toDataUpperBound(LbV1.toDataInteger).toData([
            { name: "NegInf" },
            true,
          ]),
        ]],
      },
    );
    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcInterval(fc.bigInt()),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataInterval(LbV1.toDataInteger),
              LbV1.fromDataInterval(LbV1.fromDataInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
