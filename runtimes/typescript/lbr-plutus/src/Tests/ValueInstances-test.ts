// Tests for the instances for various types in the `V1/Value.js` file
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbAssocMap from "../LambdaBuffers/Runtime/AssocMap.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import * as TestAssocMap from "./AssocMap-test.js";
import fc from "fast-check";

// `currencySymbol1` is distinct from `currencySymbol2`
export const currencySymbol1: LbV1.CurrencySymbol = LbPrelude.fromJust(
  LbV1.currencySymbolFromBytes(
    Uint8Array.from([
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
    ]),
  ),
);
export const currencySymbol2: LbV1.CurrencySymbol = LbPrelude.fromJust(
  LbV1.currencySymbolFromBytes(Uint8Array.from(
    [
      28,
      27,
      26,
      25,
      24,
      23,
      22,
      21,
      20,
      19,
      18,
      17,
      16,
      15,
      14,
      13,
      12,
      11,
      10,
      9,
      8,
      7,
      6,
      5,
      4,
      3,
      2,
      1,
    ],
  )),
);

// `tokenName1` is distinct from `tokenName2`
export const tokenName1: LbV1.TokenName = LbPrelude.fromJust(
  LbV1.tokenNameFromBytes(
    Uint8Array.from([
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
    ]),
  ),
);
export const tokenName2: LbV1.TokenName = LbPrelude.fromJust(
  LbV1.tokenNameFromBytes(Uint8Array.from([])),
);

export const value1: LbV1.Value = LbAssocMap.fromList([
  [currencySymbol1, LbAssocMap.fromList([[tokenName1, -69n]])],
  [currencySymbol2, LbAssocMap.fromList([])],
]);
export const value2: LbV1.Value = LbAssocMap.fromList([]);

export function fcCurrencySymbol(): fc.Arbitrary<LbV1.CurrencySymbol> {
  return fc.uint8Array({ minLength: 28, maxLength: 28 }).map((t) => {
    return LbPrelude.fromJust(LbV1.currencySymbolFromBytes(t));
  });
}

export function fcTokenName(): fc.Arbitrary<LbV1.TokenName> {
  return fc.uint8Array({ minLength: 0, maxLength: 32 }).map((t) => {
    return LbPrelude.fromJust(LbV1.tokenNameFromBytes(t));
  });
}

export function fcValue(): fc.Arbitrary<LbV1.Value> {
  return TestAssocMap.fcAssocMap(
    LbV1.eqCurrencySymbol,
    fcCurrencySymbol(),
    TestAssocMap.fcAssocMap(LbV1.eqTokenName, fcTokenName(), fc.bigInt()),
  );
}

describe("CurrencySymbol tests", () => {
  describe("Eq CurrencySymbol tests", () => {
    const dict = LbV1.eqCurrencySymbol;

    TestUtils.eqIt(dict, currencySymbol1, currencySymbol1, true);
    TestUtils.eqIt(dict, currencySymbol2, currencySymbol1, false);
    TestUtils.eqIt(dict, currencySymbol1, currencySymbol2, false);
    TestUtils.eqIt(dict, currencySymbol2, currencySymbol2, true);

    TestUtils.neqIt(dict, currencySymbol1, currencySymbol1, false);
    TestUtils.neqIt(dict, currencySymbol2, currencySymbol1, true);
    TestUtils.neqIt(dict, currencySymbol1, currencySymbol2, true);
    TestUtils.neqIt(dict, currencySymbol2, currencySymbol2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcCurrencySymbol(),
          fcCurrencySymbol(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [
            [currencySymbol1, currencySymbol1],
            [currencySymbol1, currencySymbol2],
            [currencySymbol2, currencySymbol1],
            [currencySymbol2, currencySymbol2],
          ],
        },
      );
    });
  });

  describe("Json CurrencySymbol tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonCurrencySymbol,
      currencySymbol2,
      "1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201",
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonCurrencySymbol,
      currencySymbol1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c",
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcCurrencySymbol(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonCurrencySymbol, data);
          },
        ),
        { examples: [[currencySymbol1], [currencySymbol2]] },
      );
    });
  });

  describe("IsPlutusData CurrencySymbol tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataCurrencySymbol,
      currencySymbol2,
      { name: "Bytes", fields: currencySymbol2 },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataCurrencySymbol,
      currencySymbol1,
      { name: "Bytes", fields: currencySymbol1 },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcCurrencySymbol(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataCurrencySymbol,
              data,
            );
          },
        ),
        { examples: [[currencySymbol1], [currencySymbol2]] },
      );
    });
  });
});

describe("TokenName tests", () => {
  describe("Eq TokenName tests", () => {
    const dict = LbV1.eqTokenName;

    TestUtils.eqIt(dict, tokenName1, tokenName1, true);
    TestUtils.eqIt(dict, tokenName2, tokenName1, false);
    TestUtils.eqIt(dict, tokenName1, tokenName2, false);
    TestUtils.eqIt(dict, tokenName2, tokenName2, true);

    TestUtils.neqIt(dict, tokenName1, tokenName1, false);
    TestUtils.neqIt(dict, tokenName2, tokenName1, true);
    TestUtils.neqIt(dict, tokenName1, tokenName2, true);
    TestUtils.neqIt(dict, tokenName2, tokenName2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcTokenName(),
          fcTokenName(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [[tokenName1, tokenName1], [tokenName1, tokenName2], [
            tokenName2,
            tokenName1,
          ], [tokenName2, tokenName2]],
        },
      );
    });
  });

  describe("Json TokenName tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonTokenName,
      tokenName1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
    );
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonTokenName, tokenName2, "");

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcTokenName(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonTokenName, data);
          },
        ),
        { examples: [[tokenName1], [tokenName2]] },
      );
    });
  });

  describe("IsPlutusData TokenName tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataTokenName,
      tokenName2,
      { name: "Bytes", fields: tokenName2 },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataTokenName,
      tokenName1,
      { name: "Bytes", fields: tokenName1 },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcTokenName(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataTokenName,
              data,
            );
          },
        ),
        { examples: [[tokenName1], [tokenName2]] },
      );
    });
  });
});

describe("Value tests", () => {
  describe("Eq Value tests", () => {
    const dict = LbV1.eqValue;

    TestUtils.eqIt(dict, value1, value1, true);
    TestUtils.neqIt(dict, value1, value1, false);

    TestUtils.eqIt(dict, value2, value2, true);
    TestUtils.neqIt(dict, value2, value2, false);

    TestUtils.eqIt(dict, value1, value2, false);
    TestUtils.neqIt(dict, value1, value2, true);

    TestUtils.eqIt(dict, value2, value1, false);
    TestUtils.neqIt(dict, value2, value1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcValue(),
          fcValue(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json Value tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonValue, value1, [[
      LbV1.jsonCurrencySymbol.toJson(currencySymbol1),
      [
        [LbV1.jsonTokenName.toJson(tokenName1), new LbPrelude.Scientific(-69n)],
      ],
    ], [LbV1.jsonCurrencySymbol.toJson(currencySymbol2), []]]);
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonValue, value2, []);

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcValue(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonValue, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("IsPlutusData Value tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataValue,
      value1,
      {
        name: "Map",
        fields: [[LbV1.isPlutusDataCurrencySymbol.toData(currencySymbol1), {
          name: "Map",
          fields: [[
            LbV1.isPlutusDataTokenName.toData(tokenName1),
            LbV1.isPlutusDataInteger.toData(-69n),
          ]],
        }], [LbV1.isPlutusDataCurrencySymbol.toData(currencySymbol2), {
          name: "Map",
          fields: [],
        }]],
      },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataValue,
      value2,
      { name: "Map", fields: [] },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcValue(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataValue,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
