// Tests for the instances for `DatumHash`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

// `datumHash1` is distinct from `datumHash2`
export const datumHash1: LbV1.DatumHash = LbPrelude.fromJust(
  LbV1.datumHashFromBytes(
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
export const datumHash2: LbV1.DatumHash = LbPrelude.fromJust(
  LbV1.datumHashFromBytes(Uint8Array.from(
    [
      32,
      31,
      30,
      29,
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

export function fcDatumHash(): fc.Arbitrary<LbV1.DatumHash> {
  return fc.uint8Array({ minLength: 32, maxLength: 32 }).map((t) => {
    return LbPrelude.fromJust(LbV1.datumHashFromBytes(t));
  });
}

describe("DatumHash tests", () => {
  describe("Eq DatumHash tests", () => {
    const dict = LbV1.eqDatumHash;

    TestUtils.eqIt(dict, datumHash1, datumHash1, true);
    TestUtils.eqIt(dict, datumHash2, datumHash1, false);
    TestUtils.eqIt(dict, datumHash1, datumHash2, false);
    TestUtils.eqIt(dict, datumHash2, datumHash2, true);

    TestUtils.neqIt(dict, datumHash1, datumHash1, false);
    TestUtils.neqIt(dict, datumHash2, datumHash1, true);
    TestUtils.neqIt(dict, datumHash1, datumHash2, true);
    TestUtils.neqIt(dict, datumHash2, datumHash2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcDatumHash(),
          fcDatumHash(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [[datumHash1, datumHash1], [datumHash1, datumHash2], [
            datumHash2,
            datumHash1,
          ], [datumHash2, datumHash2]],
        },
      );
    });
  });

  describe("Json DatumHash tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonDatumHash,
      datumHash2,
      "201f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201",
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonDatumHash,
      datumHash1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcDatumHash(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonDatumHash, data);
          },
        ),
        { examples: [[datumHash1], [datumHash2]] },
      );
    });
  });

  describe("IsPlutusData DatumHash tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataDatumHash,
      datumHash2,
      { name: "Bytes", fields: datumHash2 },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataDatumHash,
      datumHash1,
      { name: "Bytes", fields: datumHash1 },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcDatumHash(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataDatumHash,
              data,
            );
          },
        ),
        { examples: [[datumHash1], [datumHash2]] },
      );
    });
  });
});
