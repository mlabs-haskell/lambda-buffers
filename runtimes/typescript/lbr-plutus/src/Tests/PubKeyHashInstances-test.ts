// Tests for the instances for `PubKeyHash`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

// `pubKeyHash1` is distinct from `pubKeyHash2`
export const pubKeyHash1: LbV1.PubKeyHash = LbPrelude.fromJust(
  LbV1.pubKeyHashFromBytes(
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
export const pubKeyHash2: LbV1.PubKeyHash = LbPrelude.fromJust(
  LbV1.pubKeyHashFromBytes(Uint8Array.from(
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

export function fcPubKeyHash(): fc.Arbitrary<LbV1.PubKeyHash> {
  return fc.uint8Array({ minLength: 28, maxLength: 28 }).map((t) => {
    return LbPrelude.fromJust(LbV1.pubKeyHashFromBytes(t));
  });
}

describe("PubKeyHash tests", () => {
  describe("Eq PubKeyHash tests", () => {
    const dict = LbV1.eqPubKeyHash;

    TestUtils.eqIt(dict, pubKeyHash1, pubKeyHash1, true);
    TestUtils.eqIt(dict, pubKeyHash2, pubKeyHash1, false);
    TestUtils.eqIt(dict, pubKeyHash1, pubKeyHash2, false);
    TestUtils.eqIt(dict, pubKeyHash2, pubKeyHash2, true);

    TestUtils.neqIt(dict, pubKeyHash1, pubKeyHash1, false);
    TestUtils.neqIt(dict, pubKeyHash2, pubKeyHash1, true);
    TestUtils.neqIt(dict, pubKeyHash1, pubKeyHash2, true);
    TestUtils.neqIt(dict, pubKeyHash2, pubKeyHash2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcPubKeyHash(),
          fcPubKeyHash(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [[pubKeyHash1, pubKeyHash1], [pubKeyHash1, pubKeyHash2], [
            pubKeyHash2,
            pubKeyHash1,
          ], [pubKeyHash2, pubKeyHash2]],
        },
      );
    });
  });

  describe("Json PubKeyHash tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonPubKeyHash,
      pubKeyHash2,
      "1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201",
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonPubKeyHash,
      pubKeyHash1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c",
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcPubKeyHash(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonPubKeyHash, data);
          },
        ),
        { examples: [[pubKeyHash1], [pubKeyHash2]] },
      );
    });
  });

  describe("IsPlutusData PubKeyHash tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataPubKeyHash,
      pubKeyHash2,
      { name: "Bytes", fields: pubKeyHash2 },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataPubKeyHash,
      pubKeyHash1,
      { name: "Bytes", fields: pubKeyHash1 },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcPubKeyHash(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataPubKeyHash,
              data,
            );
          },
        ),
        { examples: [[pubKeyHash1], [pubKeyHash2]] },
      );
    });
  });
});
