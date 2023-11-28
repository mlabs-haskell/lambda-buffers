// Tests for the instances for `TxId`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

// `txId1` is distinct from `txId2`
export const txId1: LbV1.TxId = LbPrelude.fromJust(
  LbV1.txIdFromBytes(
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
export const txId2: LbV1.TxId = LbPrelude.fromJust(
  LbV1.txIdFromBytes(Uint8Array.from(
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

export function fcTxId(): fc.Arbitrary<LbV1.TxId> {
  return fc.uint8Array({ minLength: 32, maxLength: 32 }).map((t) => {
    return LbPrelude.fromJust(LbV1.txIdFromBytes(t));
  });
}

describe("TxId tests", () => {
  describe("Eq TxId tests", () => {
    const dict = LbV1.eqTxId;

    TestUtils.eqIt(dict, txId1, txId1, true);
    TestUtils.eqIt(dict, txId2, txId1, false);
    TestUtils.eqIt(dict, txId1, txId2, false);
    TestUtils.eqIt(dict, txId2, txId2, true);

    TestUtils.neqIt(dict, txId1, txId1, false);
    TestUtils.neqIt(dict, txId2, txId1, true);
    TestUtils.neqIt(dict, txId1, txId2, true);
    TestUtils.neqIt(dict, txId2, txId2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxId(),
          fcTxId(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [[txId1, txId1], [txId1, txId2], [txId2, txId1], [
            txId2,
            txId2,
          ]],
        },
      );
    });
  });

  describe("Json TxId tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonTxId,
      txId2,
      "201f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201",
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonTxId,
      txId1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxId(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonTxId, data);
          },
        ),
        { examples: [[txId1], [txId2]] },
      );
    });
  });

  describe("IsPlutusData TxId tests", () => {
    TestUtils.isPlutusDataIt(LbV1.isPlutusDataTxId, txId2, {
      name: "Constr",
      fields: [0n, [{ name: "Bytes", fields: txId2 }]],
    });
    TestUtils.isPlutusDataIt(LbV1.isPlutusDataTxId, txId1, {
      name: "Constr",
      fields: [0n, [{ name: "Bytes", fields: txId1 }]],
    });

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxId(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataTxId,
              data,
            );
          },
        ),
        { examples: [[txId1], [txId2]] },
      );
    });
  });
});
