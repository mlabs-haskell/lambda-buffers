// Tests for the instances for `TxOutRef`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

import * as TestTxId from "./TxIdInstances-test.js";

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

export const txOutRef1: LbV1.TxOutRef = { txOutRefId: txId1, txOutRefIdx: 69n };

export const txOutRef2: LbV1.TxOutRef = {
  txOutRefId: txId2,
  txOutRefIdx: 420n,
};

export function fcTxOutRef(): fc.Arbitrary<LbV1.TxOutRef> {
  return fc.record({
    txOutRefId: TestTxId.fcTxId(),
    txOutRefIdx: fc.bigInt(),
  }) as fc.Arbitrary<LbV1.TxOutRef>;
}

describe("TxOutRef tests", () => {
  describe("Eq Credential tests", () => {
    const dict = LbV1.eqTxOutRef;

    // Same address
    TestUtils.eqIt(dict, txOutRef1, txOutRef1, true);
    TestUtils.neqIt(dict, txOutRef1, txOutRef1, false);

    TestUtils.eqIt(dict, txOutRef2, txOutRef2, true);
    TestUtils.neqIt(dict, txOutRef2, txOutRef2, false);

    // Mixing the addresses
    TestUtils.eqIt(dict, txOutRef2, txOutRef1, false);
    TestUtils.neqIt(dict, txOutRef2, txOutRef1, true);

    TestUtils.eqIt(dict, txOutRef1, txOutRef2, false);
    TestUtils.neqIt(dict, txOutRef1, txOutRef2, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOutRef(),
          fcTxOutRef(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [
            [txOutRef1, txOutRef1],
            [txOutRef2, txOutRef2],
            [txOutRef1, txOutRef2],
            [txOutRef2, txOutRef1],
          ],
        },
      );
    });
  });

  describe("Json TxOutRef tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonTxOutRef, txOutRef1, {
      index: new LbPrelude.Scientific(69n),
      transaction_id: LbV1.jsonTxId.toJson(txId1),
    });

    TestUtils.toJsonAndFromJsonIt(LbV1.jsonTxOutRef, txOutRef2, {
      index: new LbPrelude.Scientific(420n),
      transaction_id: LbV1.jsonTxId.toJson(txId2),
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOutRef(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonTxOutRef, data);
          },
        ),
        { examples: [[txOutRef1], [txOutRef2]] },
      );
    });
  });

  describe("IsPlutusData TxOutRef tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataTxOutRef,
      txOutRef1,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.isPlutusDataTxId.toData(txId1),
          LbV1.isPlutusDataInteger.toData(69n),
        ]],
      },
    );

    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataTxOutRef,
      txOutRef2,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.isPlutusDataTxId.toData(txId2),
          LbV1.isPlutusDataInteger.toData(420n),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOutRef(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataTxOutRef,
              data,
            );
          },
        ),
        { examples: [[txOutRef1], [txOutRef2]] },
      );
    });
  });
});
