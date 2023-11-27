// Tests for the instances for `TxInInfo`
import * as LbAssocMap from "../LambdaBuffers/Runtime/AssocMap.js";
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbV2 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V2.js";
//import * as LbPlutusData from "../LambdaBuffers/Runtime/PlutusData.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

import * as TestTxOutRef from "./TxOutRefInstances-test.js";
import * as TestTxOut from "./TxOutInstances-test.js";

export function fcTxInInfo(): fc.Arbitrary<LbV2.TxInInfo> {
  return fc.record({
    txInInfoOutRef: TestTxOutRef.fcTxOutRef(),
    txInInfoResolved: TestTxOut.fcTxOut(),
  }) as fc.Arbitrary<LbV2.TxInInfo>;
}

const pubKeyHash1 = LbPrelude.fromJust(
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

const credential1: LbV1.Credential = {
  name: "PubKeyCredential",
  fields: pubKeyHash1,
};
const address1: LbV1.Address = {
  addressCredential: credential1,
  addressStakingCredential: { name: "Nothing" },
};

const value1: LbV1.Value = LbAssocMap.fromList([]);

const outputDatum1: LbV2.OutputDatum = { name: "NoOutputDatum" };

const txOut1: LbV2.TxOut = {
  txOutAddress: address1,
  txOutValue: value1,
  txOutDatum: outputDatum1,
  txOutReferenceScript: { name: "Nothing" },
};

const txId1: LbV1.TxId = LbPrelude.fromJust(
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

const txOutRef1: LbV1.TxOutRef = { txOutRefId: txId1, txOutRefIdx: 69n };

const txOutRef2: LbV1.TxOutRef = { txOutRefId: txId1, txOutRefIdx: 420n };

const txInInfo1: LbV2.TxInInfo = {
  txInInfoOutRef: txOutRef1,
  txInInfoResolved: txOut1,
};

const txInInfo2: LbV2.TxInInfo = {
  txInInfoOutRef: txOutRef2,
  txInInfoResolved: txOut1,
};

describe("TxInInfo tests", () => {
  describe("Eq TxInInfo tests", () => {
    const dict = LbV2.eqTxInInfo;
    TestUtils.eqIt(dict, txInInfo1, txInInfo1, true);
    TestUtils.neqIt(dict, txInInfo1, txInInfo1, false);

    TestUtils.eqIt(dict, txInInfo2, txInInfo2, true);
    TestUtils.neqIt(dict, txInInfo2, txInInfo2, false);

    TestUtils.eqIt(dict, txInInfo1, txInInfo2, false);
    TestUtils.neqIt(dict, txInInfo1, txInInfo2, true);

    TestUtils.eqIt(dict, txInInfo2, txInInfo1, false);
    TestUtils.neqIt(dict, txInInfo2, txInInfo1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxInInfo(),
          fcTxInInfo(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json TxInInfo tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV2.jsonTxInInfo, txInInfo1, {
      output: LbV2.jsonTxOut.toJson(txOut1),
      reference: LbV1.jsonTxOutRef.toJson(txOutRef1),
    });

    TestUtils.toJsonAndFromJsonIt(LbV2.jsonTxInInfo, txInInfo2, {
      output: LbV2.jsonTxOut.toJson(txOut1),
      reference: LbV1.jsonTxOutRef.toJson(txOutRef2),
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxInInfo(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV2.jsonTxInInfo, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData TxInInfo tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV2.toDataTxInInfo,
      LbV2.fromDataTxInInfo,
      txInInfo1,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataTxOutRef.toData(txOutRef1),
          LbV2.toDataTxOut.toData(txOut1),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV2.toDataTxInInfo,
      LbV2.fromDataTxInInfo,
      txInInfo2,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataTxOutRef.toData(txOutRef2),
          LbV2.toDataTxOut.toData(txOut1),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxInInfo(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV2.toDataTxInInfo,
              LbV2.fromDataTxInInfo,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
