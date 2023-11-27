// Tests for the instances for `TxOut`
import * as LbAssocMap from "../LambdaBuffers/Runtime/AssocMap.js";
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbV2 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V2.js";
//import * as LbPlutusData from "../LambdaBuffers/Runtime/PlutusData.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

import * as TestAddress from "./AddressInstances-test.js";
import * as TestValue from "./ValueInstances-test.js";
import * as TestOutputDatum from "./OutputDatumInstances-test.js";
import * as TestMaybe from "./MaybeInstances-test.js";
import * as TestScriptHash from "./ScriptHashInstances-test.js";

export function fcTxOut(): fc.Arbitrary<LbV2.TxOut> {
  return fc.record({
    txOutAddress: TestAddress.fcAddress(),
    txOutValue: TestValue.fcValue(),
    txOutDatum: TestOutputDatum.fcOutputDatum(),
    txOutReferenceScript: TestMaybe.fcMaybe(TestScriptHash.fcScriptHash()),
  }) as fc.Arbitrary<LbV2.TxOut>;
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

const scriptHash1: LbV1.ScriptHash = LbPrelude.fromJust(
  LbV1.scriptHashFromBytes(
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

const txOut2: LbV2.TxOut = {
  txOutAddress: address1,
  txOutValue: value1,
  txOutDatum: outputDatum1,
  txOutReferenceScript: { name: "Just", fields: scriptHash1 },
};

describe("TxOut tests", () => {
  describe("Eq TxOut tests", () => {
    const dict = LbV2.eqTxOut;
    TestUtils.eqIt(dict, txOut1, txOut1, true);
    TestUtils.neqIt(dict, txOut1, txOut1, false);

    TestUtils.eqIt(dict, txOut2, txOut2, true);
    TestUtils.neqIt(dict, txOut2, txOut2, false);

    TestUtils.eqIt(dict, txOut2, txOut1, false);
    TestUtils.neqIt(dict, txOut2, txOut1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOut(),
          fcTxOut(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json TxOut tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV2.jsonTxOut, txOut1, {
      address: LbV1.jsonAddress.toJson(address1),
      datum: LbV2.jsonOutputDatum.toJson(outputDatum1),
      reference_script: LbPrelude.jsonMaybe(LbV1.jsonScriptHash).toJson({
        name: "Nothing",
      }),
      value: [],
    });

    TestUtils.toJsonAndFromJsonIt(LbV2.jsonTxOut, txOut2, {
      address: LbV1.jsonAddress.toJson(address1),
      datum: LbV2.jsonOutputDatum.toJson(outputDatum1),
      reference_script: LbPrelude.jsonMaybe(LbV1.jsonScriptHash).toJson({
        name: "Just",
        fields: scriptHash1,
      }),
      value: [],
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOut(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV2.jsonTxOut, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData TxOut tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV2.toDataTxOut,
      LbV2.fromDataTxOut,
      txOut1,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataAddress.toData(address1),
          LbV1.toDataValue.toData(value1),
          LbV2.toDataOutputDatum.toData(outputDatum1),
          LbV1.toDataMaybe(LbV1.toDataScriptHash).toData({ name: "Nothing" }),
        ]],
      },
    );

    TestUtils.toDataAndFromDataIt(
      LbV2.toDataTxOut,
      LbV2.fromDataTxOut,
      txOut2,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataAddress.toData(address1),
          LbV1.toDataValue.toData(value1),
          LbV2.toDataOutputDatum.toData(outputDatum1),
          LbV1.toDataMaybe(LbV1.toDataScriptHash).toData({
            name: "Just",
            fields: scriptHash1,
          }),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcTxOut(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV2.toDataTxOut,
              LbV2.fromDataTxOut,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
