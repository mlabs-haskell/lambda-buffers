// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs

import type { Bytes, Eq, Integer, Json } from "lbr-prelude";
import type { FromData, ToData } from "../../PlutusData.js";
import { FromDataError } from "../../PlutusData.js";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

// -- PlutusLedgerApi.V1.Tx
// opaque TxId
//
// instance PlutusData TxId
// instance Eq TxId
// instance Json TxId

type TxId = Bytes;

export const eqTxId = LbBytes.eqLedgerBytes;
export const jsonTxId: Json<TxId> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = LbBytes.jsonLedgerBytes.fromJson(value);
    if (bs.length !== 32) {
      throw new JsonError(`Expected 32 bytes`);
    }
    return bs;
  },
};
export const toDataTxId: ToData<TxId> = {
  toData: (txid) => {
    return {
      name: "Constr",
      fields: [0n, [LbBytes.toDataLedgerBytes.toData(txid)]],
    };
  },
};
export const fromDataTxId: FromData<TxId> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 1) {
          return LbBytes.fromDataLedgerBytes.fromData(plutusData.fields[1][0]!);
        }
        break;
      default:
        break;
    }
    throw new FromDataError("Invalid data");
  },
};

// opaque TxOutRef
//
// instance PlutusData TxOutRef
// instance Eq TxOutRef
// instance Json TxOutRef
//
export type TxOutRef = { txOutRefId: TxId; txOutRefIdx: Integer };

export const eqTxOutRef: Eq<TxOutRef> = {
  eq: (l, r) => {
    return eqTxId.eq(l.txOutRefId, r.txOutRefId) &&
      LbPrelude.eqInteger.eq(l.txOutRefIdx, r.txOutRefIdx);
  },
  neq: (l, r) => {
    return eqTxId.neq(l.txOutRefId, r.txOutRefId) ||
      LbPrelude.eqInteger.neq(l.txOutRefIdx, r.txOutRefIdx);
  },
};
export const jsonTxOutRef: Json<TxOutRef> = {
  toJson: (txoutRef) => {
    return {
      "transaction_id": jsonTxId.toJson(txoutRef.txOutRefId),
      "index": LbPrelude.jsonInteger.toJson(txoutRef.txOutRefIdx),
    };
  },
  fromJson: (value) => {
    const txid = LbPrelude.caseFieldWithValue(
      "transaction_id",
      jsonTxId.fromJson,
      value,
    );
    const txidx = LbPrelude.caseFieldWithValue(
      "index",
      LbPrelude.jsonInteger.fromJson,
      value,
    );
    return { txOutRefId: txid, txOutRefIdx: txidx };
  },
};

export const toDataTxOutRef: ToData<TxOutRef> = {
  toData: (txoutref) => {
    return {
      name: "Constr",
      fields: [0n, [
        toDataTxId.toData(txoutref.txOutRefId),
        LbPreludeInstances.toDataInteger.toData(txoutref.txOutRefIdx),
      ]],
    };
  },
};

export const fromDataTxOutRef: FromData<TxOutRef> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 2) {
          return {
            txOutRefId: fromDataTxId.fromData(plutusData.fields[1][0]!),
            txOutRefIdx: LbPreludeInstances.fromDataInteger.fromData(
              plutusData.fields[1][1]!,
            ),
          };
        }
        break;
      default:
        break;
    }
    throw new FromDataError("Invalid data");
  },
};
