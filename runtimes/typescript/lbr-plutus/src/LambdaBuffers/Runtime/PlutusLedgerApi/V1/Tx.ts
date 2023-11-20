// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs

import type { Eq, Integer, Json } from "lbr-prelude";
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

type TxId = LbBytes.LedgerBytes & { __compileTimeOnlyTxId: TxId };

/**
 * Checks if the bytes are 32 bytes long.
 */
export function txIdFromBytes(bytes: LbBytes.LedgerBytes): TxId | undefined {
  if (bytes.length === 32) {
    return bytes as TxId;
  } else {
    return undefined;
  }
}

/**
 * Alias for the identity function
 */
export function txIdToBytes(txid: TxId): LbBytes.LedgerBytes {
  return txid;
}

export const eqTxId: Eq<TxId> = LbBytes.eqLedgerBytes as Eq<TxId>;
export const jsonTxId: Json<TxId> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = txIdFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (bs === undefined) {
      throw new JsonError(`TxId should be 32 bytes`);
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
          const res = txIdFromBytes(
            LbBytes.fromDataLedgerBytes.fromData(plutusData.fields[1][0]!),
          );
          if (res === undefined) {
            break;
          }
          return res;
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
