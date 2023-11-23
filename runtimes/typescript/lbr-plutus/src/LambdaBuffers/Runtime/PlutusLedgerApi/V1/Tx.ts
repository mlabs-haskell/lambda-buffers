// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs

import type { Eq, Integer, Json, Maybe } from "lbr-prelude";
import type { FromData, ToData } from "../../PlutusData.js";
import { FromDataError } from "../../PlutusData.js";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs

/**
 * {@link TxId} is a transaction id i.e., the hash of a transaction. 32 bytes.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs#L51-L65}
 */
type TxId = LbBytes.LedgerBytes & { __compileTimeOnlyTxId: TxId };

/**
 * {@link txIdFromBytes} checks if the bytes are 32 bytes long.
 */
export function txIdFromBytes(bytes: LbBytes.LedgerBytes): Maybe<TxId> {
  if (bytes.length === 32) {
    return bytes as TxId;
  } else {
    return undefined;
  }
}

/**
 * {@link Eq} instance for {@link TxId}
 */
export const eqTxId: Eq<TxId> = LbBytes.eqLedgerBytes as Eq<TxId>;

/**
 * {@link Json} instance for {@link TxId}
 */
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

/**
 * {@link ToData} instance for {@link TxId}
 *
 * Note this includes the `0` tag.
 */
export const toDataTxId: ToData<TxId> = {
  toData: (txid) => {
    return {
      name: "Constr",
      fields: [0n, [LbBytes.toDataLedgerBytes.toData(txid)]],
    };
  },
};

/**
 * {@link FromData} instance for {@link TxId}
 *
 * Note this includes the `0` tag.
 */
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

/**
 * {@link TxOutRef} is a reference to a transaction output.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Tx.hs#L83-L91}
 */
export type TxOutRef = { txOutRefId: TxId; txOutRefIdx: Integer };

/**
 *  {@link Eq} instance for {@link TxOutRef}
 */
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

/**
 *  {@link Json} instance for {@link TxOutRef}
 */
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

/**
 *  {@link ToData} instance for {@link TxOutRef}
 */
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

/**
 *  {@link FromData} instance for {@link TxOutRef}
 */
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
