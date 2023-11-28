import type { Eq, Integer, Json, Maybe } from "lbr-prelude";
import type { IsPlutusData } from "../../PlutusData.js";
import { IsPlutusDataError } from "../../PlutusData.js";
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
export type TxId = LbBytes.LedgerBytes & { __compileTimeOnlyTxId: TxId };

/**
 * {@link txIdFromBytes} checks if the bytes are 32 bytes long.
 */
export function txIdFromBytes(bytes: LbBytes.LedgerBytes): Maybe<TxId> {
  if (bytes.length === 32) {
    return { name: "Just", fields: bytes as TxId };
  } else {
    return { name: "Nothing" };
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
    if (bs.name === "Nothing") {
      throw new JsonError(`TxId should be 32 bytes`);
    }
    return bs.fields;
  },
};

/**
 * {@link IsPlutusData} instance for {@link TxId}
 *
 * Note this includes the `0` tag.
 */
export const isPlutusDataTxId: IsPlutusData<TxId> = {
  toData: (txid) => {
    return {
      name: "Constr",
      fields: [0n, [LbBytes.isPlutusDataLedgerBytes.toData(txid)]],
    };
  },

  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 1) {
          const res = txIdFromBytes(
            LbBytes.isPlutusDataLedgerBytes.fromData(plutusData.fields[1][0]!),
          );
          if (res.name === "Nothing") {
            break;
          }
          return res.fields;
        }
        break;
      default:
        break;
    }
    throw new IsPlutusDataError("Invalid data");
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
 *  {@link IsPlutusData} instance for {@link TxOutRef}
 */
export const isPlutusDataTxOutRef: IsPlutusData<TxOutRef> = {
  toData: (txoutref) => {
    return {
      name: "Constr",
      fields: [0n, [
        isPlutusDataTxId.toData(txoutref.txOutRefId),
        LbPreludeInstances.isPlutusDataInteger.toData(txoutref.txOutRefIdx),
      ]],
    };
  },
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 2) {
          return {
            txOutRefId: isPlutusDataTxId.fromData(plutusData.fields[1][0]!),
            txOutRefIdx: LbPreludeInstances.isPlutusDataInteger.fromData(
              plutusData.fields[1][1]!,
            ),
          };
        }
        break;
      default:
        break;
    }
    throw new IsPlutusDataError("Invalid data");
  },
};
