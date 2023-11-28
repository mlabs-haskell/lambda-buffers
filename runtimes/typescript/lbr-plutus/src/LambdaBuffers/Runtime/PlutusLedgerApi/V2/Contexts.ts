import type { TxOutRef } from "../V1/Tx.js";
import * as LbV1Tx from "../V1/Tx.js";
import type { IsPlutusData } from "../../PlutusData.js";
import { IsPlutusDataError } from "../../PlutusData.js";
import type { Eq, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import type { TxOut } from "./Tx.js";
import * as LbV2Tx from "./Tx.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/Contexts.hs

/**
 * {@link TxInInfo} is an input of a pending transaction.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/Contexts.hs#L58-L62}
 */
export type TxInInfo = { txInInfoOutRef: TxOutRef; txInInfoResolved: TxOut };

/**
 * {@link Eq} instance for {@link TxInInfo}
 */
export const eqTxInInfo: Eq<TxInInfo> = {
  eq: (l, r) => {
    return LbV1Tx.eqTxOutRef.eq(l.txInInfoOutRef, r.txInInfoOutRef) &&
      LbV2Tx.eqTxOut.eq(l.txInInfoResolved, r.txInInfoResolved);
  },
  neq: (l, r) => {
    return LbV1Tx.eqTxOutRef.neq(l.txInInfoOutRef, r.txInInfoOutRef) ||
      LbV2Tx.eqTxOut.neq(l.txInInfoResolved, r.txInInfoResolved);
  },
};

/**
 * {@link Json} instance for {@link TxInInfo}
 */
export const jsonTxInInfo: Json<TxInInfo> = {
  toJson: (txininfo) => {
    return {
      "reference": LbV1Tx.jsonTxOutRef.toJson(txininfo.txInInfoOutRef),
      "output": LbV2Tx.jsonTxOut.toJson(txininfo.txInInfoResolved),
    };
  },
  fromJson: (value) => {
    const outref = LbPrelude.caseFieldWithValue(
      "reference",
      LbV1Tx.jsonTxOutRef.fromJson,
      value,
    );
    const output = LbPrelude.caseFieldWithValue(
      "output",
      LbV2Tx.jsonTxOut.fromJson,
      value,
    );
    return { txInInfoOutRef: outref, txInInfoResolved: output };
  },
};

/**
 * {@link IsPlutusData} instance for {@link TxInInfo}
 */
export const isPlutusDataTxInInfo: IsPlutusData<TxInInfo> = {
  toData: (txininfo) => {
    return {
      name: "Constr",
      fields: [0n, [
        LbV1Tx.isPlutusDataTxOutRef.toData(txininfo.txInInfoOutRef),
        LbV2Tx.isPlutusDataTxOut.toData(txininfo.txInInfoResolved),
      ]],
    };
  },

  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 2) {
          return {
            txInInfoOutRef: LbV1Tx.isPlutusDataTxOutRef.fromData(
              plutusData.fields[1][0]!,
            ),
            txInInfoResolved: LbV2Tx.isPlutusDataTxOut.fromData(
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
