import type { Datum, DatumHash, ScriptHash } from "../V1/Scripts.js";
import * as LbScripts from "../V1/Scripts.js";
import type { Eq, Json, Maybe } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import type { FromData, ToData } from "../../PlutusData.js";
import { FromDataError } from "../../PlutusData.js";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { Address } from "../V1/Address.js";
import * as LbAddress from "../V1/Address.js";
import type { Value } from "../V1/Value.js";
import * as LbValue from "../V1/Value.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs

/**
 * {@link OutputDatum} the datum attached to an output.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L51-L54}
 */
export type OutputDatum =
  | { name: "NoOutputDatum" }
  | { name: "OutputDatumHash"; fields: DatumHash }
  | { name: "OutputDatum"; fields: Datum };

/**
 * {@link Eq} instance for {@link OutputDatum}
 */
export const eqOutputDatum: Eq<OutputDatum> = {
  eq: (l, r) => {
    if (l.name === "NoOutputDatum" && r.name === "NoOutputDatum") {
      return true;
    } else if (l.name === "OutputDatumHash" && r.name === "OutputDatumHash") {
      return LbScripts.eqDatumHash.eq(l.fields, r.fields);
    } else if (l.name === "OutputDatum" && r.name === "OutputDatum") {
      return LbScripts.eqDatum.eq(l.fields, r.fields);
    } else {
      return false;
    }
  },
  neq: (l, r) => {
    if (l.name === "NoOutputDatum" && r.name === "NoOutputDatum") {
      return false;
    } else if (l.name === "OutputDatumHash" && r.name === "OutputDatumHash") {
      return LbScripts.eqDatumHash.neq(l.fields, r.fields);
    } else if (l.name === "OutputDatum" && r.name === "OutputDatum") {
      return LbScripts.eqDatum.neq(l.fields, r.fields);
    } else {
      return true;
    }
  },
};

/**
 * {@link Json} instance for {@link OutputDatum}
 */
export const jsonOutputDatum: Json<OutputDatum> = {
  toJson: (outputDatum) => {
    switch (outputDatum.name) {
      case "NoOutputDatum":
        return LbPrelude.jsonConstructor(outputDatum.name, []);
      case "OutputDatumHash":
        return LbPrelude.jsonConstructor(outputDatum.name, [
          LbScripts.jsonDatumHash.toJson(outputDatum.fields),
        ]);
      case "OutputDatum":
        return LbPrelude.jsonConstructor(outputDatum.name, [
          LbScripts.jsonDatum.toJson(outputDatum.fields),
        ]);
    }
  },
  fromJson: (value) => {
    return LbPrelude.caseJsonConstructor<OutputDatum>("Plutus.V2.OutputDatum", {
      "NoOutputDatum": (ctorfields) => {
        if (ctorfields.length !== 0) {
          throw new JsonError("Expected 0 fields");
        }
        return { name: "NoOutputDatum" };
      },
      "OutputDatumHash": (ctorfields) => {
        if (ctorfields.length !== 1) {
          throw new JsonError("Expected 1 field");
        }
        return {
          name: "OutputDatumHash",
          fields: LbScripts.jsonDatumHash.fromJson(ctorfields[0]!),
        };
      },
      "OutputDatum": (ctorfields) => {
        if (ctorfields.length !== 1) {
          throw new JsonError("Expected 1 field");
        }
        return {
          name: "OutputDatum",
          fields: LbScripts.jsonDatum.fromJson(ctorfields[0]!),
        };
      },
    }, value);
  },
};

/**
 * {@link ToData} instance for {@link OutputDatum}
 */
export const toDataOutputDatum: ToData<OutputDatum> = {
  toData: (outputDatum) => {
    switch (outputDatum.name) {
      case "NoOutputDatum":
        return { name: "Constr", fields: [0n, []] };
      case "OutputDatumHash":
        return {
          name: "Constr",
          fields: [1n, [LbScripts.toDataDatumHash.toData(outputDatum.fields)]],
        };
      case "OutputDatum":
        return {
          name: "Constr",
          fields: [2n, [LbScripts.toDataDatum.toData(outputDatum.fields)]],
        };
    }
  },
};

/**
 * {@link FromData} instance for {@link OutputDatum}
 */
export const fromDataOutputDatum: FromData<OutputDatum> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 0) {
          return { name: "NoOutputDatum" };
        } else if (
          plutusData.fields[0] === 1n && plutusData.fields[1].length === 1
        ) {
          return {
            name: "OutputDatumHash",
            fields: LbScripts.fromDataDatumHash.fromData(
              plutusData.fields[1][0]!,
            ),
          };
        } else if (
          plutusData.fields[0] === 2n && plutusData.fields[1].length === 1
        ) {
          return {
            name: "OutputDatum",
            fields: LbScripts.fromDataDatum.fromData(plutusData.fields[1][0]!),
          };
        }
        break;
      default:
        break;
    }
    throw new FromDataError("Unexpected data");
  },
};

/**
 * {@link TxOut} a transaction output.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L68-L77}
 */
export type TxOut = {
  txOutAddress: Address;
  txOutValue: Value;
  txOutDatum: OutputDatum;
  txOutReferenceScript: Maybe<ScriptHash>;
};

/**
 * {@link Eq} instance for {@link TxOut}
 */
export const eqTxOut: Eq<TxOut> = {
  eq: (l, r) => {
    return LbAddress.eqAddress.eq(l.txOutAddress, r.txOutAddress) &&
      LbValue.eqValue.eq(l.txOutValue, r.txOutValue) &&
      eqOutputDatum.eq(l.txOutDatum, r.txOutDatum) &&
      LbPrelude.eqMaybe(LbScripts.eqScriptHash).eq(
        l.txOutReferenceScript,
        r.txOutReferenceScript,
      );
  },
  neq: (l, r) => {
    return LbAddress.eqAddress.neq(l.txOutAddress, r.txOutAddress) ||
      LbValue.eqValue.neq(l.txOutValue, r.txOutValue) ||
      eqOutputDatum.neq(l.txOutDatum, r.txOutDatum) ||
      LbPrelude.eqMaybe(LbScripts.eqScriptHash).neq(
        l.txOutReferenceScript,
        r.txOutReferenceScript,
      );
  },
};

/**
 * {@link Json} instance for {@link TxOut}
 */
export const jsonTxOut: Json<TxOut> = {
  toJson: (txout) => {
    return {
      "address": LbAddress.jsonAddress.toJson(txout.txOutAddress),
      "value": LbValue.jsonValue.toJson(txout.txOutValue),
      "datum": jsonOutputDatum.toJson(txout.txOutDatum),
      "reference_script": LbPrelude.jsonMaybe(LbScripts.jsonScriptHash).toJson(
        txout.txOutReferenceScript,
      ),
    };
  },
  fromJson: (value) => {
    const address = LbPrelude.caseFieldWithValue(
      "address",
      LbAddress.jsonAddress.fromJson,
      value,
    );
    const ovalue = LbPrelude.caseFieldWithValue(
      "value",
      LbValue.jsonValue.fromJson,
      value,
    );
    const outputdatum = LbPrelude.caseFieldWithValue(
      "datum",
      jsonOutputDatum.fromJson,
      value,
    );
    const refscript = LbPrelude.caseFieldWithValue(
      "reference_script",
      LbPrelude.jsonMaybe(LbScripts.jsonScriptHash).fromJson,
      value,
    );

    return {
      txOutAddress: address,
      txOutValue: ovalue,
      txOutDatum: outputdatum,
      txOutReferenceScript: refscript,
    };
  },
};

/**
 * {@link ToData} instance for {@link TxOut}
 */
export const toDataTxOut: ToData<TxOut> = {
  toData: (txout) => {
    return {
      name: "Constr",
      fields: [0n, [
        LbAddress.toDataAddress.toData(txout.txOutAddress),
        LbValue.toDataValue.toData(txout.txOutValue),
        toDataOutputDatum.toData(txout.txOutDatum),
        LbPreludeInstances.toDataMaybe(LbScripts.toDataScriptHash).toData(
          txout.txOutReferenceScript,
        ),
      ]],
    };
  },
};

/**
 * {@link FromData} instance for {@link TxOut}
 */
export const fromDataTxOut: FromData<TxOut> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr":
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 4) {
          return {
            txOutAddress: LbAddress.fromDataAddress.fromData(
              plutusData.fields[1][0]!,
            ),
            txOutValue: LbValue.fromDataValue.fromData(
              plutusData.fields[1][1]!,
            ),
            txOutDatum: fromDataOutputDatum.fromData(plutusData.fields[1][2]!),
            txOutReferenceScript: LbPreludeInstances.fromDataMaybe(
              LbScripts.fromDataScriptHash,
            ).fromData(plutusData.fields[1][3]!),
          };
        }
        break;
      default:
        break;
    }
    throw new FromDataError("Unexpected data");
  },
};
