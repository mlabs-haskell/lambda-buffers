import * as LbPlutusData from "../../PlutusData.js";
import type { FromData, PlutusData, ToData } from "../../PlutusData.js";
import type { Bytes, Eq, Json } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

/**
 * https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs
 */

// -- PlutusLedgerApi.V1.Scripts
// opaque Redeemer
//
// instance PlutusData Redeemer
// instance Eq Redeemer
// instance Json Redeemer
export type Redeemer = PlutusData;

export const eqRedeemer: Eq<Redeemer> = LbPlutusData.eqPlutusData;
export const jsonRedeemer: Json<Redeemer> = LbPlutusData.jsonPlutusData;
export const toDataRedeemer: ToData<Redeemer> = LbPlutusData.toDataPlutusData;
export const fromDataRedeemer: FromData<Redeemer> =
  LbPlutusData.fromDataPlutusData;

//
// opaque Datum
//
// instance PlutusData Datum
// instance Eq Datum
// instance Json Datum

export type Datum = PlutusData;

export const eqDatum: Eq<Datum> = LbPlutusData.eqPlutusData;
export const jsonDatum: Json<Datum> = LbPlutusData.jsonPlutusData;
export const toDataDatum: ToData<Datum> = LbPlutusData.toDataPlutusData;
export const fromDataDatum: FromData<Datum> = LbPlutusData.fromDataPlutusData;

// opaque DatumHash
//
// instance PlutusData DatumHash
// instance Eq DatumHash
// instance Json DatumHash
export type DatumHash = Bytes;

export const eqDatumHash: Eq<DatumHash> = LbBytes.eqLedgerBytes;
export const jsonDatumHash: Json<DatumHash> = LbBytes.jsonLedgerBytes;
export const toDataDatumHash: ToData<DatumHash> = LbBytes.toDataLedgerBytes;
export const fromDataDatumHash: FromData<DatumHash> =
  LbBytes.fromDataLedgerBytes;

// opaque RedeemerHash
//
// instance PlutusData RedeemerHash
// instance Eq RedeemerHash
// instance Json RedeemerHash

export type RedeemerHash = Bytes;

export const eqRedeemerHash: Eq<RedeemerHash> = LbBytes.eqLedgerBytes;
export const jsonRedeemerHash: Json<RedeemerHash> = LbBytes.jsonLedgerBytes;
export const toDataRedeemerHash: ToData<RedeemerHash> =
  LbBytes.toDataLedgerBytes;
export const fromDataRedeemerHash: FromData<RedeemerHash> =
  LbBytes.fromDataLedgerBytes;

// opaque ScriptHash
//
// instance PlutusData ScriptHash
// instance Eq ScriptHash
// instance Json ScriptHash

export type ScriptHash = Bytes;

export const eqScriptHash: Eq<ScriptHash> = LbBytes.eqLedgerBytes;
export const jsonScriptHash: Json<ScriptHash> = LbBytes.jsonLedgerBytes;
export const toDataScriptHash: ToData<ScriptHash> = LbBytes.toDataLedgerBytes;
export const fromDataScriptHash: FromData<ScriptHash> =
  LbBytes.fromDataLedgerBytes;
