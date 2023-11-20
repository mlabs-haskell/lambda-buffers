import * as LbPlutusData from "../../PlutusData.js";
import type { FromData, PlutusData, ToData } from "../../PlutusData.js";
import type { Eq, Json } from "lbr-prelude";
import { JsonError } from "lbr-prelude";
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
export type DatumHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyDatumHash: DatumHash;
};

/**
 * Checks if the bytes are 32 bytes long.
 */
export function datumHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): DatumHash | undefined {
  if (bytes.length === 32) {
    return bytes as DatumHash;
  } else {
    return undefined;
  }
}

/**
 * Alias for the identity function
 */
export function datumHashToBytes(datumhash: DatumHash): LbBytes.LedgerBytes {
  return datumhash;
}

export const eqDatumHash: Eq<DatumHash> = LbBytes.eqLedgerBytes as Eq<
  DatumHash
>;
export const jsonDatumHash: Json<DatumHash> = {
  toJson: (datumhash) => {
    return LbBytes.jsonLedgerBytes.toJson(datumHashToBytes(datumhash));
  },
  fromJson: (value) => {
    const res = datumHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res === undefined) {
      throw new JsonError("DatumHash should be 32 bytes long");
    }
    return res;
  },
};
export const toDataDatumHash: ToData<DatumHash> = LbBytes
  .toDataLedgerBytes as ToData<DatumHash>;
export const fromDataDatumHash: FromData<DatumHash> = LbBytes
  .fromDataLedgerBytes as FromData<DatumHash>;

// opaque RedeemerHash
//
// instance PlutusData RedeemerHash
// instance Eq RedeemerHash
// instance Json RedeemerHash

export type RedeemerHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyRedeemerHash: RedeemerHash;
};

/**
 * Checks if the bytes are 32 bytes long.
 */
export function redeemerHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): RedeemerHash | undefined {
  if (bytes.length === 32) {
    return bytes as RedeemerHash;
  } else {
    return undefined;
  }
}

/**
 * Alias for the identity function
 */
export function redeemerHashToBytes(
  redeemerhash: RedeemerHash,
): LbBytes.LedgerBytes {
  return redeemerhash;
}

export const eqRedeemerHash: Eq<RedeemerHash> = LbBytes.eqLedgerBytes as Eq<
  RedeemerHash
>;
export const jsonRedeemerHash: Json<RedeemerHash> = {
  toJson: (redeemerhash) => {
    return LbBytes.jsonLedgerBytes.toJson(redeemerHashToBytes(redeemerhash));
  },
  fromJson: (value) => {
    const res = redeemerHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res === undefined) {
      throw new JsonError("RedeemerHash should be 32 bytes long");
    }
    return res;
  },
};
export const toDataRedeemerHash: ToData<RedeemerHash> = LbBytes
  .toDataLedgerBytes as ToData<RedeemerHash>;
export const fromDataRedeemerHash: FromData<RedeemerHash> = LbBytes
  .fromDataLedgerBytes as FromData<RedeemerHash>;

// opaque ScriptHash
//
// instance PlutusData ScriptHash
// instance Eq ScriptHash
// instance Json ScriptHash

export type ScriptHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyScriptHash: ScriptHash;
};

/**
 * Checks if the bytes are 28 bytes long.
 */
export function scriptHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): ScriptHash | undefined {
  if (bytes.length === 28) {
    return bytes as ScriptHash;
  } else {
    return undefined;
  }
}

/**
 * Alias for the identity function
 */
export function scriptHashToBytes(scripthash: ScriptHash): LbBytes.LedgerBytes {
  return scripthash;
}

export const eqScriptHash: Eq<ScriptHash> = LbBytes.eqLedgerBytes as Eq<
  ScriptHash
>;
export const jsonScriptHash: Json<ScriptHash> = {
  toJson: (scripthash) => {
    return LbBytes.jsonLedgerBytes.toJson(scriptHashToBytes(scripthash));
  },
  fromJson: (value) => {
    const res = scriptHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res === undefined) {
      throw new JsonError("ScriptHash should be 28 bytes long");
    }
    return res;
  },
};
export const toDataScriptHash: ToData<ScriptHash> = LbBytes
  .toDataLedgerBytes as ToData<ScriptHash>;
export const fromDataScriptHash: FromData<ScriptHash> = LbBytes
  .fromDataLedgerBytes as FromData<ScriptHash>;
