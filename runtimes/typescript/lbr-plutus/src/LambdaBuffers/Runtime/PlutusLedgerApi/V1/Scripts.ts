import * as LbPlutusData from "../../PlutusData.js";
import type { FromData, PlutusData, ToData } from "../../PlutusData.js";
import type { Eq, Json, Maybe } from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs

/**
 * {@link Redeemer} is wrapper around {@link PlutusData} values that are used as redeemers in transaction inputs.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs#L67-L71}
 */
export type Redeemer = PlutusData;

export const eqRedeemer: Eq<Redeemer> = LbPlutusData.eqPlutusData;
export const jsonRedeemer: Json<Redeemer> = LbPlutusData.jsonPlutusData;
export const toDataRedeemer: ToData<Redeemer> = LbPlutusData.toDataPlutusData;
export const fromDataRedeemer: FromData<Redeemer> =
  LbPlutusData.fromDataPlutusData;

/**
 * {@link Datum} is wrapper around {@link PlutusData} values that are used as redeemers in transaction inputs.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs#L56-L60 }
 */
export type Datum = PlutusData;

export const eqDatum: Eq<Datum> = LbPlutusData.eqPlutusData;
export const jsonDatum: Json<Datum> = LbPlutusData.jsonPlutusData;
export const toDataDatum: ToData<Datum> = LbPlutusData.toDataPlutusData;
export const fromDataDatum: FromData<Datum> = LbPlutusData.fromDataPlutusData;

/**
 * {@link DatumHash} represents a hash of a datum. 32 bytes.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs#L94-L108 }
 */
export type DatumHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyDatumHash: DatumHash;
};

/**
 * {@link datumHashFromBytes} checks if the provided bytes are 32 bytes long.
 */
export function datumHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<DatumHash> {
  if (bytes.length === 32) {
    return { name: "Just", fields: bytes as DatumHash };
  } else {
    return { name: "Nothing" };
  }
}

export const eqDatumHash: Eq<DatumHash> = LbBytes.eqLedgerBytes as Eq<
  DatumHash
>;
export const jsonDatumHash: Json<DatumHash> = {
  toJson: (datumhash) => {
    return LbBytes.jsonLedgerBytes.toJson(datumhash);
  },
  fromJson: (value) => {
    const res = datumHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res.name === "Nothing") {
      throw new JsonError("DatumHash should be 32 bytes long");
    }
    return res.fields;
  },
};
export const toDataDatumHash: ToData<DatumHash> = LbBytes
  .toDataLedgerBytes as ToData<DatumHash>;
export const fromDataDatumHash: FromData<DatumHash> = LbBytes
  .fromDataLedgerBytes as FromData<DatumHash>;

/**
 * {@link RedeemerHash} represents the hash of a redeemer. 32 bytes.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs#L110-L125}
 */
export type RedeemerHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyRedeemerHash: RedeemerHash;
};

/**
 * {@link redeemerHashFromBytes } checks if the provided bytes are 32 bytes long.
 */
export function redeemerHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<RedeemerHash> {
  if (bytes.length === 32) {
    return { name: "Just", fields: bytes as RedeemerHash };
  } else {
    return { name: "Nothing" };
  }
}

export const eqRedeemerHash: Eq<RedeemerHash> = LbBytes.eqLedgerBytes as Eq<
  RedeemerHash
>;
export const jsonRedeemerHash: Json<RedeemerHash> = {
  toJson: (redeemerhash) => {
    return LbBytes.jsonLedgerBytes.toJson(redeemerhash);
  },
  fromJson: (value) => {
    const res = redeemerHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res.name === "Nothing") {
      throw new JsonError("RedeemerHash should be 32 bytes long");
    }
    return res.fields;
  },
};
export const toDataRedeemerHash: ToData<RedeemerHash> = LbBytes
  .toDataLedgerBytes as ToData<RedeemerHash>;
export const fromDataRedeemerHash: FromData<RedeemerHash> = LbBytes
  .fromDataLedgerBytes as FromData<RedeemerHash>;

/**
 * {@link ScriptHash} is the hash of a script. 28 bytes.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Scripts.hs#L78-L92 }
 */
export type ScriptHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyScriptHash: ScriptHash;
};

/**
 * {@link scriptHashFromBytes } checks if the provided bytes are 28 bytes long.
 */
export function scriptHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<ScriptHash> {
  if (bytes.length === 28) {
    return { name: "Just", fields: bytes as ScriptHash };
  } else {
    return { name: "Nothing" };
  }
}

export const eqScriptHash: Eq<ScriptHash> = LbBytes.eqLedgerBytes as Eq<
  ScriptHash
>;
export const jsonScriptHash: Json<ScriptHash> = {
  toJson: (scripthash) => {
    return LbBytes.jsonLedgerBytes.toJson(scripthash);
  },
  fromJson: (value) => {
    const res = scriptHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res.name === "Nothing") {
      throw new JsonError("ScriptHash should be 28 bytes long");
    }
    return res.fields;
  },
};
export const toDataScriptHash: ToData<ScriptHash> = LbBytes
  .toDataLedgerBytes as ToData<ScriptHash>;
export const fromDataScriptHash: FromData<ScriptHash> = LbBytes
  .fromDataLedgerBytes as FromData<ScriptHash>;
