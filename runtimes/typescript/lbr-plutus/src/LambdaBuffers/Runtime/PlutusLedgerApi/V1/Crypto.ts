import * as LbBytes from "./Bytes.js";
import type { Eq, Json } from "lbr-prelude";
import type { FromData, ToData } from "../../PlutusData.js";

/**
 * {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Crypto.hs}
 */

// -- PlutusLedgerApi.V1.Crypto
// opaque PubKeyHash
//
// instance PlutusData PubKeyHash
// instance Eq PubKeyHash
// instance Json PubKeyHash
export type PubKeyHash = LbBytes.LedgerBytes;

export const eqPubKeyHash: Eq<PubKeyHash> = LbBytes.eqLedgerBytes;
export const jsonPubKeyHash: Json<PubKeyHash> = LbBytes.jsonLedgerBytes;
export const toDataPubKeyHash: ToData<PubKeyHash> = LbBytes.toDataLedgerBytes;
export const fromDataPubKeyHash: FromData<PubKeyHash> =
  LbBytes.fromDataLedgerBytes;
