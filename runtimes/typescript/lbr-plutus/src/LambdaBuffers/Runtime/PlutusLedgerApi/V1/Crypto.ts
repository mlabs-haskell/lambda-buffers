import * as LbBytes from "./Bytes.js";
import type { Eq, Json } from "lbr-prelude";
import { JsonError } from "lbr-prelude";
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
export type PubKeyHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyPubKeyHash: PubKeyHash;
};

/**
 * Checks if the bytes are 28 bytes long.
 */
export function pubKeyHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): PubKeyHash | undefined {
  if (bytes.length === 28) {
    return bytes as PubKeyHash;
  } else {
    return undefined;
  }
}

/**
 * Alias for the identity function
 */
export function pubKeyHashToBytes(pubkeyhash: PubKeyHash): LbBytes.LedgerBytes {
  return pubkeyhash;
}

export const eqPubKeyHash: Eq<PubKeyHash> = LbBytes.eqLedgerBytes as Eq<
  PubKeyHash
>;
export const jsonPubKeyHash: Json<PubKeyHash> = {
  toJson: (pubkeyhash) => {
    return LbBytes.jsonLedgerBytes.toJson(pubKeyHashToBytes(pubkeyhash));
  },
  fromJson: (value) => {
    const res = pubKeyHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res === undefined) {
      throw new JsonError("PubKeyHash should be 28 bytes");
    }
    return res;
  },
};
export const toDataPubKeyHash: ToData<PubKeyHash> = LbBytes
  .toDataLedgerBytes as ToData<PubKeyHash>;
export const fromDataPubKeyHash: FromData<PubKeyHash> = LbBytes
  .fromDataLedgerBytes as FromData<PubKeyHash>;
