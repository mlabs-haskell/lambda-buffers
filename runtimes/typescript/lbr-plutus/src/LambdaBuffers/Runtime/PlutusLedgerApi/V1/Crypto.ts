import * as LbBytes from "./Bytes.js";
import type { Eq, Json, Maybe } from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import type { IsPlutusData } from "../../PlutusData.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Crypto.hs

/**
 * {@link PubKeyHash}
 *
 * @see {@link  https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Crypto.hs#L22-L44 }
 */
export type PubKeyHash = LbBytes.LedgerBytes & {
  __compileTimeOnlyPubKeyHash: PubKeyHash;
};

/**
 * {@link pubKeyHashFromBytes} verifies that the provided bytes are 28 bytes
 * long.
 */
export function pubKeyHashFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<PubKeyHash> {
  if (bytes.length === 28) {
    return { name: "Just", fields: bytes as PubKeyHash };
  } else {
    return { name: "Nothing" };
  }
}

/**
 * {@link Eq} instance for {@link PubKeyHash}
 */
export const eqPubKeyHash: Eq<PubKeyHash> = LbBytes.eqLedgerBytes as Eq<
  PubKeyHash
>;

/**
 * {@link Json} instance for {@link PubKeyHash}
 */
export const jsonPubKeyHash: Json<PubKeyHash> = {
  toJson: (pubkeyhash) => {
    return LbBytes.jsonLedgerBytes.toJson(pubkeyhash);
  },
  fromJson: (value) => {
    const res = pubKeyHashFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (res.name === "Nothing") {
      throw new JsonError("PubKeyHash should be 28 bytes");
    }
    return res.fields;
  },
};

/**
 * {@link IsPlutusData} instance for {@link PubKeyHash}
 *
 * @remarks
 * `fromData` does _not_ do any length checks.
 */
export const isPlutusDataPubKeyHash: IsPlutusData<PubKeyHash> = LbBytes
  .isPlutusDataLedgerBytes as IsPlutusData<unknown> as IsPlutusData<PubKeyHash>;
