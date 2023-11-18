import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import { FromDataError } from "../../PlutusData.js";
import type { FromData, ToData } from "../../PlutusData.js";
import type { Bytes, Json } from "lbr-prelude";
import { Buffer } from "node:buffer";

/**
 * `Bytes`
 * {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Bytes.hs}
 */

// -- PlutusLedgerApi.V1.Bytes
// -- TODO(bladyjoker): We don't need this, use Prelude.Bytes? Json encoding is different though, base16 vs base64. But, you need base16 for hashes, which is covered regardless. So yeah, remove this and use Prelude.Bytes.
// opaque Bytes
//
// instance PlutusData Bytes
// instance Eq Bytes
// instance Json Bytes
export type LedgerBytes = Bytes;

export const eqLedgerBytes = LbPrelude.eqBytes;

/**
 * `Json` instance for `LedgerBytes` encodes / decodes the bytes as a base16
 * (hexadecimal) string.
 */
export const jsonLedgerBytes: Json<LedgerBytes> = {
  toJson: (bytes) => Buffer.from(bytes).toString("hex"),
  fromJson: (value) => {
    if (typeof value === "string") {
      return Buffer.from(value, "hex");
    } else {
      throw new JsonError("JSON Value is not a string");
    }
  },
};

export const toDataLedgerBytes: ToData<LedgerBytes> = {
  toData: (bytes) => {
    return { name: "Bytes", fields: bytes };
  },
};

export const fromDataLedgerBytes: FromData<LedgerBytes> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Bytes":
        return plutusData.fields;
      default:
        throw new FromDataError("Expected bytes but got " + plutusData);
    }
  },
};
