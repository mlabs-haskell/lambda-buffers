import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { IsPlutusData } from "../PlutusData.js";
import type { Map } from "../AssocMap.js";
import * as AssocMap from "../AssocMap.js";
import type { Eq, Integer, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import type { Maybe } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs
/**
 * {@link CurrencySymbol} represetns the currency. It is empty for Ada or 28 bytes for `MintingPolicyHash`.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L75-L92}
 */

export type CurrencySymbol = LbBytes.LedgerBytes & {
  __compileTimeOnlyCurrencySymbol: CurrencySymbol;
};

/**
 * Checks if the bytes are 28 bytes long.
 */
export function currencySymbolFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<CurrencySymbol> {
  if (bytes.length === 28) {
    return { name: "Just", fields: bytes as CurrencySymbol };
  } else {
    return { name: "Nothing" };
  }
}

/**
 * {@link Eq} instance for {@link CurrencySymbol}
 */
export const eqCurrencySymbol: Eq<CurrencySymbol> = LbBytes.eqLedgerBytes as Eq<
  CurrencySymbol
>;
/**
 * {@link Json} instance for {@link CurrencySymbol}
 */
export const jsonCurrencySymbol: Json<CurrencySymbol> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = currencySymbolFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (bs.name === "Nothing") {
      throw new JsonError("CurrencySymbol should be 28 bytes");
    }
    return bs.fields;
  },
};

/**
 * {@link IsPlutusData} instance for {@link CurrencySymbol}
 */
export const isPlutusDataCurrencySymbol: IsPlutusData<CurrencySymbol> = LbBytes
  .isPlutusDataLedgerBytes as IsPlutusData<unknown> as IsPlutusData<
    CurrencySymbol
  >;

/**
 * {@link TokenName} at most 32 bytes.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L99-L112}
 */
export type TokenName = LbBytes.LedgerBytes & {
  __compileTimeOnlyTokenName: TokenName;
};

/**
 * {@link tokenNameFromBytes} checks if the bytes are at most 32 bytes long.
 */
export function tokenNameFromBytes(
  bytes: LbBytes.LedgerBytes,
): Maybe<TokenName> {
  if (bytes.length <= 32) {
    return { name: "Just", fields: bytes as TokenName };
  } else {
    return { name: "Nothing" };
  }
}

/**
 * {@link Eq} instance for {@link TokenName}
 */
export const eqTokenName: Eq<TokenName> = LbBytes.eqLedgerBytes as Eq<
  TokenName
>;

/**
 * {@link Json} instance for {@link TokenName}
 */
export const jsonTokenName: Json<TokenName> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = tokenNameFromBytes(LbBytes.jsonLedgerBytes.fromJson(value));
    if (bs.name === "Nothing") {
      throw new JsonError("TokenName should be at most 32 bytes");
    }
    return bs.fields;
  },
};
/**
 * {@link IsPlutusData} instance for {@link TokenName}
 */
export const isPlutusDataTokenName: IsPlutusData<TokenName> = LbBytes
  .isPlutusDataLedgerBytes as IsPlutusData<unknown> as IsPlutusData<TokenName>;

/**
 * An asset class identified by a {@link CurrencySymbol} and {@link TokenName}
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L157-L162}
 */
export type AssetClass = [CurrencySymbol, TokenName];

/**
 * {@link Eq} instance for {@link AssetClass}
 */
export const eqAssetClass: Eq<AssetClass> = LbPrelude.eqPair(
  eqCurrencySymbol,
  eqTokenName,
);

/**
 * {@link Json} instance for {@link AssetClass}
 */
export const jsonAssetClass: Json<AssetClass> = {
  toJson: (assetClass) => {
    return {
      "currency_symbol": jsonCurrencySymbol.toJson(assetClass[0]),
      "token_name": jsonTokenName.toJson(assetClass[1]),
    };
  },
  fromJson: (value) => {
    const currencySymbol = LbPrelude.caseFieldWithValue(
      "currency_symbol",
      jsonCurrencySymbol.fromJson,
      value,
    );
    const tokenName = LbPrelude.caseFieldWithValue(
      "token_name",
      jsonTokenName.fromJson,
      value,
    );
    return [currencySymbol, tokenName];
  },
};
/**
 * {@link IsPlutusData} instance for {@link AssetClass}
 */
export const isPlutusDataAssetClass: IsPlutusData<AssetClass> =
  LbPreludeInstances
    .isPlutusDataPairWithTag(isPlutusDataCurrencySymbol, isPlutusDataTokenName);

/**
 * {@link Value} represents a collection of amounts of different currencies.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L176-L201}
 */
export type Value = Map<CurrencySymbol, Map<TokenName, Integer>>;

/**
 * {@link Eq} instance for {@link Value}
 */
export const eqValue: Eq<Value> = AssocMap.eqMap(
  eqCurrencySymbol,
  AssocMap.eqMap(eqTokenName, LbPrelude.eqInteger),
);
/**
 * {@link Json} instance for {@link Value}
 */
export const jsonValue: Json<Value> = AssocMap.jsonMap(
  jsonCurrencySymbol,
  AssocMap.jsonMap(jsonTokenName, LbPrelude.jsonInteger),
);

/**
 * {@link IsPlutusData} instance for {@link Value}
 */
export const isPlutusDataValue: IsPlutusData<Value> = AssocMap.isPlutusDataMap(
  isPlutusDataCurrencySymbol,
  AssocMap.isPlutusDataMap(
    isPlutusDataTokenName,
    LbPreludeInstances.isPlutusDataInteger,
  ),
);

/**
 * The currency symbol of ada
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L147-L150}
 */
export const adaSymbol: CurrencySymbol = Uint8Array.from([]) as CurrencySymbol;

/**
 * The token name of ada
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L152-L155}
 */
export const adaToken: TokenName = Uint8Array.from([]) as TokenName;
