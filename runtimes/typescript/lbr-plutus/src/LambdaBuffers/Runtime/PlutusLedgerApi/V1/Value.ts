import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { FromData, ToData } from "../../PlutusData.js";
import type { Map } from "../../AssocMap.js";
import * as AssocMap from "../../AssocMap.js";
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
    return bytes as CurrencySymbol;
  } else {
    return undefined;
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
    if (bs === undefined) {
      throw new JsonError("CurrencySymbol should be 28 bytes");
    }
    return bs;
  },
};

/**
 * {@link ToData} instance for {@link CurrencySymbol}
 */
export const toDataCurrencySymbol: ToData<CurrencySymbol> = LbBytes
  .toDataLedgerBytes as ToData<CurrencySymbol>;
/**
 * {@link FromData} instance for {@link CurrencySymbol}
 */
export const fromDataCurrencySymbol: FromData<CurrencySymbol> = LbBytes
  .fromDataLedgerBytes as FromData<CurrencySymbol>;

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
    return bytes as TokenName;
  } else {
    return undefined;
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
    if (bs === undefined) {
      throw new JsonError("TokenName should be at most 32 bytes");
    }
    return bs;
  },
};
/**
 * {@link ToData} instance for {@link TokenName}
 */
export const toDataTokenName: ToData<TokenName> = LbBytes
  .toDataLedgerBytes as ToData<TokenName>;
/**
 * {@link FromData} instance for {@link TokenName}
 */
export const fromDataTokenName: FromData<TokenName> = LbBytes
  .fromDataLedgerBytes as FromData<TokenName>;

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
 * {@link ToData} instance for {@link AssetClass}
 */
export const toDataAssetClass: ToData<AssetClass> = LbPreludeInstances
  .toDataPairWithTag(toDataCurrencySymbol, toDataTokenName);
/**
 * {@link FromData} instance for {@link AssetClass}
 */
export const fromDataAssetClass: FromData<AssetClass> = LbPreludeInstances
  .fromDataPairWithTag(fromDataCurrencySymbol, fromDataTokenName);

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
 * {@link ToData} instance for {@link Value}
 */
export const toDataValue: ToData<Value> = AssocMap.toDataMap(
  toDataCurrencySymbol,
  AssocMap.toDataMap(toDataTokenName, LbPreludeInstances.toDataInteger),
);

/**
 * {@link FromData} instance for {@link Value}
 */
export const fromDataValue: FromData<Value> = AssocMap.fromDataMap(
  fromDataCurrencySymbol,
  AssocMap.fromDataMap(fromDataTokenName, LbPreludeInstances.fromDataInteger),
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
