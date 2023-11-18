import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { FromData, ToData } from "../../PlutusData.js";
import type { Map } from "../../AssocMap.js";
import * as AssocMap from "../../AssocMap.js";
import type { Bytes, Eq, Integer, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";
import * as LbBytes from "./Bytes.js";

/**
 * https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs
 */

// -- PlutusLedgerApi.V1.Value
// opaque CurrencySymbol
//
// instance PlutusData CurrencySymbol
// instance Eq CurrencySymbol
// instance Json CurrencySymbol

export type CurrencySymbol = Bytes;

export const eqCurrencySymbol: Eq<CurrencySymbol> = LbBytes.eqLedgerBytes;
export const jsonCurrencySymbol: Json<CurrencySymbol> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = LbBytes.jsonLedgerBytes.fromJson(value);
    if (bs.length !== 28) {
      throw new JsonError("Expected 28 bytes");
    }
    return bs;
  },
};
export const toDataCurrencySymbol: ToData<CurrencySymbol> =
  LbBytes.toDataLedgerBytes;
export const fromDataCurrencySymbol: FromData<CurrencySymbol> =
  LbBytes.fromDataLedgerBytes;

// opaque TokenName
//
// instance PlutusData TokenName
// instance Eq TokenName
// instance Json TokenName

export type TokenName = Bytes;

export const eqTokenName: Eq<TokenName> = LbBytes.eqLedgerBytes;
export const jsonTokenName: Json<TokenName> = {
  toJson: LbBytes.jsonLedgerBytes.toJson,
  fromJson: (value) => {
    const bs = LbBytes.jsonLedgerBytes.fromJson(value);
    if (!(bs.length <= 32)) {
      throw new JsonError("Expected 28 bytes");
    }
    return bs;
  },
};
export const toDataTokenName: ToData<TokenName> = LbBytes.toDataLedgerBytes;
export const fromDataTokenName: FromData<TokenName> =
  LbBytes.fromDataLedgerBytes;

// opaque AssetClass
//
// instance PlutusData AssetClass
// instance Eq AssetClass
// instance Json AssetClass
export type AssetClass = [CurrencySymbol, TokenName];

export const eqAssetClass: Eq<AssetClass> = LbPrelude.eqPair(
  eqCurrencySymbol,
  eqTokenName,
);
export const jsonAssetClass: Json<AssetClass> = {
  toJson: (assetClass) => {
    return {
      "currency_symbol": jsonCurrencySymbol.toJson(assetClass[0]),
      "token_name": jsonCurrencySymbol.toJson(assetClass[1]),
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
export const toDataAssetClass: ToData<AssetClass> = LbPreludeInstances
  .toDataPairWithTag(toDataCurrencySymbol, toDataTokenName);
export const fromDataAssetClass: FromData<AssetClass> = LbPreludeInstances
  .fromDataPairWithTag(fromDataCurrencySymbol, fromDataTokenName);

// opaque Value
//
// instance PlutusData Value
// instance Eq Value
// instance Json Value

export type Value = Map<CurrencySymbol, Map<TokenName, Integer>>;

export const eqValue: Eq<Value> = AssocMap.eqMap(
  eqCurrencySymbol,
  AssocMap.eqMap(eqTokenName, LbPrelude.eqInteger),
);
export const jsonValue: Json<Value> = AssocMap.jsonMap(
  jsonCurrencySymbol,
  AssocMap.jsonMap(jsonTokenName, LbPrelude.jsonInteger),
);
export const toDataValue: ToData<Value> = AssocMap.toDataMap(
  toDataCurrencySymbol,
  AssocMap.toDataMap(toDataTokenName, LbPreludeInstances.toDataInteger),
);
export const fromDataValue: FromData<Value> = AssocMap.fromDataMap(
  fromDataCurrencySymbol,
  AssocMap.fromDataMap(fromDataTokenName, LbPreludeInstances.fromDataInteger),
);
