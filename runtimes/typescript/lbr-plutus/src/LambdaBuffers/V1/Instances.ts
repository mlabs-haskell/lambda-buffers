import * as PlutusData from "../PlutusData.js";
import * as PlutusLedgerApiPlutusData from "plutus-ledger-api/PlutusData.js";
import * as PlutusLedgerApiV1 from "plutus-ledger-api/V1.js";
import * as LbrPrelude from "lbr-prelude";
import * as Prelude from "prelude";
import * as Symbols from "./Symbols.js";

// Address
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Address]: Prelude.Eq<PlutusLedgerApiV1.Address>;
  }

  export interface JsonInstances {
    [Symbols.Address]: Prelude.Json<PlutusLedgerApiV1.Address>;
  }
}

LbrPrelude.Eq[Symbols.Address] = PlutusLedgerApiV1.eqAddress;
LbrPrelude.Json[Symbols.Address] = PlutusLedgerApiV1.jsonAddress;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Address]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Address
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Address] =
  PlutusLedgerApiV1.isPlutusDataAddress;

// LedgerBytes
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.LedgerBytes]: Prelude.Eq<PlutusLedgerApiV1.LedgerBytes>;
  }

  export interface JsonInstances {
    [Symbols.LedgerBytes]: Prelude.Json<PlutusLedgerApiV1.LedgerBytes>;
  }
}

LbrPrelude.Eq[Symbols.LedgerBytes] = PlutusLedgerApiV1.eqLedgerBytes;
LbrPrelude.Json[Symbols.LedgerBytes] = PlutusLedgerApiV1.jsonLedgerBytes;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.LedgerBytes]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.LedgerBytes
    >;
  }
}
PlutusData.IsPlutusData[Symbols.LedgerBytes] =
  PlutusLedgerApiV1.isPlutusDataLedgerBytes;

// StakingCredential
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.StakingCredential]: Prelude.Eq<
      PlutusLedgerApiV1.StakingCredential
    >;
  }

  export interface JsonInstances {
    [Symbols.StakingCredential]: Prelude.Json<
      PlutusLedgerApiV1.StakingCredential
    >;
  }
}

LbrPrelude.Eq[Symbols.StakingCredential] =
  PlutusLedgerApiV1.eqStakingCredential;
LbrPrelude.Json[Symbols.StakingCredential] =
  PlutusLedgerApiV1.jsonStakingCredential;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.StakingCredential]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.StakingCredential
    >;
  }
}
PlutusData.IsPlutusData[Symbols.StakingCredential] =
  PlutusLedgerApiV1.isPlutusDataStakingCredential;

// Credential
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Credential]: Prelude.Eq<PlutusLedgerApiV1.Credential>;
  }

  export interface JsonInstances {
    [Symbols.Credential]: Prelude.Json<PlutusLedgerApiV1.Credential>;
  }
}

LbrPrelude.Eq[Symbols.Credential] = PlutusLedgerApiV1.eqCredential;
LbrPrelude.Json[Symbols.Credential] = PlutusLedgerApiV1.jsonCredential;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Credential]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Credential
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Credential] =
  PlutusLedgerApiV1.isPlutusDataCredential;

// PubKeyHash
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.PubKeyHash]: Prelude.Eq<PlutusLedgerApiV1.PubKeyHash>;
  }

  export interface JsonInstances {
    [Symbols.PubKeyHash]: Prelude.Json<PlutusLedgerApiV1.PubKeyHash>;
  }
}

LbrPrelude.Eq[Symbols.PubKeyHash] = PlutusLedgerApiV1.eqPubKeyHash;
LbrPrelude.Json[Symbols.PubKeyHash] = PlutusLedgerApiV1.jsonPubKeyHash;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.PubKeyHash]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.PubKeyHash
    >;
  }
}
PlutusData.IsPlutusData[Symbols.PubKeyHash] =
  PlutusLedgerApiV1.isPlutusDataPubKeyHash;

// Interval
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Interval]: <A>(
      dictA: Prelude.Eq<A>,
    ) => Prelude.Eq<PlutusLedgerApiV1.Interval<A>>;
  }

  export interface JsonInstances {
    [Symbols.Interval]: <A>(
      dictA: Prelude.Json<A>,
    ) => Prelude.Json<PlutusLedgerApiV1.Interval<A>>;
  }
}

LbrPrelude.Eq[Symbols.Interval] = PlutusLedgerApiV1.eqInterval;
LbrPrelude.Json[Symbols.Interval] = PlutusLedgerApiV1.jsonInterval;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Interval]: <A>(
      dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
    ) => PlutusLedgerApiPlutusData.IsPlutusData<PlutusLedgerApiV1.Interval<A>>;
  }
}
PlutusData.IsPlutusData[Symbols.Interval] =
  PlutusLedgerApiV1.isPlutusDataInterval;

// Extended
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Extended]: <A>(
      dictA: Prelude.Eq<A>,
    ) => Prelude.Eq<PlutusLedgerApiV1.Extended<A>>;
  }

  export interface JsonInstances {
    [Symbols.Extended]: <A>(
      dictA: Prelude.Json<A>,
    ) => Prelude.Json<PlutusLedgerApiV1.Extended<A>>;
  }
}

LbrPrelude.Eq[Symbols.Extended] = PlutusLedgerApiV1.eqExtended;
LbrPrelude.Json[Symbols.Extended] = PlutusLedgerApiV1.jsonExtended;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Extended]: <A>(
      dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
    ) => PlutusLedgerApiPlutusData.IsPlutusData<PlutusLedgerApiV1.Extended<A>>;
  }
}
PlutusData.IsPlutusData[Symbols.Extended] =
  PlutusLedgerApiV1.isPlutusDataExtended;

// LowerBound
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.LowerBound]: <A>(
      dictA: Prelude.Eq<A>,
    ) => Prelude.Eq<PlutusLedgerApiV1.LowerBound<A>>;
  }

  export interface JsonInstances {
    [Symbols.LowerBound]: <A>(
      dictA: Prelude.Json<A>,
    ) => Prelude.Json<PlutusLedgerApiV1.LowerBound<A>>;
  }
}

LbrPrelude.Eq[Symbols.LowerBound] = PlutusLedgerApiV1.eqLowerBound;
LbrPrelude.Json[Symbols.LowerBound] = PlutusLedgerApiV1.jsonLowerBound;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.LowerBound]: <A>(
      dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
    ) => PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.LowerBound<A>
    >;
  }
}
PlutusData.IsPlutusData[Symbols.LowerBound] =
  PlutusLedgerApiV1.isPlutusDataLowerBound;

// UpperBound
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.UpperBound]: <A>(
      dictA: Prelude.Eq<A>,
    ) => Prelude.Eq<PlutusLedgerApiV1.UpperBound<A>>;
  }

  export interface JsonInstances {
    [Symbols.UpperBound]: <A>(
      dictA: Prelude.Json<A>,
    ) => Prelude.Json<PlutusLedgerApiV1.UpperBound<A>>;
  }
}

LbrPrelude.Eq[Symbols.UpperBound] = PlutusLedgerApiV1.eqUpperBound;
LbrPrelude.Json[Symbols.UpperBound] = PlutusLedgerApiV1.jsonUpperBound;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.UpperBound]: <A>(
      dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
    ) => PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.UpperBound<A>
    >;
  }
}
PlutusData.IsPlutusData[Symbols.UpperBound] =
  PlutusLedgerApiV1.isPlutusDataUpperBound;

// Closure
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Closure]: Prelude.Eq<PlutusLedgerApiV1.Closure>;
  }

  export interface JsonInstances {
    [Symbols.Closure]: Prelude.Json<PlutusLedgerApiV1.Closure>;
  }
}

LbrPrelude.Eq[Symbols.Closure] = Prelude.eqBool;
LbrPrelude.Json[Symbols.Closure] = Prelude.jsonBool;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Closure]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Closure
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Closure] = PlutusLedgerApiV1.isPlutusDataBool;

// Redeemer
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Redeemer]: Prelude.Eq<PlutusLedgerApiV1.Redeemer>;
  }

  export interface JsonInstances {
    [Symbols.Redeemer]: Prelude.Json<PlutusLedgerApiV1.Redeemer>;
  }
}

LbrPrelude.Eq[Symbols.Redeemer] = PlutusLedgerApiV1.eqRedeemer;
LbrPrelude.Json[Symbols.Redeemer] = PlutusLedgerApiV1.jsonRedeemer;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Redeemer]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Redeemer
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Redeemer] =
  PlutusLedgerApiV1.isPlutusDataRedeemer;

// Datum
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Datum]: Prelude.Eq<PlutusLedgerApiV1.Datum>;
  }

  export interface JsonInstances {
    [Symbols.Datum]: Prelude.Json<PlutusLedgerApiV1.Datum>;
  }
}

LbrPrelude.Eq[Symbols.Datum] = PlutusLedgerApiV1.eqDatum;
LbrPrelude.Json[Symbols.Datum] = PlutusLedgerApiV1.jsonDatum;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Datum]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Datum
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Datum] = PlutusLedgerApiV1.isPlutusDataDatum;

// DatumHash
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.DatumHash]: Prelude.Eq<PlutusLedgerApiV1.DatumHash>;
  }

  export interface JsonInstances {
    [Symbols.DatumHash]: Prelude.Json<PlutusLedgerApiV1.DatumHash>;
  }
}

LbrPrelude.Eq[Symbols.DatumHash] = PlutusLedgerApiV1.eqDatumHash;
LbrPrelude.Json[Symbols.DatumHash] = PlutusLedgerApiV1.jsonDatumHash;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.DatumHash]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.DatumHash
    >;
  }
}
PlutusData.IsPlutusData[Symbols.DatumHash] =
  PlutusLedgerApiV1.isPlutusDataDatumHash;

// RedeemerHash
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.RedeemerHash]: Prelude.Eq<PlutusLedgerApiV1.RedeemerHash>;
  }

  export interface JsonInstances {
    [Symbols.RedeemerHash]: Prelude.Json<PlutusLedgerApiV1.RedeemerHash>;
  }
}

LbrPrelude.Eq[Symbols.RedeemerHash] = PlutusLedgerApiV1.eqRedeemerHash;
LbrPrelude.Json[Symbols.RedeemerHash] = PlutusLedgerApiV1.jsonRedeemerHash;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.RedeemerHash]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.RedeemerHash
    >;
  }
}
PlutusData.IsPlutusData[Symbols.RedeemerHash] =
  PlutusLedgerApiV1.isPlutusDataRedeemerHash;

// ScriptHash
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ScriptHash]: Prelude.Eq<PlutusLedgerApiV1.ScriptHash>;
  }

  export interface JsonInstances {
    [Symbols.ScriptHash]: Prelude.Json<PlutusLedgerApiV1.ScriptHash>;
  }
}

LbrPrelude.Eq[Symbols.ScriptHash] = PlutusLedgerApiV1.eqScriptHash;
LbrPrelude.Json[Symbols.ScriptHash] = PlutusLedgerApiV1.jsonScriptHash;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ScriptHash]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.ScriptHash
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ScriptHash] =
  PlutusLedgerApiV1.isPlutusDataScriptHash;

// POSIXTime
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.POSIXTime]: Prelude.Eq<PlutusLedgerApiV1.POSIXTime>;
  }

  export interface JsonInstances {
    [Symbols.POSIXTime]: Prelude.Json<PlutusLedgerApiV1.POSIXTime>;
  }
}

LbrPrelude.Eq[Symbols.POSIXTime] = PlutusLedgerApiV1.eqPOSIXTime;
LbrPrelude.Json[Symbols.POSIXTime] = PlutusLedgerApiV1.jsonPOSIXTime;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.POSIXTime]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.POSIXTime
    >;
  }
}
PlutusData.IsPlutusData[Symbols.POSIXTime] =
  PlutusLedgerApiV1.isPlutusDataPOSIXTime;

// POSIXTimeRange
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.POSIXTimeRange]: Prelude.Eq<PlutusLedgerApiV1.POSIXTimeRange>;
  }

  export interface JsonInstances {
    [Symbols.POSIXTimeRange]: Prelude.Json<PlutusLedgerApiV1.POSIXTimeRange>;
  }
}

LbrPrelude.Eq[Symbols.POSIXTimeRange] = PlutusLedgerApiV1.eqPOSIXTimeRange;
LbrPrelude.Json[Symbols.POSIXTimeRange] = PlutusLedgerApiV1.jsonPOSIXTimeRange;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.POSIXTimeRange]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.POSIXTimeRange
    >;
  }
}
PlutusData.IsPlutusData[Symbols.POSIXTimeRange] =
  PlutusLedgerApiV1.isPlutusDataPOSIXTimeRange;

// TxId
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxId]: Prelude.Eq<PlutusLedgerApiV1.TxId>;
  }

  export interface JsonInstances {
    [Symbols.TxId]: Prelude.Json<PlutusLedgerApiV1.TxId>;
  }
}

LbrPrelude.Eq[Symbols.TxId] = PlutusLedgerApiV1.eqTxId;
LbrPrelude.Json[Symbols.TxId] = PlutusLedgerApiV1.jsonTxId;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxId]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.TxId
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxId] = PlutusLedgerApiV1.isPlutusDataTxId;

// TxOutRef
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxOutRef]: Prelude.Eq<PlutusLedgerApiV1.TxOutRef>;
  }

  export interface JsonInstances {
    [Symbols.TxOutRef]: Prelude.Json<PlutusLedgerApiV1.TxOutRef>;
  }
}

LbrPrelude.Eq[Symbols.TxOutRef] = PlutusLedgerApiV1.eqTxOutRef;
LbrPrelude.Json[Symbols.TxOutRef] = PlutusLedgerApiV1.jsonTxOutRef;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxOutRef]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.TxOutRef
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxOutRef] =
  PlutusLedgerApiV1.isPlutusDataTxOutRef;

// CurrencySymbol
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.CurrencySymbol]: Prelude.Eq<PlutusLedgerApiV1.CurrencySymbol>;
  }

  export interface JsonInstances {
    [Symbols.CurrencySymbol]: Prelude.Json<PlutusLedgerApiV1.CurrencySymbol>;
  }
}

LbrPrelude.Eq[Symbols.CurrencySymbol] = PlutusLedgerApiV1.eqCurrencySymbol;
LbrPrelude.Json[Symbols.CurrencySymbol] = PlutusLedgerApiV1.jsonCurrencySymbol;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.CurrencySymbol]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.CurrencySymbol
    >;
  }
}
PlutusData.IsPlutusData[Symbols.CurrencySymbol] =
  PlutusLedgerApiV1.isPlutusDataCurrencySymbol;

// TokenName
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TokenName]: Prelude.Eq<PlutusLedgerApiV1.TokenName>;
  }

  export interface JsonInstances {
    [Symbols.TokenName]: Prelude.Json<PlutusLedgerApiV1.TokenName>;
  }
}

LbrPrelude.Eq[Symbols.TokenName] = PlutusLedgerApiV1.eqTokenName;
LbrPrelude.Json[Symbols.TokenName] = PlutusLedgerApiV1.jsonTokenName;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TokenName]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.TokenName
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TokenName] =
  PlutusLedgerApiV1.isPlutusDataTokenName;

// AssetClass
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.AssetClass]: Prelude.Eq<PlutusLedgerApiV1.AssetClass>;
  }

  export interface JsonInstances {
    [Symbols.AssetClass]: Prelude.Json<PlutusLedgerApiV1.AssetClass>;
  }
}

LbrPrelude.Eq[Symbols.AssetClass] = PlutusLedgerApiV1.eqAssetClass;
LbrPrelude.Json[Symbols.AssetClass] = PlutusLedgerApiV1.jsonAssetClass;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.AssetClass]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.AssetClass
    >;
  }
}
PlutusData.IsPlutusData[Symbols.AssetClass] =
  PlutusLedgerApiV1.isPlutusDataAssetClass;

// Value
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Value]: Prelude.Eq<PlutusLedgerApiV1.Value>;
  }

  export interface JsonInstances {
    [Symbols.Value]: Prelude.Json<PlutusLedgerApiV1.Value>;
  }
}

LbrPrelude.Eq[Symbols.Value] = PlutusLedgerApiV1.eqValue;
LbrPrelude.Json[Symbols.Value] = PlutusLedgerApiV1.jsonValue;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Value]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV1.Value
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Value] = PlutusLedgerApiV1.isPlutusDataValue;

// PlutusData
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.PlutusData]: Prelude.Eq<PlutusLedgerApiPlutusData.PlutusData>;
  }

  export interface JsonInstances {
    [Symbols.PlutusData]: Prelude.Json<PlutusLedgerApiPlutusData.PlutusData>;
  }
}

LbrPrelude.Eq[Symbols.PlutusData] = PlutusLedgerApiPlutusData.eqPlutusData;
LbrPrelude.Json[Symbols.PlutusData] = PlutusLedgerApiPlutusData.jsonPlutusData;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.PlutusData]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiPlutusData.PlutusData
    >;
  }
}
PlutusData.IsPlutusData[Symbols.PlutusData] =
  PlutusLedgerApiPlutusData.isPlutusDataPlutusData;
