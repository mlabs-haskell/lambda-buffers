import * as PlutusData from "../PlutusData.js";
import * as PlutusLedgerApiPlutusData from "plutus-ledger-api/PlutusData.js";
import * as PlutusLedgerApiV2 from "plutus-ledger-api/V2.js";
import * as LbrPrelude from "lbr-prelude";
import * as Prelude from "prelude";
import * as Symbols from "./Symbols.js";

// TxInInfo
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxInInfo]: Prelude.Eq<PlutusLedgerApiV2.TxInInfo>;
  }

  export interface JsonInstances {
    [Symbols.TxInInfo]: Prelude.Json<PlutusLedgerApiV2.TxInInfo>;
  }
}

LbrPrelude.Eq[Symbols.TxInInfo] = PlutusLedgerApiV2.eqTxInInfo;
LbrPrelude.Json[Symbols.TxInInfo] = PlutusLedgerApiV2.jsonTxInInfo;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxInInfo]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV2.TxInInfo
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxInInfo] =
  PlutusLedgerApiV2.isPlutusDataTxInInfo;

// OutputDatum
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.OutputDatum]: Prelude.Eq<PlutusLedgerApiV2.OutputDatum>;
  }

  export interface JsonInstances {
    [Symbols.OutputDatum]: Prelude.Json<PlutusLedgerApiV2.OutputDatum>;
  }
}

LbrPrelude.Eq[Symbols.OutputDatum] = PlutusLedgerApiV2.eqOutputDatum;
LbrPrelude.Json[Symbols.OutputDatum] = PlutusLedgerApiV2.jsonOutputDatum;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.OutputDatum]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV2.OutputDatum
    >;
  }
}
PlutusData.IsPlutusData[Symbols.OutputDatum] =
  PlutusLedgerApiV2.isPlutusDataOutputDatum;

// TxOut
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxOut]: Prelude.Eq<PlutusLedgerApiV2.TxOut>;
  }

  export interface JsonInstances {
    [Symbols.TxOut]: Prelude.Json<PlutusLedgerApiV2.TxOut>;
  }
}

LbrPrelude.Eq[Symbols.TxOut] = PlutusLedgerApiV2.eqTxOut;
LbrPrelude.Json[Symbols.TxOut] = PlutusLedgerApiV2.jsonTxOut;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxOut]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV2.TxOut
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxOut] = PlutusLedgerApiV2.isPlutusDataTxOut;

// TxInfo
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxInfo]: Prelude.Eq<PlutusLedgerApiV2.TxInfo>;
  }

  export interface JsonInstances {
    [Symbols.TxInfo]: Prelude.Json<PlutusLedgerApiV2.TxInfo>;
  }
}

LbrPrelude.Eq[Symbols.TxInfo] = PlutusLedgerApiV2.eqTxInfo;
LbrPrelude.Json[Symbols.TxInfo] = PlutusLedgerApiV2.jsonTxInfo;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxInfo]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV2.TxInfo
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxInfo] = PlutusLedgerApiV2.isPlutusDataTxInfo;

// ScriptContext
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ScriptContext]: Prelude.Eq<PlutusLedgerApiV2.ScriptContext>;
  }

  export interface JsonInstances {
    [Symbols.ScriptContext]: Prelude.Json<PlutusLedgerApiV2.ScriptContext>;
  }
}

LbrPrelude.Eq[Symbols.ScriptContext] = PlutusLedgerApiV2.eqScriptContext;
LbrPrelude.Json[Symbols.ScriptContext] = PlutusLedgerApiV2.jsonScriptContext;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ScriptContext]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV2.ScriptContext
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ScriptContext] =
  PlutusLedgerApiV2.isPlutusDataScriptContext;
