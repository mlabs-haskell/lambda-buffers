import * as PlutusLedgerApiV2 from "plutus-ledger-api/V2.js";

export const TxInInfo: unique symbol = Symbol("TxInInfo");
export const OutputDatum: unique symbol = Symbol("OutputDatum");
export const TxOut: unique symbol = Symbol("TxOut");

export type TxInInfo = PlutusLedgerApiV2.TxInInfo;
export type OutputDatum = PlutusLedgerApiV2.OutputDatum;
export type TxOut = PlutusLedgerApiV2.TxOut;
