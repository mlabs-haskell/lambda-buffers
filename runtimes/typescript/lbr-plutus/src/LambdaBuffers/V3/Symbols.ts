import * as PlutusLedgerApiV3 from "plutus-ledger-api/V3.js";
import * as PlutusLedgerApiRatio from "plutus-ledger-api/Ratio.js";

export const Rational: unique symbol = Symbol("Rational");
export const ColdCommitteeCredential: unique symbol = Symbol(
  "ColdCommitteeCredential",
);
export const HotCommitteeCredential: unique symbol = Symbol(
  "HotCommitteeCredential",
);
export const DRepCredential: unique symbol = Symbol("DRepCredential");
export const DRep: unique symbol = Symbol("DRep");
export const Delegatee: unique symbol = Symbol("Delegatee");
export const TxCert: unique symbol = Symbol("TxCert");
export const Voter: unique symbol = Symbol("Voter");
export const Vote: unique symbol = Symbol("Vote");
export const GovernanceActionId: unique symbol = Symbol("GovernanceActionId");
export const Committee: unique symbol = Symbol("Committee");
export const Constitution: unique symbol = Symbol("Constitution");
export const ProtocolVersion: unique symbol = Symbol("ProtocolVersion");
export const ChangedParameters: unique symbol = Symbol("ChangedParameters");
export const GovernanceAction: unique symbol = Symbol("GovernanceAction");
export const ProposalProcedure: unique symbol = Symbol("ProposalProcedure");
export const ScriptPurpose: unique symbol = Symbol("ScriptPurpose");
export const ScriptInfo: unique symbol = Symbol("ScriptInfo");
export const TxInfo: unique symbol = Symbol("TxInfo");
export const ScriptContext: unique symbol = Symbol("ScriptContext");

export type Rational = PlutusLedgerApiRatio.Rational;
export type ColdCommitteeCredential = PlutusLedgerApiV3.ColdCommitteeCredential;
export type HotCommitteeCredential = PlutusLedgerApiV3.HotCommitteeCredential;
export type DRepCredential = PlutusLedgerApiV3.DRepCredential;
export type DRep = PlutusLedgerApiV3.DRep;
export type Delegatee = PlutusLedgerApiV3.Delegatee;
export type TxCert = PlutusLedgerApiV3.TxCert;
export type Voter = PlutusLedgerApiV3.Voter;
export type Vote = PlutusLedgerApiV3.Vote;
export type GovernanceActionId = PlutusLedgerApiV3.GovernanceActionId;
export type Committee = PlutusLedgerApiV3.Committee;
export type Constitution = PlutusLedgerApiV3.Constitution;
export type ProtocolVersion = PlutusLedgerApiV3.ProtocolVersion;
export type ChangedParameters = PlutusLedgerApiV3.ChangedParameters;
export type GovernanceAction = PlutusLedgerApiV3.GovernanceAction;
export type ProposalProcedure = PlutusLedgerApiV3.ProposalProcedure;
export type ScriptPurpose = PlutusLedgerApiV3.ScriptPurpose;
export type ScriptInfo = PlutusLedgerApiV3.ScriptInfo;
export type TxInfo = PlutusLedgerApiV3.TxInfo;
export type ScriptContext = PlutusLedgerApiV3.ScriptContext;
