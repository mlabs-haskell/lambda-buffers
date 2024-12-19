import * as PlutusData from "../PlutusData.js";
import * as PlutusLedgerApiPlutusData from "plutus-ledger-api/PlutusData.js";
import * as PlutusLedgerApiV3 from "plutus-ledger-api/V3.js";
import * as PlutusLedgerApiRatio from "plutus-ledger-api/Ratio.js";
import * as LbrPrelude from "lbr-prelude";
import * as Prelude from "prelude";
import * as Symbols from "./Symbols.js";

// TxId
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxId]: Prelude.Eq<PlutusLedgerApiV3.TxId>;
  }

  export interface JsonInstances {
    [Symbols.TxId]: Prelude.Json<PlutusLedgerApiV3.TxId>;
  }
}

LbrPrelude.Eq[Symbols.TxId] = PlutusLedgerApiV3.eqTxId;
LbrPrelude.Json[Symbols.TxId] = PlutusLedgerApiV3.jsonTxId;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxId]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.TxId
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxId] = PlutusLedgerApiV3.isPlutusDataTxId;

// TxOutRef
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxOutRef]: Prelude.Eq<PlutusLedgerApiV3.TxOutRef>;
  }

  export interface JsonInstances {
    [Symbols.TxOutRef]: Prelude.Json<PlutusLedgerApiV3.TxOutRef>;
  }
}

LbrPrelude.Eq[Symbols.TxOutRef] = PlutusLedgerApiV3.eqTxOutRef;
LbrPrelude.Json[Symbols.TxOutRef] = PlutusLedgerApiV3.jsonTxOutRef;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxOutRef]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.TxOutRef
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxOutRef] =
  PlutusLedgerApiV3.isPlutusDataTxOutRef;

// Rational
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Rational]: Prelude.Eq<PlutusLedgerApiRatio.Rational>;
  }

  export interface JsonInstances {
    [Symbols.Rational]: Prelude.Json<PlutusLedgerApiRatio.Rational>;
  }
}

LbrPrelude.Eq[Symbols.Rational] = PlutusLedgerApiRatio.eqRational;
LbrPrelude.Json[Symbols.Rational] = PlutusLedgerApiRatio.jsonRational;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Rational]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiRatio.Rational
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Rational] =
  PlutusLedgerApiRatio.isPlutusDataRational;

// ColdCommitteeCredential
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ColdCommitteeCredential]: Prelude.Eq<
      PlutusLedgerApiV3.ColdCommitteeCredential
    >;
  }

  export interface JsonInstances {
    [Symbols.ColdCommitteeCredential]: Prelude.Json<
      PlutusLedgerApiV3.ColdCommitteeCredential
    >;
  }
}

LbrPrelude.Eq[Symbols.ColdCommitteeCredential] =
  PlutusLedgerApiV3.eqColdCommitteeCredential;
LbrPrelude.Json[Symbols.ColdCommitteeCredential] =
  PlutusLedgerApiV3.jsonColdCommitteeCredential;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ColdCommitteeCredential]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ColdCommitteeCredential
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ColdCommitteeCredential] =
  PlutusLedgerApiV3.isPlutusDataColdCommitteeCredential;

// HotCommitteeCredential
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.HotCommitteeCredential]: Prelude.Eq<
      PlutusLedgerApiV3.HotCommitteeCredential
    >;
  }

  export interface JsonInstances {
    [Symbols.HotCommitteeCredential]: Prelude.Json<
      PlutusLedgerApiV3.HotCommitteeCredential
    >;
  }
}

LbrPrelude.Eq[Symbols.HotCommitteeCredential] =
  PlutusLedgerApiV3.eqHotCommitteeCredential;
LbrPrelude.Json[Symbols.HotCommitteeCredential] =
  PlutusLedgerApiV3.jsonHotCommitteeCredential;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.HotCommitteeCredential]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.HotCommitteeCredential
    >;
  }
}
PlutusData.IsPlutusData[Symbols.HotCommitteeCredential] =
  PlutusLedgerApiV3.isPlutusDataHotCommitteeCredential;

// DRepCredential
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.DRepCredential]: Prelude.Eq<PlutusLedgerApiV3.DRepCredential>;
  }

  export interface JsonInstances {
    [Symbols.DRepCredential]: Prelude.Json<PlutusLedgerApiV3.DRepCredential>;
  }
}

LbrPrelude.Eq[Symbols.DRepCredential] = PlutusLedgerApiV3.eqDRepCredential;
LbrPrelude.Json[Symbols.DRepCredential] = PlutusLedgerApiV3.jsonDRepCredential;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.DRepCredential]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.DRepCredential
    >;
  }
}
PlutusData.IsPlutusData[Symbols.DRepCredential] =
  PlutusLedgerApiV3.isPlutusDataDRepCredential;

// DRep
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.DRep]: Prelude.Eq<PlutusLedgerApiV3.DRep>;
  }

  export interface JsonInstances {
    [Symbols.DRep]: Prelude.Json<PlutusLedgerApiV3.DRep>;
  }
}

LbrPrelude.Eq[Symbols.DRep] = PlutusLedgerApiV3.eqDRep;
LbrPrelude.Json[Symbols.DRep] = PlutusLedgerApiV3.jsonDRep;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.DRep]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.DRep
    >;
  }
}
PlutusData.IsPlutusData[Symbols.DRep] = PlutusLedgerApiV3.isPlutusDataDRep;

// Delegatee
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Delegatee]: Prelude.Eq<PlutusLedgerApiV3.Delegatee>;
  }

  export interface JsonInstances {
    [Symbols.Delegatee]: Prelude.Json<PlutusLedgerApiV3.Delegatee>;
  }
}

LbrPrelude.Eq[Symbols.Delegatee] = PlutusLedgerApiV3.eqDelegatee;
LbrPrelude.Json[Symbols.Delegatee] = PlutusLedgerApiV3.jsonDelegatee;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Delegatee]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.Delegatee
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Delegatee] =
  PlutusLedgerApiV3.isPlutusDataDelegatee;

// TxCert
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxCert]: Prelude.Eq<PlutusLedgerApiV3.TxCert>;
  }

  export interface JsonInstances {
    [Symbols.TxCert]: Prelude.Json<PlutusLedgerApiV3.TxCert>;
  }
}

LbrPrelude.Eq[Symbols.TxCert] = PlutusLedgerApiV3.eqTxCert;
LbrPrelude.Json[Symbols.TxCert] = PlutusLedgerApiV3.jsonTxCert;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxCert]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.TxCert
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxCert] = PlutusLedgerApiV3.isPlutusDataTxCert;

// Voter
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Voter]: Prelude.Eq<PlutusLedgerApiV3.Voter>;
  }

  export interface JsonInstances {
    [Symbols.Voter]: Prelude.Json<PlutusLedgerApiV3.Voter>;
  }
}

LbrPrelude.Eq[Symbols.Voter] = PlutusLedgerApiV3.eqVoter;
LbrPrelude.Json[Symbols.Voter] = PlutusLedgerApiV3.jsonVoter;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Voter]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.Voter
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Voter] = PlutusLedgerApiV3.isPlutusDataVoter;

// Vote
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Vote]: Prelude.Eq<PlutusLedgerApiV3.Vote>;
  }

  export interface JsonInstances {
    [Symbols.Vote]: Prelude.Json<PlutusLedgerApiV3.Vote>;
  }
}

LbrPrelude.Eq[Symbols.Vote] = PlutusLedgerApiV3.eqVote;
LbrPrelude.Json[Symbols.Vote] = PlutusLedgerApiV3.jsonVote;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Vote]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.Vote
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Vote] = PlutusLedgerApiV3.isPlutusDataVote;

// GovernanceActionId
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.GovernanceActionId]: Prelude.Eq<
      PlutusLedgerApiV3.GovernanceActionId
    >;
  }

  export interface JsonInstances {
    [Symbols.GovernanceActionId]: Prelude.Json<
      PlutusLedgerApiV3.GovernanceActionId
    >;
  }
}

LbrPrelude.Eq[Symbols.GovernanceActionId] =
  PlutusLedgerApiV3.eqGovernanceActionId;
LbrPrelude.Json[Symbols.GovernanceActionId] =
  PlutusLedgerApiV3.jsonGovernanceActionId;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.GovernanceActionId]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.GovernanceActionId
    >;
  }
}
PlutusData.IsPlutusData[Symbols.GovernanceActionId] =
  PlutusLedgerApiV3.isPlutusDataGovernanceActionId;

// Committee
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Committee]: Prelude.Eq<PlutusLedgerApiV3.Committee>;
  }

  export interface JsonInstances {
    [Symbols.Committee]: Prelude.Json<PlutusLedgerApiV3.Committee>;
  }
}

LbrPrelude.Eq[Symbols.Committee] = PlutusLedgerApiV3.eqCommittee;
LbrPrelude.Json[Symbols.Committee] = PlutusLedgerApiV3.jsonCommittee;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Committee]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.Committee
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Committee] =
  PlutusLedgerApiV3.isPlutusDataCommittee;

// Constitution
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.Constitution]: Prelude.Eq<PlutusLedgerApiV3.Constitution>;
  }

  export interface JsonInstances {
    [Symbols.Constitution]: Prelude.Json<PlutusLedgerApiV3.Constitution>;
  }
}

LbrPrelude.Eq[Symbols.Constitution] = PlutusLedgerApiV3.eqConstitution;
LbrPrelude.Json[Symbols.Constitution] = PlutusLedgerApiV3.jsonConstitution;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.Constitution]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.Constitution
    >;
  }
}
PlutusData.IsPlutusData[Symbols.Constitution] =
  PlutusLedgerApiV3.isPlutusDataConstitution;

// ProtocolVersion
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ProtocolVersion]: Prelude.Eq<PlutusLedgerApiV3.ProtocolVersion>;
  }

  export interface JsonInstances {
    [Symbols.ProtocolVersion]: Prelude.Json<PlutusLedgerApiV3.ProtocolVersion>;
  }
}

LbrPrelude.Eq[Symbols.ProtocolVersion] = PlutusLedgerApiV3.eqProtocolVersion;
LbrPrelude.Json[Symbols.ProtocolVersion] =
  PlutusLedgerApiV3.jsonProtocolVersion;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ProtocolVersion]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ProtocolVersion
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ProtocolVersion] =
  PlutusLedgerApiV3.isPlutusDataProtocolVersion;

// ChangedParameters
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ChangedParameters]: Prelude.Eq<
      PlutusLedgerApiV3.ChangedParameters
    >;
  }

  export interface JsonInstances {
    [Symbols.ChangedParameters]: Prelude.Json<
      PlutusLedgerApiV3.ChangedParameters
    >;
  }
}

LbrPrelude.Eq[Symbols.ChangedParameters] =
  PlutusLedgerApiV3.eqChangedParameters;
LbrPrelude.Json[Symbols.ChangedParameters] =
  PlutusLedgerApiV3.jsonChangedParameters;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ChangedParameters]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ChangedParameters
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ChangedParameters] =
  PlutusLedgerApiV3.isPlutusDataChangedParameters;

// GovernanceAction
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.GovernanceAction]: Prelude.Eq<PlutusLedgerApiV3.GovernanceAction>;
  }

  export interface JsonInstances {
    [Symbols.GovernanceAction]: Prelude.Json<
      PlutusLedgerApiV3.GovernanceAction
    >;
  }
}

LbrPrelude.Eq[Symbols.GovernanceAction] = PlutusLedgerApiV3.eqGovernanceAction;
LbrPrelude.Json[Symbols.GovernanceAction] =
  PlutusLedgerApiV3.jsonGovernanceAction;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.GovernanceAction]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.GovernanceAction
    >;
  }
}
PlutusData.IsPlutusData[Symbols.GovernanceAction] =
  PlutusLedgerApiV3.isPlutusDataGovernanceAction;

// ProposalProcedure
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ProposalProcedure]: Prelude.Eq<
      PlutusLedgerApiV3.ProposalProcedure
    >;
  }

  export interface JsonInstances {
    [Symbols.ProposalProcedure]: Prelude.Json<
      PlutusLedgerApiV3.ProposalProcedure
    >;
  }
}

LbrPrelude.Eq[Symbols.ProposalProcedure] =
  PlutusLedgerApiV3.eqProposalProcedure;
LbrPrelude.Json[Symbols.ProposalProcedure] =
  PlutusLedgerApiV3.jsonProposalProcedure;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ProposalProcedure]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ProposalProcedure
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ProposalProcedure] =
  PlutusLedgerApiV3.isPlutusDataProposalProcedure;

// ScriptPurpose
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ScriptPurpose]: Prelude.Eq<PlutusLedgerApiV3.ScriptPurpose>;
  }

  export interface JsonInstances {
    [Symbols.ScriptPurpose]: Prelude.Json<PlutusLedgerApiV3.ScriptPurpose>;
  }
}

LbrPrelude.Eq[Symbols.ScriptPurpose] = PlutusLedgerApiV3.eqScriptPurpose;
LbrPrelude.Json[Symbols.ScriptPurpose] = PlutusLedgerApiV3.jsonScriptPurpose;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ScriptPurpose]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ScriptPurpose
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ScriptPurpose] =
  PlutusLedgerApiV3.isPlutusDataScriptPurpose;

// ScriptInfo
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ScriptInfo]: Prelude.Eq<PlutusLedgerApiV3.ScriptInfo>;
  }

  export interface JsonInstances {
    [Symbols.ScriptInfo]: Prelude.Json<PlutusLedgerApiV3.ScriptInfo>;
  }
}

LbrPrelude.Eq[Symbols.ScriptInfo] = PlutusLedgerApiV3.eqScriptInfo;
LbrPrelude.Json[Symbols.ScriptInfo] = PlutusLedgerApiV3.jsonScriptInfo;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ScriptInfo]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ScriptInfo
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ScriptInfo] =
  PlutusLedgerApiV3.isPlutusDataScriptInfo;

// TxInfo
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.TxInfo]: Prelude.Eq<PlutusLedgerApiV3.TxInfo>;
  }

  export interface JsonInstances {
    [Symbols.TxInfo]: Prelude.Json<PlutusLedgerApiV3.TxInfo>;
  }
}

LbrPrelude.Eq[Symbols.TxInfo] = PlutusLedgerApiV3.eqTxInfo;
LbrPrelude.Json[Symbols.TxInfo] = PlutusLedgerApiV3.jsonTxInfo;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.TxInfo]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.TxInfo
    >;
  }
}
PlutusData.IsPlutusData[Symbols.TxInfo] = PlutusLedgerApiV3.isPlutusDataTxInfo;

// ScriptContext
declare module "lbr-prelude" {
  export interface EqInstances {
    [Symbols.ScriptContext]: Prelude.Eq<PlutusLedgerApiV3.ScriptContext>;
  }

  export interface JsonInstances {
    [Symbols.ScriptContext]: Prelude.Json<PlutusLedgerApiV3.ScriptContext>;
  }
}

LbrPrelude.Eq[Symbols.ScriptContext] = PlutusLedgerApiV3.eqScriptContext;
LbrPrelude.Json[Symbols.ScriptContext] = PlutusLedgerApiV3.jsonScriptContext;

declare module "../PlutusData.js" {
  export interface IsPlutusDataInstances {
    [Symbols.ScriptContext]: PlutusLedgerApiPlutusData.IsPlutusData<
      PlutusLedgerApiV3.ScriptContext
    >;
  }
}
PlutusData.IsPlutusData[Symbols.ScriptContext] =
  PlutusLedgerApiV3.isPlutusDataScriptContext;
