module Test.LambdaBuffers.Plutus.Golden (
  credentialGoldens,
  plutusDataGoldens,
  pubKeyHashGoldens,
  scriptHashGoldens,
  closureGoldens,
  extendedGoldens,
  upperBoundGoldens,
  lowerBoundGoldens,
  intervalGoldens,
  bytesGoldens,
  stakingCredentialGoldens,
  addressGoldens,
  posixTimeRangeGoldens,
  posixTimeGoldens,
  currencySymbolGoldens,
  tokenNameGoldens,
  adaCurrencySymbolGolden,
  assetClassGoldens,
  mapGoldens,
  valueGoldens,
  redeemerGoldens,
  datumGoldens,
  redeemerHashGoldens,
  datumHashGoldens,
  txIdGoldens,
  txOutRefGoldens,
  outDatumGoldens,
  txInfoGoldensV2,
  scriptContextGoldensV2,
  txOutGoldensV2,
  txInInfoGoldensV2,
  plutusDataGoldens',
  freeDayGoldens,
  workDayGoldens,
  dayGoldens,
  dGoldens,
  cGoldens,
  bGoldens,
  aGoldens,
  txOutGoldensV1,
  dCertGoldens,
  scriptPurposeGoldens,
  txInfoGoldensV1,
  scriptContextGoldensV1,
  txInInfoGoldensV1,
  fIntGoldens,
  gIntGoldens,
  maybeGoldens,
  eitherGoldens,
  listGoldens,
  boolGoldens,
  rationalGoldens,
  txIdGoldensV3,
  txOutRefGoldensV3,
  coldCommitteeCredentialGoldensV3,
  hotCommitteeCredentialGoldensV3,
  drepCredentialGoldensV3,
  drepGoldensV3,
  delegateeGoldensV3,
  lovelaceGoldens,
  txCertGoldensV3,
  voterGoldensV3,
  voteGoldensV3,
  governanceActionIdGoldensV3,
  committeeGoldensV3,
  constitutionGoldensV3,
  protocolVersionGoldensV3,
  changedParametersGoldensV3,
  governanceActionGoldensV3,
  proposalProcedureGoldensV3,
  scriptPurposeGoldensV3,
  scriptInfoGoldensV3,
  txInInfoGoldensV3,
  txInfoGoldensV3,
  scriptContextGoldensV3,
) where

import Data.ByteString qualified as B
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), FreeDay (FreeDay), WorkDay (WorkDay))
import LambdaBuffers.Foo (A (A), B (B), C (C), D (D), FInt (FInt), GInt (GInt))
import LambdaBuffers.Foo.Bar (F (F'Nil, F'Rec), FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax), G (G'Nil, G'Rec))
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Ratio (unsafeRatio)

-- | Plutus.V1
plutusDataGoldens :: [PlutusV1.Data]
plutusDataGoldens =
  [ PlutusV1.Constr 0 []
  , PlutusV1.Constr 1 [PlutusV1.I 1, PlutusV1.B "some bytes"]
  , PlutusV1.List []
  , PlutusV1.List [PlutusV1.I 1, PlutusV1.I 2]
  , PlutusV1.List [PlutusV1.I 1, PlutusV1.B "some bytes"]
  , PlutusV1.Map []
  , PlutusV1.Map [(PlutusV1.I 1, PlutusV1.B "some bytes"), (PlutusV1.I 2, PlutusV1.B "some more bytes")]
  , PlutusV1.I 0
  , PlutusV1.I 1
  , PlutusV1.I (-1)
  , PlutusV1.B ""
  , PlutusV1.B "\0"
  , PlutusV1.B "some bytes"
  ]

plutusDataGoldens' :: [PlutusV1.BuiltinData]
plutusDataGoldens' = PlutusV1.dataToBuiltinData <$> plutusDataGoldens

blake2b_256Hash :: PlutusV1.BuiltinByteString
blake2b_256Hash = PlutusV1.toBuiltin $ B.pack [1 .. 32]

blake2b_224Hash :: PlutusV1.BuiltinByteString
blake2b_224Hash = PlutusV1.toBuiltin $ B.pack [1 .. 28]

addressGoldens :: [PlutusV1.Address]
addressGoldens =
  mconcat
    [ PlutusV1.Address <$> credentialGoldens <*> pure Nothing
    , PlutusV1.Address <$> credentialGoldens <*> (Just <$> stakingCredentialGoldens)
    ]

credentialGoldens :: [PlutusV1.Credential]
credentialGoldens =
  mconcat
    [ PlutusV1.PubKeyCredential <$> pubKeyHashGoldens
    , PlutusV1.ScriptCredential <$> scriptHashGoldens
    ]

pubKeyHashGoldens :: [PlutusV1.PubKeyHash]
pubKeyHashGoldens = [PlutusV1.PubKeyHash blake2b_224Hash]

scriptHashGoldens :: [PlutusV1.ScriptHash]
scriptHashGoldens = [PlutusV1.ScriptHash blake2b_224Hash]

stakingCredentialGoldens :: [PlutusV1.StakingCredential]
stakingCredentialGoldens =
  mconcat
    [ PlutusV1.StakingHash <$> credentialGoldens
    , [PlutusV1.StakingPtr 0 1 2]
    ]

bytesGoldens :: [PlutusV1.BuiltinByteString]
bytesGoldens = PlutusV1.toBuiltin <$> [B.empty, B.pack [0], "some bytes"]

intervalGoldens :: [PlutusV1.Interval PlutusV1.POSIXTime]
intervalGoldens = mconcat [PlutusV1.Interval <$> lowerBoundGoldens <*> upperBoundGoldens]

lowerBoundGoldens :: [PlutusV1.LowerBound PlutusV1.POSIXTime]
lowerBoundGoldens = mconcat [PlutusV1.LowerBound <$> extendedGoldens <*> closureGoldens]

upperBoundGoldens :: [PlutusV1.UpperBound PlutusV1.POSIXTime]
upperBoundGoldens = mconcat [PlutusV1.UpperBound <$> extendedGoldens <*> closureGoldens]

extendedGoldens :: [PlutusV1.Extended PlutusV1.POSIXTime]
extendedGoldens = [PlutusV1.NegInf, PlutusV1.PosInf, PlutusV1.Finite 0]

closureGoldens :: [PlutusV1.Closure]
closureGoldens = [True, False]

posixTimeGoldens :: [PlutusV1.POSIXTime]
posixTimeGoldens = [0, 1, 2]

posixTimeRangeGoldens :: [PlutusV1.POSIXTimeRange]
posixTimeRangeGoldens = intervalGoldens

currencySymbolGoldens :: [PlutusV1.CurrencySymbol]
currencySymbolGoldens =
  [ PlutusV1.CurrencySymbol blake2b_224Hash
  ]

adaCurrencySymbolGolden :: PlutusV1.CurrencySymbol
adaCurrencySymbolGolden = PlutusV1.adaSymbol

tokenNameGoldens :: [PlutusV1.TokenName]
tokenNameGoldens =
  [ PlutusV1.TokenName $ PlutusV1.toBuiltin B.empty
  , PlutusV1.TokenName $ PlutusV1.toBuiltin $ B.pack [1 .. 16]
  , PlutusV1.TokenName $ PlutusV1.toBuiltin $ B.pack [1 .. 32]
  ]

assetClassGoldens :: [PlutusV1.AssetClass]
assetClassGoldens =
  mconcat
    [ PlutusV1.AssetClass <$> ((,) <$> currencySymbolGoldens <*> tokenNameGoldens)
    , [PlutusV1.AssetClass (PlutusV1.adaSymbol, PlutusV1.adaToken)]
    ]

valueGoldens :: [PlutusV1.Value]
valueGoldens =
  mconcat
    [ PlutusV1.Value <$> mapGoldens
    ]

mapGoldens :: [AssocMap.Map PlutusV1.CurrencySymbol (AssocMap.Map PlutusV1.TokenName Integer)]
mapGoldens =
  [ AssocMap.unsafeFromList []
  , AssocMap.unsafeFromList
      [ (PlutusV1.adaSymbol, AssocMap.unsafeFromList [(PlutusV1.adaToken, 1337)])
      ]
  , AssocMap.unsafeFromList
      [ (PlutusV1.adaSymbol, AssocMap.unsafeFromList [(PlutusV1.adaToken, 1337)])
      ,
        ( PlutusV1.CurrencySymbol blake2b_224Hash
        , AssocMap.unsafeFromList
            [ (PlutusV1.TokenName $ PlutusV1.toBuiltin B.empty, 1337)
            , (PlutusV1.TokenName $ PlutusV1.toBuiltin $ B.pack [1 .. 16], 16)
            , (PlutusV1.TokenName $ PlutusV1.toBuiltin $ B.pack [1 .. 32], 32)
            ]
        )
      ]
  ]

redeemerGoldens :: [PlutusV1.Redeemer]
redeemerGoldens = PlutusV1.Redeemer . PlutusV1.dataToBuiltinData <$> [PlutusV1.I 1337]

datumGoldens :: [PlutusV1.Datum]
datumGoldens = PlutusV1.Datum . PlutusV1.dataToBuiltinData <$> [PlutusV1.I 1337]

redeemerHashGoldens :: [PlutusV1.RedeemerHash]
redeemerHashGoldens = [PlutusV1.RedeemerHash blake2b_256Hash]

datumHashGoldens :: [PlutusV1.DatumHash]
datumHashGoldens = [PlutusV1.DatumHash blake2b_256Hash]

txIdGoldens :: [PlutusV1.TxId]
txIdGoldens = [PlutusV1.TxId blake2b_256Hash]

txOutRefGoldens :: [PlutusV1.TxOutRef]
txOutRefGoldens = mconcat [PlutusV1.TxOutRef <$> txIdGoldens <*> [0]]

txInInfoGoldensV1 :: [PlutusV1.TxInInfo]
txInInfoGoldensV1 = mconcat [PlutusV1.TxInInfo <$> txOutRefGoldens <*> txOutGoldensV1]

txOutGoldensV1 :: [PlutusV1.TxOut]
txOutGoldensV1 =
  mconcat
    [PlutusV1.TxOut <$> addressGoldens <*> valueGoldens <*> toMaybe datumHashGoldens]

dCertGoldens :: [PlutusV1.DCert]
dCertGoldens =
  mconcat
    [ pure PlutusV1.DCertMir
    , pure PlutusV1.DCertGenesis
    , PlutusV1.DCertPoolRetire <$> pubKeyHashGoldens <*> pure 1337
    , PlutusV1.DCertDelegRegKey <$> stakingCredentialGoldens
    , PlutusV1.DCertPoolRegister <$> pubKeyHashGoldens <*> pubKeyHashGoldens
    , PlutusV1.DCertDelegDeRegKey <$> stakingCredentialGoldens
    , PlutusV1.DCertDelegDelegate <$> stakingCredentialGoldens <*> pubKeyHashGoldens
    ]

scriptPurposeGoldens :: [PlutusV1.ScriptPurpose]
scriptPurposeGoldens =
  mconcat
    [ PlutusV1.Minting <$> currencySymbolGoldens
    , PlutusV1.Spending <$> txOutRefGoldens
    , PlutusV1.Rewarding <$> stakingCredentialGoldens
    , PlutusV1.Certifying <$> dCertGoldens
    ]

txInfoGoldensV1 :: [PlutusV1.TxInfo]
txInfoGoldensV1 =
  PlutusV1.TxInfo txInInfoGoldensV1 txOutGoldensV1
    <$> valueGoldens
    <*> valueGoldens
    <*> pure dCertGoldens
    <*> pure (map (,1234) stakingCredentialGoldens)
    <*> posixTimeRangeGoldens
    <*> pure pubKeyHashGoldens
    <*> pure (zip datumHashGoldens datumGoldens)
    <*> txIdGoldens

scriptContextGoldensV1 :: [PlutusV1.ScriptContext]
scriptContextGoldensV1 =
  PlutusV1.ScriptContext
    <$> txInfoGoldensV1
    <*> scriptPurposeGoldens

-- | Plutus.V2
txInInfoGoldensV2 :: [PlutusV2.TxInInfo]
txInInfoGoldensV2 = mconcat [PlutusV2.TxInInfo <$> txOutRefGoldens <*> txOutGoldensV2]

txOutGoldensV2 :: [PlutusV2.TxOut]
txOutGoldensV2 =
  mconcat
    [ PlutusV2.TxOut <$> addressGoldens <*> valueGoldens <*> take 2 outDatumGoldens <*> toMaybe scriptHashGoldens
    ]

outDatumGoldens :: [PlutusV2.OutputDatum]
outDatumGoldens =
  mconcat
    [ [PlutusV2.NoOutputDatum]
    , PlutusV2.OutputDatumHash <$> datumHashGoldens
    , PlutusV2.OutputDatum <$> datumGoldens
    ]

txInfoGoldensV2 :: [PlutusV2.TxInfo]
txInfoGoldensV2 =
  PlutusV2.TxInfo txInInfoGoldensV2 txInInfoGoldensV2 txOutGoldensV2
    <$> valueGoldens
    <*> valueGoldens
    <*> pure dCertGoldens
    <*> pure (AssocMap.unsafeFromList (map (,1234) stakingCredentialGoldens))
    <*> posixTimeRangeGoldens
    <*> pure pubKeyHashGoldens
    <*> pure (AssocMap.unsafeFromList (zip scriptPurposeGoldens redeemerGoldens))
    <*> pure (AssocMap.unsafeFromList (zip datumHashGoldens datumGoldens))
    <*> txIdGoldens

scriptContextGoldensV2 :: [PlutusV2.ScriptContext]
scriptContextGoldensV2 =
  PlutusV2.ScriptContext
    <$> txInfoGoldensV2
    <*> scriptPurposeGoldens

rationalGoldens :: [PlutusV3.Rational]
rationalGoldens =
  [unsafeRatio 1 2]

txIdGoldensV3 :: [PlutusV3.TxId]
txIdGoldensV3 = [PlutusV3.TxId blake2b_256Hash]

txOutRefGoldensV3 :: [PlutusV3.TxOutRef]
txOutRefGoldensV3 = mconcat [PlutusV3.TxOutRef <$> txIdGoldensV3 <*> [0]]

coldCommitteeCredentialGoldensV3 :: [PlutusV3.ColdCommitteeCredential]
coldCommitteeCredentialGoldensV3 = PlutusV3.ColdCommitteeCredential <$> credentialGoldens

hotCommitteeCredentialGoldensV3 :: [PlutusV3.HotCommitteeCredential]
hotCommitteeCredentialGoldensV3 = PlutusV3.HotCommitteeCredential <$> credentialGoldens

drepCredentialGoldensV3 :: [PlutusV3.DRepCredential]
drepCredentialGoldensV3 = PlutusV3.DRepCredential <$> credentialGoldens

drepGoldensV3 :: [PlutusV3.DRep]
drepGoldensV3 =
  mconcat
    [ PlutusV3.DRep <$> drepCredentialGoldensV3
    , [PlutusV3.DRepAlwaysAbstain]
    , [PlutusV3.DRepAlwaysNoConfidence]
    ]

delegateeGoldensV3 :: [PlutusV3.Delegatee]
delegateeGoldensV3 =
  mconcat
    [ PlutusV3.DelegStake <$> pubKeyHashGoldens
    , PlutusV3.DelegVote <$> drepGoldensV3
    , PlutusV3.DelegStakeVote <$> pubKeyHashGoldens <*> drepGoldensV3
    ]

lovelaceGoldens :: [PlutusV1.Lovelace]
lovelaceGoldens = PlutusV1.Lovelace <$> [0]

txCertGoldensV3 :: [PlutusV3.TxCert]
txCertGoldensV3 =
  mconcat
    [ PlutusV3.TxCertRegStaking <$> credentialGoldens <*> toMaybe lovelaceGoldens
    , PlutusV3.TxCertUnRegStaking <$> credentialGoldens <*> toMaybe lovelaceGoldens
    , PlutusV3.TxCertDelegStaking <$> credentialGoldens <*> delegateeGoldensV3
    , PlutusV3.TxCertRegDeleg <$> credentialGoldens <*> delegateeGoldensV3 <*> lovelaceGoldens
    , PlutusV3.TxCertRegDRep <$> drepCredentialGoldensV3 <*> lovelaceGoldens
    , PlutusV3.TxCertUpdateDRep <$> drepCredentialGoldensV3
    , PlutusV3.TxCertUnRegDRep <$> drepCredentialGoldensV3 <*> lovelaceGoldens
    , PlutusV3.TxCertPoolRegister <$> pubKeyHashGoldens <*> pubKeyHashGoldens
    , PlutusV3.TxCertPoolRetire <$> pubKeyHashGoldens <*> [0]
    , PlutusV3.TxCertAuthHotCommittee <$> coldCommitteeCredentialGoldensV3 <*> hotCommitteeCredentialGoldensV3
    , PlutusV3.TxCertResignColdCommittee <$> coldCommitteeCredentialGoldensV3
    ]

voterGoldensV3 :: [PlutusV3.Voter]
voterGoldensV3 =
  mconcat
    [ PlutusV3.CommitteeVoter <$> hotCommitteeCredentialGoldensV3
    , PlutusV3.DRepVoter <$> drepCredentialGoldensV3
    , PlutusV3.StakePoolVoter <$> pubKeyHashGoldens
    ]

voteGoldensV3 :: [PlutusV3.Vote]
voteGoldensV3 = [PlutusV3.VoteNo, PlutusV3.VoteYes, PlutusV3.Abstain]

governanceActionIdGoldensV3 :: [PlutusV3.GovernanceActionId]
governanceActionIdGoldensV3 =
  PlutusV3.GovernanceActionId <$> txIdGoldensV3 <*> [0]

committeeGoldensV3 :: [PlutusV3.Committee]
committeeGoldensV3 =
  PlutusV3.Committee
    <$> [AssocMap.unsafeFromList ((,) <$> coldCommitteeCredentialGoldensV3 <*> [0])]
    <*> rationalGoldens

constitutionGoldensV3 :: [PlutusV3.Constitution]
constitutionGoldensV3 = PlutusV3.Constitution <$> toMaybe scriptHashGoldens

protocolVersionGoldensV3 :: [PlutusV3.ProtocolVersion]
protocolVersionGoldensV3 =
  PlutusV3.ProtocolVersion <$> [1] <*> [2]

changedParametersGoldensV3 :: [PlutusV3.ChangedParameters]
changedParametersGoldensV3 =
  PlutusV3.ChangedParameters <$> plutusDataGoldens'

governanceActionGoldensV3 :: [PlutusV3.GovernanceAction]
governanceActionGoldensV3 =
  mconcat
    [ PlutusV3.ParameterChange <$> toMaybe governanceActionIdGoldensV3 <*> changedParametersGoldensV3 <*> toMaybe scriptHashGoldens
    , PlutusV3.HardForkInitiation <$> toMaybe governanceActionIdGoldensV3 <*> protocolVersionGoldensV3
    , PlutusV3.TreasuryWithdrawals <$> [toMap credentialGoldens lovelaceGoldens] <*> toMaybe scriptHashGoldens
    , PlutusV3.NoConfidence <$> toMaybe governanceActionIdGoldensV3
    , PlutusV3.UpdateCommittee <$> toMaybe governanceActionIdGoldensV3 <*> [coldCommitteeCredentialGoldensV3] <*> [toMap coldCommitteeCredentialGoldensV3 [0]] <*> rationalGoldens
    , PlutusV3.NewConstitution <$> toMaybe governanceActionIdGoldensV3 <*> constitutionGoldensV3
    , [PlutusV3.InfoAction]
    ]

proposalProcedureGoldensV3 :: [PlutusV3.ProposalProcedure]
proposalProcedureGoldensV3 =
  PlutusV3.ProposalProcedure <$> lovelaceGoldens <*> credentialGoldens <*> governanceActionGoldensV3

scriptPurposeGoldensV3 :: [PlutusV3.ScriptPurpose]
scriptPurposeGoldensV3 =
  mconcat
    [ PlutusV3.Minting <$> currencySymbolGoldens
    , PlutusV3.Spending <$> txOutRefGoldensV3
    , PlutusV3.Rewarding <$> credentialGoldens
    , PlutusV3.Certifying <$> [0] <*> txCertGoldensV3
    , PlutusV3.Voting <$> voterGoldensV3
    , PlutusV3.Proposing <$> [0] <*> proposalProcedureGoldensV3
    ]

scriptInfoGoldensV3 :: [PlutusV3.ScriptInfo]
scriptInfoGoldensV3 =
  mconcat
    [ PlutusV3.MintingScript <$> currencySymbolGoldens
    , PlutusV3.SpendingScript <$> txOutRefGoldensV3 <*> toMaybe datumGoldens
    , PlutusV3.RewardingScript <$> credentialGoldens
    , PlutusV3.CertifyingScript <$> [0] <*> txCertGoldensV3
    , PlutusV3.VotingScript <$> voterGoldensV3
    , PlutusV3.ProposingScript <$> [0] <*> proposalProcedureGoldensV3
    ]

txInInfoGoldensV3 :: [PlutusV3.TxInInfo]
txInInfoGoldensV3 = mconcat [PlutusV3.TxInInfo <$> txOutRefGoldensV3 <*> txOutGoldensV2]

txInfoGoldensV3 :: [PlutusV3.TxInfo]
txInfoGoldensV3 =
  PlutusV3.TxInfo txInInfoGoldensV3 txInInfoGoldensV3 txOutGoldensV2
    <$> lovelaceGoldens
    <*> valueGoldens
    <*> pure txCertGoldensV3
    <*> pure (AssocMap.unsafeFromList (map (,1234) credentialGoldens))
    <*> posixTimeRangeGoldens
    <*> pure pubKeyHashGoldens
    <*> pure (AssocMap.unsafeFromList (zip scriptPurposeGoldensV3 redeemerGoldens))
    <*> pure (AssocMap.unsafeFromList (zip datumHashGoldens datumGoldens))
    <*> txIdGoldensV3
    <*> [toMap voterGoldensV3 [toMap governanceActionIdGoldensV3 voteGoldensV3]]
    <*> pure proposalProcedureGoldensV3
    <*> toMaybe lovelaceGoldens
    <*> toMaybe lovelaceGoldens

scriptContextGoldensV3 :: [PlutusV3.ScriptContext]
scriptContextGoldensV3 =
  PlutusV3.ScriptContext
    <$> txInfoGoldensV3
    <*> redeemerGoldens
    <*> scriptInfoGoldensV3

toMaybe :: forall a. [a] -> [Maybe a]
toMaybe goldens = Nothing : (Just <$> goldens)

toMap :: forall a b. [a] -> [b] -> AssocMap.Map a b
toMap goldensA goldensB = AssocMap.unsafeFromList ((,) <$> goldensA <*> goldensB)

-- | Foo.Bar
fooSumGoldens :: a -> b -> c -> [FooSum a b c]
fooSumGoldens x y z =
  [ FooSum'Foo x y z
  , FooSum'Bar x y
  , FooSum'Baz y
  , FooSum'Qax
  , FooSum'Faz 0
  ]

fooProdGoldens :: a -> b -> c -> [FooProd a b c]
fooProdGoldens x y z = [FooProd x y z 1337]

fooRecGoldens :: a -> b -> c -> [FooRec a b c]
fooRecGoldens x y z = [FooRec x y z 1337]

-- | Foo
aGoldens :: [A]
aGoldens = A <$> mconcat (fooSumGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens)

bGoldens :: [B]
bGoldens = B <$> mconcat (fooProdGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens)

cGoldens :: [C]
cGoldens = C <$> mconcat (fooRecGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens)

dGoldens :: [D]
dGoldens =
  do
    fooSum <- take 2 $ mconcat $ fooSumGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens
    fooProd <- take 2 $ mconcat $ fooProdGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens
    fooRec <- take 2 $ mconcat $ fooRecGoldens <$> addressGoldens <*> valueGoldens <*> datumGoldens
    return (D $ FooComplicated fooSum fooProd fooRec)

fIntGoldens :: [FInt]
fIntGoldens = FInt <$> [F'Nil, F'Rec G'Nil]

gIntGoldens :: [GInt]
gIntGoldens = GInt <$> [G'Nil, G'Rec F'Nil]

-- | Days
dayGoldens :: [Day]
dayGoldens = [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday, Day'Saturday, Day'Sunday]

workDayGoldens :: [WorkDay]
workDayGoldens = WorkDay <$> [Day'Monday, Day'Tuesday, Day'Wednesday, Day'Thursday, Day'Friday]

freeDayGoldens :: [FreeDay]
freeDayGoldens = FreeDay <$> [Day'Saturday, Day'Sunday]

-- | Prelude types.
boolGoldens :: [Bool]
boolGoldens = [False, True]

maybeGoldens :: [Maybe Bool]
maybeGoldens = [Nothing, Just True, Just False]

eitherGoldens :: [Either Bool Bool]
eitherGoldens = [Left True, Left False, Right True]

listGoldens :: [[Bool]]
listGoldens = [[], [True], [False], [True, True, False, False]]
