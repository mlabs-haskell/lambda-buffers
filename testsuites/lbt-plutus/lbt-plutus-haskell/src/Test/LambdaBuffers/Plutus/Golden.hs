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
) where

import Data.ByteString qualified as B
import LambdaBuffers.Days (Day (Day'Friday, Day'Monday, Day'Saturday, Day'Sunday, Day'Thursday, Day'Tuesday, Day'Wednesday), FreeDay (FreeDay), WorkDay (WorkDay))
import LambdaBuffers.Foo (A (A), B (B), C (C), D (D), FInt (FInt), GInt (GInt))
import LambdaBuffers.Foo.Bar (F (F'Nil, F'Rec), FooComplicated (FooComplicated), FooProd (FooProd), FooRec (FooRec), FooSum (FooSum'Bar, FooSum'Baz, FooSum'Faz, FooSum'Foo, FooSum'Qax), G (G'Nil, G'Rec))
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Value qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx.AssocMap qualified as AssocMap

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
  [ AssocMap.fromList []
  , AssocMap.fromList
      [ (PlutusV1.adaSymbol, AssocMap.fromList [(PlutusV1.adaToken, 1337)])
      ]
  , AssocMap.fromList
      [ (PlutusV1.adaSymbol, AssocMap.fromList [(PlutusV1.adaToken, 1337)])
      ,
        ( PlutusV1.CurrencySymbol blake2b_224Hash
        , AssocMap.fromList
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
    [PlutusV1.TxOut <$> addressGoldens <*> valueGoldens <*> (Nothing : (Just <$> datumHashGoldens))]

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
    [ PlutusV2.TxOut <$> addressGoldens <*> valueGoldens <*> take 2 outDatumGoldens <*> (Nothing : (Just <$> scriptHashGoldens))
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
    <*> pure (AssocMap.fromList (map (,1234) stakingCredentialGoldens))
    <*> posixTimeRangeGoldens
    <*> pure pubKeyHashGoldens
    <*> pure (AssocMap.fromList (zip scriptPurposeGoldens redeemerGoldens))
    <*> pure (AssocMap.fromList (zip datumHashGoldens datumGoldens))
    <*> txIdGoldens

scriptContextGoldensV2 :: [PlutusV2.ScriptContext]
scriptContextGoldensV2 =
  PlutusV2.ScriptContext
    <$> txInfoGoldensV2
    <*> scriptPurposeGoldens

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
