import { describe, it } from "node:test";

import * as Utils from "./Utils.js";

import * as Goldens from "./Goldens.js";

import * as LbrPrelude from "lbr-prelude";
import * as PreludeJson from "prelude/Json.js";

import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as LbrPlutusV2 from "lbr-plutus/V2.js";

describe("JSON tests (toJson . fromJson)", () => {
  const goldenDir = `data/lbt-plutus-golden-data`;

  it(`PlutusV1.PlutusData from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.PlutusData\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson,
        LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson,
        PreludeJson.stringify,
      ),
      Goldens.plutusDataGoldens(),
    );
  });

  it(`PlutusV1.Address from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Address\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Address].fromJson,
        LbrPrelude.Json[LbrPlutusV1.Address].toJson,
        PreludeJson.stringify,
      ),
      Goldens.addressGoldens(),
    );
  });

  it(`PlutusV1.Credential from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Credential\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Credential].fromJson,
        LbrPrelude.Json[LbrPlutusV1.Credential].toJson,
        PreludeJson.stringify,
      ),
      Goldens.credentialGoldens(),
    );
  });

  it(`PlutusV1.StakingCredential from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(
        /^PlutusV1\.StakingCredential\.[0-9]*\.json$/g,
      ),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.StakingCredential].fromJson,
        LbrPrelude.Json[LbrPlutusV1.StakingCredential].toJson,
        PreludeJson.stringify,
      ),
      Goldens.stakingCredentialGoldens(),
    );
  });

  it(`PlutusV1.PubKeyHash from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.PubKeyHash\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.PubKeyHash].fromJson,
        LbrPrelude.Json[LbrPlutusV1.PubKeyHash].toJson,
        PreludeJson.stringify,
      ),
      Goldens.pubKeyHashGoldens(),
    );
  });

  it(`PlutusV1.Bytes from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Bytes\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.LedgerBytes].fromJson,
        LbrPrelude.Json[LbrPlutusV1.LedgerBytes].toJson,
        PreludeJson.stringify,
      ),
      Goldens.bytesGoldens(),
    );
  });

  it(`PlutusV1.Interval from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Interval\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Interval](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).fromJson,
        LbrPrelude.Json[LbrPlutusV1.Interval](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).toJson,
        PreludeJson.stringify,
      ),
      Goldens.intervalGoldens(),
    );
  });

  it(`PlutusV1.Extended from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Extended\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Extended](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).fromJson,
        LbrPrelude.Json[LbrPlutusV1.Extended](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).toJson,
        PreludeJson.stringify,
      ),
      Goldens.extendedGoldens(),
    );
  });

  it(`PlutusV1.LowerBound from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.LowerBound\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.LowerBound](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).fromJson,
        LbrPrelude.Json[LbrPlutusV1.LowerBound](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).toJson,
        PreludeJson.stringify,
      ),
      Goldens.lowerBoundGoldens(),
    );
  });

  it(`PlutusV1.UpperBound from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.UpperBound\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.UpperBound](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).fromJson,
        LbrPrelude.Json[LbrPlutusV1.UpperBound](
          LbrPrelude.Json[LbrPlutusV1.POSIXTime],
        ).toJson,
        PreludeJson.stringify,
      ),
      Goldens.upperBoundGoldens(),
    );
  });

  it(`PlutusV1.POSIXTime from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.POSIXTime\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.POSIXTime].fromJson,
        LbrPrelude.Json[LbrPlutusV1.POSIXTime].toJson,
        PreludeJson.stringify,
      ),
      Goldens.posixTimeGoldens(),
    );
  });

  it(`PlutusV1.POSIXTimeRange from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.POSIXTimeRange\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.POSIXTimeRange].fromJson,
        LbrPrelude.Json[LbrPlutusV1.POSIXTimeRange].toJson,
        PreludeJson.stringify,
      ),
      Goldens.posixTimeRangeGoldens(),
    );
  });

  it(`PlutusV1.CurrencySymbol from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.CurrencySymbol\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.CurrencySymbol].fromJson,
        LbrPrelude.Json[LbrPlutusV1.CurrencySymbol].toJson,
        PreludeJson.stringify,
      ),
      [Goldens.adaCurrencySymbolGolden()].concat(
        Goldens.currencySymbolGoldens(),
      ),
    );
  });

  it(`PlutusV1.TokenName from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TokenName\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TokenName].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TokenName].toJson,
        PreludeJson.stringify,
      ),
      Goldens.tokenNameGoldens(),
    );
  });

  it(`PlutusV1.AssetClass from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.AssetClass\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.AssetClass].fromJson,
        LbrPrelude.Json[LbrPlutusV1.AssetClass].toJson,
        PreludeJson.stringify,
      ),
      Goldens.assetClassGoldens(),
    );
  });

  it(`PlutusV1.Value from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Value\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Value].fromJson,
        LbrPrelude.Json[LbrPlutusV1.Value].toJson,
        PreludeJson.stringify,
      ),
      Goldens.valueGoldens(),
    );
  });

  it(`PlutusV1.Redeemer from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Redeemer\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Redeemer].fromJson,
        LbrPrelude.Json[LbrPlutusV1.Redeemer].toJson,
        PreludeJson.stringify,
      ),
      Goldens.redeemerGoldens(),
    );
  });

  it(`PlutusV1.Datum from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Datum\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Datum].fromJson,
        LbrPrelude.Json[LbrPlutusV1.Datum].toJson,
        PreludeJson.stringify,
      ),
      Goldens.datumGoldens(),
    );
  });

  it(`PlutusV1.RedeemerHash from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.RedeemerHash\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.RedeemerHash].fromJson,
        LbrPrelude.Json[LbrPlutusV1.RedeemerHash].toJson,
        PreludeJson.stringify,
      ),
      Goldens.redeemerHashGoldens(),
    );
  });

  it(`PlutusV1.DatumHash from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.DatumHash\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.DatumHash].fromJson,
        LbrPrelude.Json[LbrPlutusV1.DatumHash].toJson,
        PreludeJson.stringify,
      ),
      Goldens.datumHashGoldens(),
    );
  });

  it(`PlutusV1.ScriptHash from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.ScriptHash\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptHash].fromJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptHash].toJson,
        PreludeJson.stringify,
      ),
      Goldens.scriptHashGoldens(),
    );
  });

  it(`PlutusV1.TxId from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TxId\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TxId].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TxId].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txIdGoldens(),
    );
  });

  it(`PlutusV1.TxOutRef from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TxOutRef\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TxOutRef].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TxOutRef].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txOutRefGoldens(),
    );
  });

  it(`PlutusV1.Map from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.Map\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.Map](
          LbrPrelude.Json[LbrPlutusV1.CurrencySymbol],
          LbrPrelude.Json[LbrPlutusV1.Map](
            LbrPrelude.Json[LbrPlutusV1.TokenName],
            LbrPrelude.Json[LbrPrelude.Integer],
          ),
        ).fromJson,
        LbrPrelude.Json[LbrPlutusV1.Map](
          LbrPrelude.Json[LbrPlutusV1.CurrencySymbol],
          LbrPrelude.Json[LbrPlutusV1.Map](
            LbrPrelude.Json[LbrPlutusV1.TokenName],
            LbrPrelude.Json[LbrPrelude.Integer],
          ),
        ).toJson,
        PreludeJson.stringify,
      ),
      Goldens.mapGoldens(),
    );
  });

  it(`PlutusV1.TxInInfo from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TxInInfo\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TxInInfo].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TxInInfo].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txInInfoGoldensV1(),
    );
  });

  it(`PlutusV1.TxOut from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TxOut\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TxOut].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TxOut].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txOutGoldensV1(),
    );
  });

  it(`PlutusV1.DCert from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.DCert\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.DCert].fromJson,
        LbrPrelude.Json[LbrPlutusV1.DCert].toJson,
        PreludeJson.stringify,
      ),
      Goldens.dCertGoldens(),
    );
  });

  it(`PlutusV1.ScriptPurpose from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.ScriptPurpose\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptPurpose].fromJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptPurpose].toJson,
        PreludeJson.stringify,
      ),
      Goldens.scriptPurposeGoldens(),
    );
  });

  it(`PlutusV1.TxInfo from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.TxInfo\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.TxInfo].fromJson,
        LbrPrelude.Json[LbrPlutusV1.TxInfo].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txInfoGoldensV1(),
    );
  });

  it(`PlutusV1.ScriptContext from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV1\.ScriptContext\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptContext].fromJson,
        LbrPrelude.Json[LbrPlutusV1.ScriptContext].toJson,
        PreludeJson.stringify,
      ),
      Goldens.scriptContextGoldensV1(),
    );
  });

  it(`PlutusV2.TxInInfo from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.TxInInfo\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV2.TxInInfo].fromJson,
        LbrPrelude.Json[LbrPlutusV2.TxInInfo].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txInInfoGoldensV2(),
    );
  });

  it(`PlutusV2.OutputDatum from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.OutputDatum\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV2.OutputDatum].fromJson,
        LbrPrelude.Json[LbrPlutusV2.OutputDatum].toJson,
        PreludeJson.stringify,
      ),
      Goldens.outDatumGoldens(),
    );
  });

  it(`PlutusV2.TxOut from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.TxOut\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV2.TxOut].fromJson,
        LbrPrelude.Json[LbrPlutusV2.TxOut].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txOutGoldensV2(),
    );
  });

  it(`PlutusV2.TxInfo from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.TxInfo\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV2.TxInfo].fromJson,
        LbrPrelude.Json[LbrPlutusV2.TxInfo].toJson,
        PreludeJson.stringify,
      ),
      Goldens.txInfoGoldensV2(),
    );
  });

  it(`PlutusV2.ScriptContext from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.ScriptPurpose\.[0-9]*\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        LbrPrelude.Json[LbrPlutusV2.ScriptContext].fromJson,
        LbrPrelude.Json[LbrPlutusV2.ScriptContext].toJson,
        PreludeJson.stringify,
      ),
      Goldens.scriptContextGoldensV2(),
    );
  });
});
