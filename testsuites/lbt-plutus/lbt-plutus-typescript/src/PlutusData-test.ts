import { describe, it } from "node:test";

import * as Utils from "./Utils.js";

import * as Goldens from "./Goldens.js";

import * as LbrPrelude from "lbr-prelude";
import * as PreludeJson from "prelude/Json.js";

import * as LbfFoo from "lbf-plutus-golden-api/LambdaBuffers/Foo.mjs";
import * as LbfDays from "lbf-plutus-golden-api/LambdaBuffers/Days.mjs";

import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as LbrPlutusV2 from "lbr-plutus/V2.js";

/**
 * Loosely, we're testing something along the lines of:
 * ```
 * toJson . toPlutusData . fromPlutusData . fromJson
 * ```
 */
describe("PlutusData tests (toJson . toPlutusData . fromPlutusData . fromJson)", () => {
  const goldenDir = `data/lbt-plutus-golden-data`;

  describe("Foo tests", () => {
    it(`Foo.A from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.A\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.A].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.A].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.aGoldens(),
      );
    });

    it(`Foo.B from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.B\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.B].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.B].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.bGoldens(),
      );
    });

    it(`Foo.C from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.C\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.C].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.C].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.cGoldens(),
      );
    });

    it(`Foo.D from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.D\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.D].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.D].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.dGoldens(),
      );
    });

    it(`Foo.FInt from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.FInt\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.FInt].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.FInt].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.fIntGoldens(),
      );
    });

    it(`Foo.GInt from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Foo\.GInt\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfFoo.GInt].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfFoo.GInt].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.gIntGoldens(),
      );
    });
  });

  describe("Day tests", () => {
    it(`Days.Day from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Days\.Day\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfDays.Day].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfDays.Day].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.dayGoldens(),
      );
    });

    it(`Days.WorkDay from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Days\.WorkDay\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfDays.WorkDay].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfDays.WorkDay].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.workDayGoldens(),
      );
    });

    it(`Days.FreeDay from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Days\.FreeDay\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbfDays.FreeDay].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbfDays.FreeDay].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.freeDayGoldens(),
      );
    });
  });

  describe("Plutus tests", () => {
    it(`PlutusV1.PlutusData from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.PlutusData\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.PlutusData].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.PlutusData].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.plutusDataGoldens(),
      );
    });

    it(`PlutusV1.Address from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Address\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Address].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Address].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.addressGoldens(),
      );
    });

    it(`PlutusV1.Credential from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Credential\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Credential].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Credential].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.credentialGoldens(),
      );
    });

    it(`PlutusV1.StakingCredential from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.StakingCredential\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.StakingCredential].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.StakingCredential].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.stakingCredentialGoldens(),
      );
    });

    it(`PlutusV1.PubKeyHash from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.PubKeyHash\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.PubKeyHash].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.PubKeyHash].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.pubKeyHashGoldens(),
      );
    });

    it(`PlutusV1.Bytes from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Bytes\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.LedgerBytes].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.LedgerBytes].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.bytesGoldens(),
      );
    });

    it(`PlutusV1.Interval from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Interval\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Interval](
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Interval](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.intervalGoldens(),
      );
    });

    it(`PlutusV1.Extended from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Extended\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Extended](
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Extended](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.extendedGoldens(),
      );
    });

    it(`PlutusV1.LowerBound from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.LowerBound\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.LowerBound](
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.LowerBound](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.lowerBoundGoldens(),
      );
    });

    it(`PlutusV1.UpperBound from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.UpperBound\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.UpperBound](
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.UpperBound](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.upperBoundGoldens(),
      );
    });

    it(`PlutusV1.POSIXTime from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.POSIXTime\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTime].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.posixTimeGoldens(),
      );
    });

    it(`PlutusV1.POSIXTimeRange from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.POSIXTimeRange\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTimeRange].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.POSIXTimeRange].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.posixTimeRangeGoldens(),
      );
    });

    it(`PlutusV1.CurrencySymbol from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.CurrencySymbol\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.CurrencySymbol].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.CurrencySymbol].toData(v),
            ),
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
        new Utils.RegExpFileFilter(/^PlutusV1\.TokenName\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TokenName].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TokenName].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.tokenNameGoldens(),
      );
    });

    it(`PlutusV1.AssetClass from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.AssetClass\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.AssetClass].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.AssetClass].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.assetClassGoldens(),
      );
    });

    it(`PlutusV1.Value from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Value\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Value].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Value].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.valueGoldens(),
      );
    });

    it(`PlutusV1.Redeemer from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Redeemer\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Redeemer].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Redeemer].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.redeemerGoldens(),
      );
    });

    it(`PlutusV1.Datum from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Datum\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Datum].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Datum].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.datumGoldens(),
      );
    });

    it(`PlutusV1.RedeemerHash from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.RedeemerHash\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.RedeemerHash].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.RedeemerHash].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.redeemerHashGoldens(),
      );
    });

    it(`PlutusV1.DatumHash from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.DatumHash\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.DatumHash].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.DatumHash].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.datumHashGoldens(),
      );
    });

    it(`PlutusV1.ScriptHash from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.ScriptHash\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptHash].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptHash].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.scriptHashGoldens(),
      );
    });

    it(`PlutusV1.TxId from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.TxId\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxId].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxId].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txIdGoldens(),
      );
    });

    it(`PlutusV1.TxOutRef from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.TxOutRef\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxOutRef].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxOutRef].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txOutRefGoldens(),
      );
    });

    it(`PlutusV1.Map from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.Map\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.Map](
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.CurrencySymbol],
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Map](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.TokenName],
                LbrPlutusV1.IsPlutusData[LbrPrelude.Integer],
              ),
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.Map](
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.CurrencySymbol],
                LbrPlutusV1.IsPlutusData[LbrPlutusV1.Map](
                  LbrPlutusV1.IsPlutusData[LbrPlutusV1.TokenName],
                  LbrPlutusV1.IsPlutusData[LbrPrelude.Integer],
                ),
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.mapGoldens(),
      );
    });

    it(`PlutusV1.TxInInfo from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.TxInInfo\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxInInfo].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxInInfo].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txInInfoGoldensV1(),
      );
    });

    it(`PlutusV1.TxOut from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.TxOut\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxOut].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxOut].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txOutGoldensV1(),
      );
    });

    it(`PlutusV1.DCert from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.DCert\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.DCert].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.DCert].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.dCertGoldens(),
      );
    });

    it(`PlutusV1.ScriptPurpose from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.ScriptPurpose\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptPurpose].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptPurpose].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.scriptPurposeGoldens(),
      );
    });

    it(`PlutusV1.TxInfo from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV1\.TxInfo\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxInfo].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.TxInfo].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txInfoGoldensV1(),
      );
    });

    it(`PlutusV1.ScriptContext from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV1\.ScriptContext\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptContext].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV1.ScriptContext].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.scriptContextGoldensV1(),
      );
    });

    it(`PlutusV2.TxInInfo from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV2\.TxInInfo\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxInInfo].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxInInfo].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txInInfoGoldensV2(),
      );
    });

    it(`PlutusV2.OutputDatum from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(
          /^PlutusV2\.OutputDatum\.[0-9]*\.pd\.json$/g,
        ),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV2.OutputDatum].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV2.OutputDatum].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.outDatumGoldens(),
      );
    });

    it(`PlutusV2.TxOut from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^PlutusV2\.TxOut\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxOut].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxOut].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.txOutGoldensV2(),
      );
    });
  });

  it(`PlutusV2.TxInfo from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(/^PlutusV2\.TxInfo\.[0-9]*\.pd\.json$/g),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        (v) =>
          LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxInfo].fromData(
            LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
          ),
        (v) =>
          LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
            LbrPlutusV1.IsPlutusData[LbrPlutusV2.TxInfo].toData(v),
          ),
        PreludeJson.stringify,
      ),
      Goldens.txInfoGoldensV2(),
    );
  });

  it(`PlutusV2.ScriptContext from to golden tests`, async () => {
    await Utils.fromToGoldenTest(
      goldenDir,
      new Utils.RegExpFileFilter(
        /^PlutusV2\.ScriptContext\.[0-9]*\.pd\.json$/g,
      ),
      Utils.mkFromToAssertGolden(
        PreludeJson.parseJson,
        (v) =>
          LbrPlutusV1.IsPlutusData[LbrPlutusV2.ScriptContext].fromData(
            LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
          ),
        (v) =>
          LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
            LbrPlutusV1.IsPlutusData[LbrPlutusV2.ScriptContext].toData(v),
          ),
        PreludeJson.stringify,
      ),
      Goldens.scriptContextGoldensV2(),
    );
  });

  describe("Prelude tests", () => {
    it(`Prelude.Bool from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Prelude\.Bool\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPrelude.Bool].fromData(
              LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v),
            ),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPrelude.Bool].toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.boolGoldens(),
      );
    });

    it(`Prelude.Maybe from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Prelude\.Maybe\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPrelude.Maybe](
              LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPrelude.Maybe](
                LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.maybeGoldens(),
      );
    });

    it(`Prelude.Either from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Prelude\.Either\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPrelude.Either](
              LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
              LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPrelude.Either](
                LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
                LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.eitherGoldens(),
      );
    });

    it(`Prelude.List from to golden tests`, async () => {
      await Utils.fromToGoldenTest(
        goldenDir,
        new Utils.RegExpFileFilter(/^Prelude\.List\.[0-9]*\.pd\.json$/g),
        Utils.mkFromToAssertGolden(
          PreludeJson.parseJson,
          (v) =>
            LbrPlutusV1.IsPlutusData[LbrPrelude.List](
              LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
            ).fromData(LbrPrelude.Json[LbrPlutusV1.PlutusData].fromJson(v)),
          (v) =>
            LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(
              LbrPlutusV1.IsPlutusData[LbrPrelude.List](
                LbrPlutusV1.IsPlutusData[LbrPrelude.Bool],
              ).toData(v),
            ),
          PreludeJson.stringify,
        ),
        Goldens.listGoldens(),
      );
    });
  });
});
