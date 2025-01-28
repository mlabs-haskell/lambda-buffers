import * as LbrPrelude from "lbr-prelude";
import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as LbrPlutusV2 from "lbr-plutus/V2.js";
import * as LbrPlutusV3 from "lbr-plutus/V3.js";

import * as PlaV1 from "plutus-ledger-api/V1.js";
import * as PlaV3 from "plutus-ledger-api/V3.js";
import * as PlaMap from "plutus-ledger-api/AssocMap.js";

import * as LbfFooBar from "lbf-plutus-golden-api/LambdaBuffers/Foo/Bar.mjs";
import * as LbfFoo from "lbf-plutus-golden-api/LambdaBuffers/Foo.mjs";
import * as LbfDays from "lbf-plutus-golden-api/LambdaBuffers/Days.mjs";

const MAX_TEST_CASES = 10;

/**
 * Hard coded bytes for testing
 */
export function someBytes(): LbrPlutusV1.LedgerBytes {
  return Uint8Array.from([115, 111, 109, 101, 32, 98, 121, 116, 101, 115]);
}

/**
 * Hard coded bytes for testing
 */
export function someMoreBytes(): LbrPlutusV1.LedgerBytes {
  return Uint8Array.from([
    115,
    111,
    109,
    101,
    32,
    109,
    111,
    114,
    101,
    32,
    98,
    121,
    116,
    101,
    115,
  ]);
}

/**
 * Hard coded bytes for testing
 */
export function emptyBytes(): LbrPlutusV1.LedgerBytes {
  return Uint8Array.from([]);
}

/**
 * Hard coded bytes for testing
 */
export function nullBytes(): LbrPlutusV1.LedgerBytes {
  return Uint8Array.from([0]);
}

/*
 * Plutus.V1 goldens
 */

/**
 * Hard coded {@link PlutusData} goldens
 */
export function plutusDataGoldens(): LbrPrelude.List<LbrPlutusV1.PlutusData> {
  return [
    { name: "Constr", fields: [0n, []] },

    {
      name: "Constr",
      fields: [
        1n,
        [
          { name: "Integer", fields: 1n },
          { name: "Bytes", fields: someBytes() },
        ],
      ],
    },
    { name: "List", fields: [] },

    {
      name: "List",
      fields: [
        { name: "Integer", fields: 1n },
        { name: "Integer", fields: 2n },
      ],
    },
    {
      name: "List",
      fields: [
        { name: "Integer", fields: 1n },
        { name: "Bytes", fields: someBytes() },
      ],
    },
    { name: "Map", fields: PlaMap.fromList([]) },
    {
      name: "Map",
      fields: PlaMap.fromList([
        [
          { name: "Integer", fields: 1n },
          {
            name: "Bytes",
            fields: someBytes(),
          },
        ],
        [
          { name: "Integer", fields: 2n },
          {
            name: "Bytes",
            fields: someMoreBytes(),
          },
        ],
      ]),
    },
    { name: "Integer", fields: 0n },
    { name: "Integer", fields: 1n },
    { name: "Integer", fields: -1n },
    { name: "Bytes", fields: emptyBytes() },
    { name: "Bytes", fields: nullBytes() },
    { name: "Bytes", fields: someBytes() },
  ];
}

/**
 * Hard coded bytes test
 */
export function blake2b_256Hash(): LbrPlutusV1.LedgerBytes {
  const arr = [];
  for (let i = 1; i <= 32; ++i) {
    arr.push(i);
  }

  return Uint8Array.from(arr);
}

/**
 * Hard coded bytes test
 */
export function blake2b_224Hash(): LbrPlutusV1.LedgerBytes {
  const arr = [];
  for (let i = 1; i <= 28; ++i) {
    arr.push(i);
  }

  return Uint8Array.from(arr);
}

/**
 * Hard coded {@link Address} test
 */
export function addressGoldens(): LbrPrelude.List<LbrPlutusV1.Address> {
  const res: LbrPrelude.List<LbrPlutusV1.Address> = [];

  for (const credential of credentialGoldens()) {
    const mStakingCredentials: LbrPrelude.List<
      LbrPrelude.Maybe<LbrPlutusV1.StakingCredential>
    > = [{ name: "Nothing" }];
    for (const mStakingCredential of mStakingCredentials) {
      res.push({
        addressCredential: credential,
        addressStakingCredential: mStakingCredential,
      });
    }
  }

  for (const credential of credentialGoldens()) {
    for (const stakingCredential of stakingCredentialGoldens()) {
      const mStakingCredential: LbrPrelude.Maybe<
        LbrPlutusV1.StakingCredential
      > = { name: "Just", fields: stakingCredential };
      res.push({
        addressCredential: credential,
        addressStakingCredential: mStakingCredential,
      });
    }
  }

  return res;
}

/**
 * Hard coded {@link Credential} test
 */
export function credentialGoldens(): LbrPrelude.List<LbrPlutusV1.Credential> {
  const res: LbrPrelude.List<LbrPlutusV1.Credential> = [];

  for (const pubKeyHash of pubKeyHashGoldens()) {
    res.push({ name: "PubKeyCredential", fields: pubKeyHash });
  }

  for (const scriptHash of scriptHashGoldens()) {
    res.push({ name: "ScriptCredential", fields: scriptHash });
  }

  return res;
}

function unsafeFromJust<A>(maybe: LbrPrelude.Maybe<A>): A {
  if (maybe.name === "Just") {
    return maybe.fields;
  } else {
    throw new Error(`unsafeFromJust error: got Nothing but expected Just`);
  }
}

/**
 * Hard coded {@link PubKeyHash} test
 */
export function pubKeyHashGoldens(): LbrPrelude.List<LbrPlutusV1.PubKeyHash> {
  return [unsafeFromJust(PlaV1.pubKeyHashFromBytes(blake2b_224Hash()))];
}

/**
 * Hard coded {@link ScriptHash} test
 */
export function scriptHashGoldens(): LbrPrelude.List<LbrPlutusV1.ScriptHash> {
  return [unsafeFromJust(PlaV1.scriptHashFromBytes(blake2b_224Hash()))];
}

/**
 * Hard coded {@link StakingCredential} test
 */
export function stakingCredentialGoldens(): LbrPrelude.List<
  LbrPlutusV1.StakingCredential
> {
  const res: LbrPrelude.List<LbrPlutusV1.StakingCredential> = [];

  for (const credential of credentialGoldens()) {
    res.push({ name: "StakingHash", fields: credential });
  }

  res.push({ name: "StakingPtr", fields: [0n, 1n, 2n] });

  return res;
}

/**
 * Hard coded {@link LedgerBytes} tests
 */
export function bytesGoldens(): LbrPrelude.List<LbrPlutusV1.LedgerBytes> {
  return [emptyBytes(), nullBytes(), someBytes()];
}

/**
 * Hard coded {@link Interval} tests
 */
export function intervalGoldens(): LbrPrelude.List<
  LbrPlutusV1.Interval<LbrPlutusV1.POSIXTime>
> {
  const res: LbrPrelude.List<LbrPlutusV1.Interval<LbrPlutusV1.POSIXTime>> = [];

  for (const lb of lowerBoundGoldens()) {
    for (const ub of upperBoundGoldens()) {
      res.push({ ivFrom: lb, ivTo: ub });
    }
  }

  return res;
}

/**
 * Hard coded {@link LowerBound} tests
 */
export function lowerBoundGoldens(): LbrPrelude.List<
  LbrPlutusV1.LowerBound<LbrPlutusV1.POSIXTime>
> {
  const res: LbrPrelude.List<LbrPlutusV1.LowerBound<LbrPlutusV1.POSIXTime>> =
    [];

  for (const extended of extendedGoldens()) {
    for (const closure of closureGoldens()) {
      res.push([extended, closure]);
    }
  }

  return res;
}

/**
 * Hard coded {@link UpperBound} tests
 */
export function upperBoundGoldens(): LbrPrelude.List<
  LbrPlutusV1.UpperBound<LbrPlutusV1.POSIXTime>
> {
  const res: LbrPrelude.List<LbrPlutusV1.UpperBound<LbrPlutusV1.POSIXTime>> =
    [];

  for (const extended of extendedGoldens()) {
    for (const closure of closureGoldens()) {
      res.push([extended, closure]);
    }
  }

  return res;
}

/**
 * Hard coded {@link Extended} tests
 */
export function extendedGoldens(): LbrPrelude.List<
  LbrPlutusV1.Extended<LbrPlutusV1.POSIXTime>
> {
  return [
    { name: "NegInf" },
    { name: "PosInf" },
    {
      name: "Finite",
      fields: 0n,
    },
  ];
}

/**
 * Hard coded {@link Closure} tests
 */
export function closureGoldens(): LbrPrelude.List<LbrPlutusV1.Closure> {
  return [true, false];
}

/**
 * Hard coded {@link POSIXTime} tests
 */
export function posixTimeGoldens(): LbrPrelude.List<LbrPlutusV1.POSIXTime> {
  return [0n, 1n, 2n];
}

/**
 * Hard coded {@link POSIXTimeRange} tests
 */
export function posixTimeRangeGoldens(): LbrPrelude.List<
  LbrPlutusV1.POSIXTimeRange
> {
  return intervalGoldens();
}

/**
 * Hard coded {@link CurrencySymbol} tests
 */
export function currencySymbolGoldens(): LbrPrelude.List<
  LbrPlutusV1.CurrencySymbol
> {
  return [unsafeFromJust(PlaV1.currencySymbolFromBytes(blake2b_224Hash()))];
}

/**
 * Hard coded ada {@link CurrencySymbol} test
 */
export function adaCurrencySymbolGolden(): LbrPlutusV1.CurrencySymbol {
  return PlaV1.adaSymbol;
}

/**
 * Hard coded {@link TokenName} tests
 */
export function tokenNameGoldens(): LbrPrelude.List<LbrPlutusV1.TokenName> {
  const tn1 = unsafeFromJust(PlaV1.tokenNameFromBytes(emptyBytes()));
  const tn2 = unsafeFromJust(
    PlaV1.tokenNameFromBytes(
      ((arr: LbrPrelude.List<number>) => {
        for (let i = 1; i < 16; ++i) {
          arr.push(i);
        }
        return Uint8Array.from(arr);
      })([]),
    ),
  );
  const tn3 = unsafeFromJust(
    PlaV1.tokenNameFromBytes(
      ((arr: LbrPrelude.List<number>) => {
        for (let i = 1; i < 32; ++i) {
          arr.push(i);
        }
        return Uint8Array.from(arr);
      })([]),
    ),
  );

  return [tn1, tn2, tn3];
}

/**
 * Hard coded {@link AssetClass} tests
 */
export function assetClassGoldens(): LbrPrelude.List<LbrPlutusV1.AssetClass> {
  const res: LbrPrelude.List<LbrPlutusV1.AssetClass> = [];

  for (const currencySymbol of currencySymbolGoldens()) {
    for (const tokenName of tokenNameGoldens()) {
      res.push([currencySymbol, tokenName]);
    }
  }

  res.push([PlaV1.adaSymbol, PlaV1.adaToken]);

  return res;
}

/**
 * Hard coded {@link Value} tests
 */
export function valueGoldens(): LbrPrelude.List<LbrPlutusV1.Value> {
  const res: LbrPrelude.List<LbrPlutusV1.Value> = [];

  for (const map of mapGoldens()) {
    res.push(map);
  }

  return res;
}

/**
 * Hard coded {@link Map} tests
 */
export function mapGoldens(): LbrPrelude.List<
  LbrPlutusV1.Map<
    LbrPlutusV1.CurrencySymbol,
    LbrPlutusV1.Map<LbrPlutusV1.TokenName, LbrPrelude.Integer>
  >
> {
  return [
    PlaMap.fromList([]),
    PlaMap.fromList<
      LbrPlutusV1.CurrencySymbol,
      LbrPlutusV1.Map<LbrPlutusV1.TokenName, LbrPrelude.Integer>
    >([
      [
        PlaV1.adaSymbol,
        PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>([
          [PlaV1.adaToken, 1337n],
        ]),
      ],
    ]),
    PlaMap.fromList<
      LbrPlutusV1.CurrencySymbol,
      LbrPlutusV1.Map<LbrPlutusV1.TokenName, LbrPrelude.Integer>
    >([
      [
        PlaV1.adaSymbol,
        PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>([
          [PlaV1.adaToken, 1337n],
        ]),
      ],

      [
        unsafeFromJust(PlaV1.currencySymbolFromBytes(blake2b_224Hash())),
        PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>([
          [unsafeFromJust(PlaV1.tokenNameFromBytes(emptyBytes())), 1337n],
          [
            unsafeFromJust(
              PlaV1.tokenNameFromBytes(
                ((arr: LbrPrelude.List<number>) => {
                  for (let i = 1; i < 16; ++i) {
                    arr.push(i);
                  }
                  return Uint8Array.from(arr);
                })([]),
              ),
            ),
            16n,
          ],
          [
            unsafeFromJust(
              PlaV1.tokenNameFromBytes(
                ((arr: LbrPrelude.List<number>) => {
                  for (let i = 1; i < 32; ++i) {
                    arr.push(i);
                  }
                  return Uint8Array.from(arr);
                })([]),
              ),
            ),
            32n,
          ],
        ]),
      ],
    ]),
  ];
}

/**
 * Hard coded {@link Redeemer} tests
 */
export function redeemerGoldens(): LbrPrelude.List<LbrPlutusV1.Redeemer> {
  return [{ name: "Integer", fields: 1337n }];
}

/**
 * Hard coded {@link Datum} tests
 */
export function datumGoldens(): LbrPrelude.List<LbrPlutusV1.Datum> {
  return [{ name: "Integer", fields: 1337n }];
}

/**
 * Hard coded {@link RedeemerHash} tests
 */
export function redeemerHashGoldens(): LbrPrelude.List<
  LbrPlutusV1.RedeemerHash
> {
  return [unsafeFromJust(PlaV1.redeemerHashFromBytes(blake2b_256Hash()))];
}

/**
 * Hard coded {@link DatumHash} tests
 */
export function datumHashGoldens(): LbrPrelude.List<LbrPlutusV1.DatumHash> {
  return [unsafeFromJust(PlaV1.datumHashFromBytes(blake2b_256Hash()))];
}

/**
 * Hard coded {@link TxId} tests
 */
export function txIdGoldens(): LbrPrelude.List<LbrPlutusV1.TxId> {
  return [unsafeFromJust(PlaV1.txIdFromBytes(blake2b_256Hash()))];
}

/**
 * Hard coded {@link TxOutRef} tests
 */
export function txOutRefGoldens(): LbrPrelude.List<LbrPlutusV1.TxOutRef> {
  const res: LbrPrelude.List<LbrPlutusV1.TxOutRef> = [];

  for (const txId of txIdGoldens()) {
    for (const txIdx of [0n]) {
      res.push({ txOutRefId: txId, txOutRefIdx: txIdx });
    }
  }

  return res;
}

/**
 * Hard coded {@link TxInInfo} tests
 */
export function txInInfoGoldensV1(): LbrPrelude.List<LbrPlutusV1.TxInInfo> {
  const res: LbrPrelude.List<LbrPlutusV1.TxInInfo> = [];

  for (const txOutRef of txOutRefGoldens()) {
    for (const txOut of txOutGoldensV1()) {
      res.push({ txInInfoOutRef: txOutRef, txInInfoResolved: txOut });
    }
  }
  return res;
}

/**
 * Hard coded {@link TxOut} tests
 */
export function txOutGoldensV1(): LbrPrelude.List<LbrPlutusV1.TxOut> {
  const res: LbrPrelude.List<LbrPlutusV1.TxOut> = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datumHash of datumHashGoldens()) {
        const datumHash1: LbrPrelude.Maybe<PlaV1.DatumHash> = {
          fields: datumHash,
          name: "Just",
        };
        const txOut1 = {
          txOutAddress: address,
          txOutValue: value,
          txOutDatumHash: datumHash1,
        };

        res.push(txOut1);
      }
      const datumHash2: LbrPrelude.Maybe<PlaV1.DatumHash> = {
        name: "Nothing",
      };
      const txOut2 = {
        txOutAddress: address,
        txOutValue: value,
        txOutDatumHash: datumHash2,
      };
      res.push(txOut2);
    }
  }
  return res;
}

/**
 * Hard coded {@link DCert} tests
 */
export function dCertGoldens(): LbrPrelude.List<LbrPlutusV1.DCert> {
  const res: LbrPrelude.List<LbrPlutusV1.DCert> = [];

  res.push({ name: "Mir" });
  res.push({ name: "Genesis" });

  for (const pubKeyHash of pubKeyHashGoldens()) {
    res.push({
      fields: [pubKeyHash, 1337n],
      name: "PoolRetire",
    });
  }

  for (const stakingCredential of stakingCredentialGoldens()) {
    res.push({
      fields: stakingCredential,
      name: "DelegRegKey",
    });
  }

  for (const pubKeyHash1 of pubKeyHashGoldens()) {
    for (const pubKeyHash2 of pubKeyHashGoldens()) {
      res.push({
        fields: [pubKeyHash1, pubKeyHash2],
        name: "PoolRegister",
      });
    }
  }

  for (const stakingCredential of stakingCredentialGoldens()) {
    res.push({
      fields: stakingCredential,
      name: "DelegDeRegKey",
    });
  }

  for (const stakingCredential of stakingCredentialGoldens()) {
    for (const pubKeyHash of pubKeyHashGoldens()) {
      res.push({
        fields: [stakingCredential, pubKeyHash],
        name: "DelegDelegate",
      });
    }
  }

  return res;
}

/**
 * Hard coded {@link ScriptPurpose} tests
 */
export function scriptPurposeGoldens(): LbrPrelude.List<
  LbrPlutusV1.ScriptPurpose
> {
  const res: LbrPrelude.List<LbrPlutusV1.ScriptPurpose> = [];

  for (const currencySymbol of currencySymbolGoldens()) {
    res.push({
      fields: currencySymbol,
      name: "Minting",
    });
  }

  for (const txOutRef of txOutRefGoldens()) {
    res.push({
      fields: txOutRef,
      name: "Spending",
    });
  }

  for (const stakingCredential of stakingCredentialGoldens()) {
    res.push({
      fields: stakingCredential,
      name: "Rewarding",
    });
  }

  for (const dCert of dCertGoldens()) {
    res.push({
      fields: dCert,
      name: "Certifying",
    });
  }

  return res;
}

/**
 * Hard coded {@link TxInfo} tests
 */
export function txInfoGoldensV1(): LbrPrelude.List<LbrPlutusV1.TxInfo> {
  const res: LbrPrelude.List<LbrPlutusV1.TxInfo> = [];

  const wdrls: LbrPrelude.List<
    [LbrPlutusV1.StakingCredential, LbrPrelude.Integer]
  > = [];

  for (const stakingCredential of stakingCredentialGoldens()) {
    wdrls.push([stakingCredential, 1234n]);
  }

  const datums: LbrPrelude.List<[LbrPlutusV1.DatumHash, LbrPlutusV1.Datum]> =
    [];

  for (const datumHash of datumHashGoldens()) {
    for (const datum of datumGoldens()) {
      datums.push([datumHash, datum]);
    }
  }

  for (const fee of valueGoldens()) {
    for (const mint of valueGoldens()) {
      for (const validRange of posixTimeRangeGoldens()) {
        for (const id of txIdGoldens()) {
          res.push({
            txInfoInputs: txInInfoGoldensV1(),
            txInfoOutputs: txOutGoldensV1(),
            txInfoFee: fee,
            txInfoMint: mint,
            txInfoDCert: dCertGoldens(),
            txInfoWdrl: wdrls,
            txInfoValidRange: validRange,
            txInfoSignatories: pubKeyHashGoldens(),
            txInfoData: datums,
            txInfoId: id,
          });
        }
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link ScriptContext} tests
 */
export function scriptContextGoldensV1(): LbrPrelude.List<
  LbrPlutusV1.ScriptContext
> {
  const res: LbrPrelude.List<LbrPlutusV1.ScriptContext> = [];

  for (const scriptPurpose of scriptPurposeGoldens()) {
    for (const txInfo of txInfoGoldensV1()) {
      res.push({
        scriptContextPurpose: scriptPurpose,
        scriptContextTxInfo: txInfo,
      });
    }
  }

  return res;
}

/*
 * Plutus.V2 goldens
 */

/**
 * Hard coded {@link  TxInInfo} tests
 */
export function txInInfoGoldensV2(): LbrPrelude.List<LbrPlutusV2.TxInInfo> {
  const res: LbrPrelude.List<LbrPlutusV2.TxInInfo> = [];

  for (const txOutRef of txOutRefGoldens()) {
    for (const txOut of txOutGoldensV2()) {
      res.push({ txInInfoOutRef: txOutRef, txInInfoResolved: txOut });
    }
  }

  return res;
}

/**
 * Hard coded {@link  TxOut} tests
 */
export function txOutGoldensV2(): LbrPrelude.List<LbrPlutusV2.TxOut> {
  const res: LbrPrelude.List<LbrPlutusV2.TxOut> = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      const outDatums = outDatumGoldens();
      for (let outDatumIx = 0; outDatumIx < 1; ++outDatumIx) {
        const mScriptHashes: LbrPrelude.List<
          LbrPrelude.Maybe<LbrPlutusV1.ScriptHash>
        > = [{ name: "Nothing" }];
        mScriptHashes.concat(
          scriptHashGoldens().map((x) => {
            return { fields: x, name: "Just" };
          }),
        );

        for (const mScriptHash of mScriptHashes) {
          res.push({
            txOutAddress: address,
            txOutValue: value,
            txOutDatum: outDatums[outDatumIx]!,
            txOutReferenceScript: mScriptHash,
          });
        }
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link  OutputDatum} tests
 */
export function outDatumGoldens(): LbrPrelude.List<LbrPlutusV2.OutputDatum> {
  const res: LbrPrelude.List<LbrPlutusV2.OutputDatum> = [];

  res.push({ name: "NoOutputDatum" });

  for (const datumHash of datumHashGoldens()) {
    res.push({ name: "OutputDatumHash", fields: datumHash });
  }

  for (const outputDatum of datumGoldens()) {
    res.push({ name: "OutputDatum", fields: outputDatum });
  }

  return res;
}

/**
 * Hard coded {@link TxInfo} tests
 */
export function txInfoGoldensV2(): LbrPrelude.List<LbrPlutusV2.TxInfo> {
  const res: LbrPrelude.List<LbrPlutusV2.TxInfo> = [];

  const wdrls: PlaMap.Map<LbrPlutusV1.StakingCredential, LbrPrelude.Integer> =
    PlaMap.empty();

  for (const stakingCredential of stakingCredentialGoldens()) {
    PlaMap.insert(PlaV1.eqStakingCredential, stakingCredential, 1234n, wdrls);
  }

  const datums: PlaMap.Map<LbrPlutusV1.DatumHash, LbrPlutusV1.Datum> = PlaMap
    .empty();

  for (const datumHash of datumHashGoldens()) {
    for (const datum of datumGoldens()) {
      PlaMap.insert(PlaV1.eqDatumHash, datumHash, datum, datums);
    }
  }

  const redeemers: PlaMap.Map<LbrPlutusV1.ScriptPurpose, LbrPlutusV1.Redeemer> =
    PlaMap.empty();

  for (const scriptPurpose of scriptPurposeGoldens()) {
    for (const redeemer of redeemerGoldens()) {
      PlaMap.insert(PlaV1.eqScriptPurpose, scriptPurpose, redeemer, redeemers);
    }
  }

  for (const fee of valueGoldens()) {
    for (const mint of valueGoldens()) {
      for (const validRange of posixTimeRangeGoldens()) {
        for (const id of txIdGoldens()) {
          res.push({
            txInfoInputs: txInInfoGoldensV2(),
            txInfoReferenceInputs: txInInfoGoldensV2(),
            txInfoOutputs: txOutGoldensV2(),
            txInfoFee: fee,
            txInfoMint: mint,
            txInfoDCert: dCertGoldens(),
            txInfoWdrl: wdrls,
            txInfoValidRange: validRange,
            txInfoSignatories: pubKeyHashGoldens(),
            txInfoRedeemers: redeemers,
            txInfoData: datums,
            txInfoId: id,
          });
        }
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link ScriptContext} tests
 */
export function scriptContextGoldensV2(): LbrPrelude.List<
  LbrPlutusV2.ScriptContext
> {
  const res: LbrPrelude.List<LbrPlutusV2.ScriptContext> = [];

  for (const scriptPurpose of scriptPurposeGoldens()) {
    for (const txInfo of txInfoGoldensV2()) {
      res.push({
        scriptContextPurpose: scriptPurpose,
        scriptContextTxInfo: txInfo,
      });
    }
  }

  return res;
}

/*
 * Plutus.V3 goldens
 */

/**
 * Hard coded {@link Rational} tests
 */
export function rationalGoldensV3(): LbrPrelude.List<LbrPlutusV3.Rational> {
  return [{ numerator: 1n, denominator: 2n }];
}

/**
 * Hard coded {@link TxId} tests
 */
export function txIdGoldensV3(): LbrPrelude.List<LbrPlutusV3.TxId> {
  return [unsafeFromJust(PlaV3.txIdFromBytes(blake2b_256Hash()))];
}

/**
 * Hard coded {@link TxOutRef} tests
 */
export function txOutRefGoldensV3(): LbrPrelude.List<LbrPlutusV3.TxOutRef> {
  const res: LbrPrelude.List<LbrPlutusV3.TxOutRef> = [];

  for (const txId of txIdGoldens()) {
    for (const txIdx of [0n]) {
      res.push({ txOutRefId: txId, txOutRefIdx: txIdx });
    }
  }

  return res;
}

/**
 * Hard coded {@link ColdCommitteeCredential} tests
 */
export function coldCommitteeCredentialGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ColdCommitteeCredential
> {
  return credentialGoldens();
}

/**
 * Hard coded {@link HotCommitteeCredential} tests
 */
export function hotCommitteeCredentialGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.HotCommitteeCredential
> {
  return credentialGoldens();
}

/**
 * Hard coded {@link DRepCredential} tests
 */
export function drepCredentialGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.DRepCredential
> {
  return credentialGoldens();
}

/**
 * Hard coded {@link DRep} tests
 */
export function drepGoldensV3(): LbrPrelude.List<LbrPlutusV3.DRep> {
  const res: LbrPrelude.List<LbrPlutusV3.DRep> = [];

  for (const drepCred of drepCredentialGoldensV3()) {
    res.push({ name: "DRep", fields: drepCred });
  }

  res.push({ name: "AlwaysAbstain" });

  res.push({ name: "AlwaysNoConfidence" });

  return res;
}

/**
 * Hard coded {@link DRep} tests
 */
export function delegateeGoldensV3(): LbrPrelude.List<LbrPlutusV3.Delegatee> {
  const res: LbrPrelude.List<LbrPlutusV3.Delegatee> = [];

  for (const pkh of pubKeyHashGoldens()) {
    res.push({ name: "Stake", fields: pkh });
  }

  for (const drep of drepGoldensV3()) {
    res.push({ name: "Vote", fields: drep });
  }

  for (const pkh of pubKeyHashGoldens()) {
    for (const drep of drepGoldensV3()) {
      res.push({ name: "StakeVote", fields: [pkh, drep] });
    }
  }

  for (const drep of drepGoldensV3()) {
    res.push({ name: "Vote", fields: drep });
  }

  return res;
}

/**
 * Hard coded {@link Lovelace} tests
 */
export function lovelaceGoldensV3(): LbrPrelude.List<PlaV1.Lovelace> {
  return [0n];
}

/**
 * Hard coded {@link TxCert} tests
 */
export function txCertGoldensV3(): LbrPrelude.List<LbrPlutusV3.TxCert> {
  const res: LbrPrelude.List<LbrPlutusV3.TxCert> = [];

  for (const cred of credentialGoldens()) {
    for (const lovelace of toMaybe(lovelaceGoldensV3())) {
      res.push({ name: "RegStaking", fields: [cred, lovelace] });
    }
  }

  for (const cred of credentialGoldens()) {
    for (const lovelace of toMaybe(lovelaceGoldensV3())) {
      res.push({ name: "UnRegStaking", fields: [cred, lovelace] });
    }
  }

  for (const cred of credentialGoldens()) {
    for (const delegatee of delegateeGoldensV3()) {
      res.push({ name: "DelegStaking", fields: [cred, delegatee] });
    }
  }

  for (const cred of credentialGoldens()) {
    for (const delegatee of delegateeGoldensV3()) {
      for (const lovelace of lovelaceGoldensV3()) {
        res.push({ name: "RegDeleg", fields: [cred, delegatee, lovelace] });
      }
    }
  }

  for (const cred of drepCredentialGoldensV3()) {
    for (const lovelace of lovelaceGoldensV3()) {
      res.push({ name: "RegDRep", fields: [cred, lovelace] });
    }
  }

  for (const cred of drepCredentialGoldensV3()) {
    res.push({ name: "UpdateDRep", fields: cred });
  }

  for (const cred of drepCredentialGoldensV3()) {
    for (const lovelace of lovelaceGoldensV3()) {
      res.push({ name: "UnRegDRep", fields: [cred, lovelace] });
    }
  }

  for (const pkh1 of pubKeyHashGoldens()) {
    for (const pkh2 of pubKeyHashGoldens()) {
      res.push({ name: "PoolRegister", fields: [pkh1, pkh2] });
    }
  }

  for (const pkh of pubKeyHashGoldens()) {
    res.push({ name: "PoolRetire", fields: [pkh, 0n] });
  }

  for (const coldCommitteeCred of coldCommitteeCredentialGoldensV3()) {
    for (const hotCommitteeCred of hotCommitteeCredentialGoldensV3()) {
      res.push({
        name: "AuthHotCommittee",
        fields: [coldCommitteeCred, hotCommitteeCred],
      });
    }
  }

  for (const coldCommitteeCred of coldCommitteeCredentialGoldensV3()) {
    res.push({ name: "ResignColdCommittee", fields: coldCommitteeCred });
  }

  return res;
}

/**
 * Hard coded {@link Voter} tests
 */
export function voterGoldensV3(): LbrPrelude.List<LbrPlutusV3.Voter> {
  const res: LbrPrelude.List<LbrPlutusV3.Voter> = [];

  for (const hotCommitteeCred of hotCommitteeCredentialGoldensV3()) {
    res.push({ name: "CommitteeVoter", fields: hotCommitteeCred });
  }

  for (const drepCred of drepCredentialGoldensV3()) {
    res.push({ name: "DRepVoter", fields: drepCred });
  }

  for (const pkh of pubKeyHashGoldens()) {
    res.push({ name: "StakePoolVoter", fields: pkh });
  }

  return res;
}

/**
 * Hard coded {@link Vote} tests
 */
export function voteGoldensV3(): LbrPrelude.List<LbrPlutusV3.Vote> {
  return [{ name: "VoteNo" }, { name: "VoteYes" }, { name: "Abstain" }];
}

/**
 * Hard coded {@link GovernanceActionId} tests
 */
export function governanceActionIdGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.GovernanceActionId
> {
  const res: LbrPrelude.List<LbrPlutusV3.GovernanceActionId> = [];

  for (const gaidTxId of txIdGoldensV3()) {
    res.push({
      gaidTxId,
      gaidGovActionIx: 0n,
    });
  }

  return res;
}

/**
 * Hard coded {@link Committee} tests
 */
export function committeeGoldensV3(): LbrPrelude.List<LbrPlutusV3.Committee> {
  const res: LbrPrelude.List<LbrPlutusV3.Committee> = [];

  for (const committeeQuorum of rationalGoldensV3()) {
    res.push({
      committeeMembers: PlaMap.fromList(
        coldCommitteeCredentialGoldensV3().map((cc) => [cc, 0n]),
      ),
      committeeQuorum,
    });
  }

  return res;
}

/**
 * Hard coded {@link Constitution} tests
 */
export function constitutionGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.Constitution
> {
  const res: LbrPrelude.List<LbrPlutusV3.Constitution> = [];

  for (const constitutionScript of toMaybe(scriptHashGoldens())) {
    res.push({
      constitutionScript,
    });
  }

  return res;
}

/**
 * Hard coded {@link ProtocolVersion} tests
 */
export function protocolVersionGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ProtocolVersion
> {
  return [{ pvMajor: 1n, pvMinor: 2n }];
}

/**
 * Hard coded {@link ChangedParameters} tests
 */
export function changedParametersGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ChangedParameters
> {
  return plutusDataGoldens();
}

/**
 * Hard coded {@link GovernanceAction} tests
 */
export function governanceActionGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.GovernanceAction
> {
  const res: LbrPrelude.List<LbrPlutusV3.GovernanceAction> = [];

  for (const govActionId of toMaybe(governanceActionIdGoldensV3())) {
    for (const changedParams of changedParametersGoldensV3()) {
      for (const scriptHash of toMaybe(scriptHashGoldens())) {
        res.push({
          name: "ParameterChange",
          fields: [govActionId, changedParams, scriptHash],
        });
      }
    }
  }

  for (const govActionId of toMaybe(governanceActionIdGoldensV3())) {
    for (const protocolVersion of protocolVersionGoldensV3()) {
      res.push({
        name: "HardForkInitiation",
        fields: [govActionId, protocolVersion],
      });
    }
  }

  for (const scriptHash of toMaybe(scriptHashGoldens())) {
    res.push({
      name: "TreasuryWithdrawal",
      fields: [toMap(credentialGoldens(), lovelaceGoldensV3()), scriptHash],
    });
  }

  for (const govActionId of toMaybe(governanceActionIdGoldensV3())) {
    res.push({
      name: "NoConfidence",
      fields: govActionId,
    });
  }

  for (const govActionId of toMaybe(governanceActionIdGoldensV3())) {
    for (const rational of rationalGoldensV3()) {
      res.push({
        name: "UpdateCommittee",
        fields: [
          govActionId,
          coldCommitteeCredentialGoldensV3(),
          toMap(coldCommitteeCredentialGoldensV3(), [0n]),
          rational,
        ],
      });
    }
  }

  for (const govActionId of toMaybe(governanceActionIdGoldensV3())) {
    for (const constitution of constitutionGoldensV3()) {
      res.push({
        name: "NewConstitution",
        fields: [
          govActionId,
          constitution,
        ],
      });
    }
  }

  return res;
}

/**
 * Hard coded {@link ProposalProcedure} tests
 */
export function proposalProcedureGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ProposalProcedure
> {
  const res: LbrPrelude.List<LbrPlutusV3.ProposalProcedure> = [];

  for (const ppDeposit of lovelaceGoldensV3()) {
    for (const ppReturnAddr of credentialGoldens()) {
      for (const ppGovernanceAction of governanceActionGoldensV3()) {
        res.push({
          ppDeposit,
          ppReturnAddr,
          ppGovernanceAction,
        });
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link ScriptPurpose} tests
 */
export function scriptPurposeGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ScriptPurpose
> {
  const res: LbrPrelude.List<LbrPlutusV3.ScriptPurpose> = [];

  for (const currencySymbol of currencySymbolGoldens()) {
    res.push({
      fields: currencySymbol,
      name: "Minting",
    });
  }

  for (const txOutRef of txOutRefGoldens()) {
    res.push({
      fields: txOutRef,
      name: "Spending",
    });
  }

  for (const credential of credentialGoldens()) {
    res.push({
      fields: credential,
      name: "Rewarding",
    });
  }

  for (const txCert of txCertGoldensV3()) {
    res.push({
      fields: [0n, txCert],
      name: "Certifying",
    });
  }

  for (const voter of voterGoldensV3()) {
    res.push({
      fields: voter,
      name: "Voting",
    });
  }

  for (const proposalProcedure of proposalProcedureGoldensV3()) {
    res.push({
      fields: [0n, proposalProcedure],
      name: "Proposing",
    });
  }

  return res;
}

/**
 * Hard coded {@link ScriptPurpose} tests
 */
export function scriptInfoGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ScriptInfo
> {
  const res: LbrPrelude.List<LbrPlutusV3.ScriptInfo> = [];

  for (const currencySymbol of currencySymbolGoldens()) {
    res.push({
      fields: currencySymbol,
      name: "Minting",
    });
  }

  for (const txOutRef of txOutRefGoldens()) {
    for (const datum of toMaybe(datumGoldens())) {
      res.push({
        fields: [txOutRef, datum],
        name: "Spending",
      });
    }
  }

  for (const credential of credentialGoldens()) {
    res.push({
      fields: credential,
      name: "Rewarding",
    });
  }

  for (const txCert of txCertGoldensV3()) {
    res.push({
      fields: [0n, txCert],
      name: "Certifying",
    });
  }

  for (const voter of voterGoldensV3()) {
    res.push({
      fields: voter,
      name: "Voting",
    });
  }

  for (const proposalProcedure of proposalProcedureGoldensV3()) {
    res.push({
      fields: [0n, proposalProcedure],
      name: "Proposing",
    });
  }

  return res;
}

/**
 * Hard coded {@link  TxInInfo} tests
 */
export function txInInfoGoldensV3(): LbrPrelude.List<LbrPlutusV3.TxInInfo> {
  const res: LbrPrelude.List<LbrPlutusV3.TxInInfo> = [];

  for (const txOutRef of txOutRefGoldensV3()) {
    for (const txOut of txOutGoldensV2()) {
      res.push({ txInInfoOutRef: txOutRef, txInInfoResolved: txOut });
    }
  }

  return res;
}

/**
 * Hard coded {@link TxInfo} tests
 */
export function txInfoGoldensV3(): LbrPrelude.List<LbrPlutusV3.TxInfo> {
  const res: LbrPrelude.List<LbrPlutusV3.TxInfo> = [];

  const wdrls: PlaMap.Map<LbrPlutusV1.Credential, PlaV1.Lovelace> = PlaMap
    .empty();

  for (const credential of credentialGoldens()) {
    PlaMap.insert(PlaV1.eqCredential, credential, 1234n, wdrls);
  }

  const datums: PlaMap.Map<LbrPlutusV1.DatumHash, LbrPlutusV1.Datum> = PlaMap
    .empty();

  for (const datumHash of datumHashGoldens()) {
    for (const datum of datumGoldens()) {
      PlaMap.insert(PlaV1.eqDatumHash, datumHash, datum, datums);
    }
  }

  const redeemers: PlaMap.Map<LbrPlutusV3.ScriptPurpose, LbrPlutusV1.Redeemer> =
    PlaMap.empty();

  for (const scriptPurpose of scriptPurposeGoldensV3()) {
    for (const redeemer of redeemerGoldens()) {
      PlaMap.insert(PlaV3.eqScriptPurpose, scriptPurpose, redeemer, redeemers);
    }
  }

  for (const fee of lovelaceGoldensV3()) {
    for (const mint of valueGoldens()) {
      for (const validRange of posixTimeRangeGoldens()) {
        for (const id of txIdGoldens()) {
          for (const currentTreasuryAmount of toMaybe(lovelaceGoldensV3())) {
            for (const treasuryDonation of toMaybe(lovelaceGoldensV3())) {
              res.push({
                txInfoInputs: txInInfoGoldensV2(),
                txInfoReferenceInputs: txInInfoGoldensV2(),
                txInfoOutputs: txOutGoldensV2(),
                txInfoFee: fee,
                txInfoMint: mint,
                txInfoTxCerts: txCertGoldensV3(),
                txInfoWdrl: wdrls,
                txInfoValidRange: validRange,
                txInfoSignatories: pubKeyHashGoldens(),
                txInfoRedeemers: redeemers,
                txInfoData: datums,
                txInfoId: id,
                txInfoVotes: toMap(voterGoldensV3().slice(0, 3), [
                  toMap(governanceActionIdGoldensV3(), voteGoldensV3()),
                ]),
                txInfoProposalProcedures: proposalProcedureGoldensV3().slice(
                  0,
                  3,
                ),
                txInfoCurrentTreasuryAmount: currentTreasuryAmount,
                txInfoTreasuryDonation: treasuryDonation,
              });
            }
          }
        }
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link ScriptContext} tests
 */
export function scriptContextGoldensV3(): LbrPrelude.List<
  LbrPlutusV3.ScriptContext
> {
  const res: LbrPrelude.List<LbrPlutusV3.ScriptContext> = [];

  for (const scriptInfo of scriptInfoGoldensV3()) {
    for (const redeemer of redeemerGoldens()) {
      for (const txInfo of txInfoGoldensV3()) {
        res.push({
          scriptContextScriptInfo: scriptInfo,
          scriptContextRedeemer: redeemer,
          scriptContextTxInfo: txInfo,
        });
        if (res.length >= MAX_TEST_CASES) {
          return res;
        }
      }
    }
  }

  return res;
}

/**
 * Hard coded {@link List} tests
 */
export function listGoldens(): LbrPrelude.List<LbrPrelude.Bool>[] {
  return [[], [true], [false], [true, true, false, false]];
}

/*
 * Foo.Bar goldens
 */

/**
 * Hardcoded {@link FooSum} tests
 */
export function fooSumGoldens<A, B, C>(
  x: A,
  y: B,
  z: C,
): LbrPrelude.List<LbfFooBar.FooSum<A, B, C>> {
  return [
    { name: "Foo", fields: [x, y, z] },
    { name: "Bar", fields: [x, y] },
    { name: "Baz", fields: y },
    { name: "Qax" },
    { name: "Faz", fields: 0n },
  ];
}

/**
 * Hardcoded {@link FooProd} tests
 */
export function fooProdGoldens<A, B, C>(
  x: A,
  y: B,
  z: C,
): LbfFooBar.FooProd<A, B, C>[] {
  return [[x, y, z, 1337n]];
}

/**
 * Hard coded  {@link FooRec} tests
 */
export function fooRecGoldens<A, B, C>(
  x: A,
  y: B,
  z: C,
): LbfFooBar.FooRec<A, B, C>[] {
  return [{ fooA: x, fooB: y, fooC: z, fooInt: 1337n }];
}

/*
 * Foo goldens
 */

/**
 * Hard coded  {@link A} tests
 */
export function aGoldens(): LbrPrelude.List<LbfFoo.A> {
  const res: LbrPrelude.List<LbfFoo.A> = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooSum of fooSumGoldens(address, value, datum)) {
          res.push(fooSum);
        }
      }
    }
  }
  return res;
}

/**
 * Hard coded  {@link B} tests
 */
export function bGoldens(): LbrPrelude.List<LbfFoo.B> {
  const res: LbrPrelude.List<LbfFoo.B> = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooProd of fooProdGoldens(address, value, datum)) {
          res.push(fooProd);
        }
      }
    }
  }
  return res;
}

/**
 * Hard coded  {@link C} tests
 */
export function cGoldens(): LbrPrelude.List<LbfFoo.C> {
  const res: LbrPrelude.List<LbfFoo.C> = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooRec of fooRecGoldens(address, value, datum)) {
          res.push(fooRec);
        }
      }
    }
  }
  return res;
}

/**
 * Hard coded  {@link D} tests
 */
export function dGoldens(): LbrPrelude.List<LbfFoo.D> {
  let fooSums: LbrPrelude.List<
    LbfFooBar.FooSum<LbrPlutusV1.Address, LbrPlutusV1.Value, LbrPlutusV1.Datum>
  > = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooSum of fooSumGoldens(address, value, datum)) {
          fooSums.push(fooSum);
        }
      }
    }
  }

  fooSums = fooSums.slice(0, 2);

  let fooProds: LbrPrelude.List<
    LbfFooBar.FooProd<LbrPlutusV1.Address, LbrPlutusV1.Value, LbrPlutusV1.Datum>
  > = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooProd of fooProdGoldens(address, value, datum)) {
          fooProds.push(fooProd);
        }
      }
    }
  }

  fooProds = fooProds.slice(0, 2);

  let fooRecs: LbrPrelude.List<
    LbfFooBar.FooRec<LbrPlutusV1.Address, LbrPlutusV1.Value, LbrPlutusV1.Datum>
  > = [];

  for (const address of addressGoldens()) {
    for (const value of valueGoldens()) {
      for (const datum of datumGoldens()) {
        for (const fooRec of fooRecGoldens(address, value, datum)) {
          fooRecs.push(fooRec);
        }
      }
    }
  }

  fooRecs = fooRecs.slice(0, 2);

  const fooComplicateds: LbrPrelude.List<LbfFoo.D> = [];
  for (const fooSum of fooSums) {
    for (const fooProd of fooProds) {
      for (const fooRec of fooRecs) {
        fooComplicateds.push({ sum: fooSum, prod: fooProd, rec: fooRec });
      }
    }
  }

  return fooComplicateds;
}

/**
 * Hard coded {@link FInt} tests
 */
export function fIntGoldens(): LbfFoo.FInt[] {
  return [
    { name: "Nil" },
    { name: "Rec", fields: { name: "Rec", fields: { name: "Nil" } } },
  ];
}

/**
 * Hard coded {@link GInt} tests
 */
export function gIntGoldens(): LbfFoo.GInt[] {
  return [
    { name: "Nil" },
    { name: "Rec", fields: { name: "Rec", fields: { name: "Nil" } } },
  ];
}

/*
 * Days goldens
 */

/**
 * Hard coded {@link Day} tests
 */
export function dayGoldens(): LbfDays.Day[] {
  return [
    { name: "Monday" },
    { name: "Tuesday" },
    { name: "Wednesday" },
    { name: "Thursday" },
    { name: "Friday" },
    { name: "Saturday" },
    { name: "Sunday" },
  ];
}

/**
 * Hard coded {@link WorkDay} tests
 */
export function workDayGoldens(): LbfDays.WorkDay[] {
  return [
    { name: "Monday" },
    { name: "Tuesday" },
    { name: "Wednesday" },
    {
      name: "Thursday",
    },
    { name: "Friday" },
  ];
}

/**
 * Hard coded {@link FreeDay} tests
 */
export function freeDayGoldens(): LbfDays.FreeDay[] {
  return [{ day: { name: "Saturday" } }, { day: { name: "Sunday" } }];
}

/*
 * Prelude goldens
 */

/**
 * Hard coded {@link Bool} tests
 */
export function boolGoldens(): LbrPrelude.Bool[] {
  return [false, true];
}

/**
 * Hard coded {@link Maybe} tests
 */
export function maybeGoldens(): LbrPrelude.Maybe<LbrPrelude.Bool>[] {
  return [
    { name: "Nothing" },
    { name: "Just", fields: true },
    {
      name: "Just",
      fields: false,
    },
  ];
}

/**
 * Hard coded {@link Either} tests
 */
export function eitherGoldens(): LbrPrelude.Either<
  LbrPrelude.Bool,
  LbrPrelude.Bool
>[] {
  return [
    { name: "Left", fields: true },
    { name: "Left", fields: false },
    {
      name: "Right",
      fields: true,
    },
  ];
}

/**
 * Map test cases to Maybe type and add a Nothing variant
 */
export function toMaybe<T>(goldens: LbrPrelude.List<T>): LbrPrelude.Maybe<T>[] {
  return [
    { name: "Nothing" },
    ...goldens.map((golden): LbrPrelude.Maybe<T> => ({
      name: "Just",
      fields: golden,
    })),
  ];
}

export function toMap<K, V>(
  goldensK: LbrPrelude.List<K>,
  goldensV: LbrPrelude.List<V>,
): PlaMap.Map<K, V> {
  const res: [K, V][] = [];

  for (const k of goldensK) {
    for (const v of goldensV) {
      res.push([k, v]);
    }
  }

  return PlaMap.fromList(res);
}
