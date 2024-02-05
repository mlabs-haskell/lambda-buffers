import * as LbrPrelude from "lbr-prelude";
import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as LbrPlutusV2 from "lbr-plutus/V2.js";

import * as PlaV1 from "plutus-ledger-api/V1.js";
import * as PlaMap from "plutus-ledger-api/AssocMap.js";

import * as LbfFooBar from "lbf-plutus-golden-api/LambdaBuffers/Foo/Bar.mjs";
import * as LbfFoo from "lbf-plutus-golden-api/LambdaBuffers/Foo.mjs";
import * as LbfDays from "lbf-plutus-golden-api/LambdaBuffers/Days.mjs";

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
      fields: [1n, [
        { name: "Integer", fields: 1n },
        { name: "Bytes", fields: someBytes() },
      ]],
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
      fields: PlaMap.fromList([[{ name: "Integer", fields: 1n }, {
        name: "Bytes",
        fields: someBytes(),
      }], [{ name: "Integer", fields: 2n }, {
        name: "Bytes",
        fields: someMoreBytes(),
      }]]),
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
  const tn2 = unsafeFromJust(PlaV1.tokenNameFromBytes(
    ((arr: LbrPrelude.List<number>) => {
      for (let i = 1; i < 16; ++i) {
        arr.push(i);
      }
      return Uint8Array.from(arr);
    })([]),
  ));
  const tn3 = unsafeFromJust(PlaV1.tokenNameFromBytes(
    ((arr: LbrPrelude.List<number>) => {
      for (let i = 1; i < 32; ++i) {
        arr.push(i);
      }
      return Uint8Array.from(arr);
    })([]),
  ));

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
    >(
      [
        [
          PlaV1.adaSymbol,
          PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>(
            [[PlaV1.adaToken, 1337n]],
          ),
        ],
      ],
    ),
    PlaMap.fromList<
      LbrPlutusV1.CurrencySymbol,
      LbrPlutusV1.Map<LbrPlutusV1.TokenName, LbrPrelude.Integer>
    >(
      [
        [
          PlaV1.adaSymbol,
          PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>([[
            PlaV1.adaToken,
            1337n,
          ]]),
        ],

        [
          unsafeFromJust(PlaV1.currencySymbolFromBytes(blake2b_224Hash())),
          PlaMap.fromList<LbrPlutusV1.TokenName, LbrPrelude.Integer>(
            [
              [unsafeFromJust(PlaV1.tokenNameFromBytes(emptyBytes())), 1337n],
              [
                unsafeFromJust(PlaV1.tokenNameFromBytes(
                  ((arr: LbrPrelude.List<number>) => {
                    for (let i = 1; i < 16; ++i) {
                      arr.push(i);
                    }
                    return Uint8Array.from(arr);
                  })([]),
                )),
                16n,
              ],
              [
                unsafeFromJust(PlaV1.tokenNameFromBytes(
                  ((arr: LbrPrelude.List<number>) => {
                    for (let i = 1; i < 32; ++i) {
                      arr.push(i);
                    }
                    return Uint8Array.from(arr);
                  })([]),
                )),
                32n,
              ],
            ],
          ),
        ],
      ],
    ),
  ];
}

/**
 * Hard coded {@link Redeemer} tests
 */
export function redeemerGoldens(): LbrPrelude.List<LbrPlutusV1.Redeemer> {
  return [
    { name: "Integer", fields: 1337n },
  ];
}

/**
 * Hard coded {@link Datum} tests
 */
export function datumGoldens(): LbrPrelude.List<LbrPlutusV1.Datum> {
  return [
    { name: "Integer", fields: 1337n },
  ];
}

/**
 * Hard coded {@link RedeemerHash} tests
 */
export function redeemerHashGoldens(): LbrPrelude.List<
  LbrPlutusV1.RedeemerHash
> {
  return [
    unsafeFromJust(PlaV1.redeemerHashFromBytes(blake2b_256Hash())),
  ];
}

/**
 * Hard coded {@link DatumHash} tests
 */
export function datumHashGoldens(): LbrPrelude.List<LbrPlutusV1.DatumHash> {
  return [
    unsafeFromJust(PlaV1.datumHashFromBytes(blake2b_256Hash())),
  ];
}

/**
 * Hard coded {@link TxId} tests
 */
export function txIdGoldens(): LbrPrelude.List<LbrPlutusV1.TxId> {
  return [
    unsafeFromJust(PlaV1.txIdFromBytes(blake2b_256Hash())),
  ];
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

// /*
//  * Hard coded {@link TxInInfo} tests
//  * TODO(jaredponn): this is borked -- this type doesn't actually exist in the .lbf file
//  */
// export function  txInInfoGoldensV1() : LbrPrelude.List<LbrPlutusV1.TxInInfo> {
//     const  res : LbrPrelude.List<LbrPlutusV1.TxInInfo> = [];
//
//     for (const txOutRef of txOutRefGoldens()) {
//         for (const txOut of txOutGoldensV1()) {
//         }
//     }
//  return res
// }

// /*
//  * Hard coded {@link TxOut} tests
//  * TODO(jaredponn): this is borked -- this type doesn't actually exist in the .lbf file
//  */
//

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
          res.push(
            {
              txOutAddress: address,
              txOutValue: value,
              txOutDatum: outDatums[outDatumIx]!,
              txOutReferenceScript: mScriptHash,
            },
          );
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
  return [
    { fooA: x, fooB: y, fooC: z, fooInt: 1337n },
  ];
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
        fooComplicateds.push(
          { sum: fooSum, prod: fooProd, rec: fooRec },
        );
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
  return [{ name: "Monday" }, { name: "Tuesday" }, { name: "Wednesday" }, {
    name: "Thursday",
  }, { name: "Friday" }];
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
  return [{ name: "Nothing" }, { name: "Just", fields: true }, {
    name: "Just",
    fields: false,
  }];
}

/**
 * Hard coded {@link Either} tests
 */
export function eitherGoldens(): LbrPrelude.Either<
  LbrPrelude.Bool,
  LbrPrelude.Bool
>[] {
  return [{ name: "Left", fields: true }, { name: "Left", fields: false }, {
    name: "Right",
    fields: true,
  }];
}

/**
 * Hard coded {@link List} tests
 */
export function listGoldens(): LbrPrelude.List<LbrPrelude.Bool>[] {
  return [[], [true], [false], [true, true, false, false]];
}
