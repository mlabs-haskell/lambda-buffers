import * as LbrPrelude from "lbr-prelude";
import * as PreludeSet from "prelude/Set.js";
import * as PreludeMap from "prelude/Map.js";
import * as Prelude from "prelude";
import * as LbfFoo from "lbf-prelude-golden-api/LambdaBuffers/Foo.mjs";
import * as LbfFooBar from "lbf-prelude-golden-api/LambdaBuffers/Foo/Bar.mjs";
import * as LbfDays from "lbf-prelude-golden-api/LambdaBuffers/Days.mjs";

/**
 * Returns some hardcoded bytes for testing.
 */
export function someBytes(): LbrPrelude.Bytes {
  return Uint8Array.from([115, 111, 109, 101, 32, 98, 121, 116, 101, 115]);
}

/**
 * Returns an array of some hardcoded tests of the {@link Foo} type
 */
export function fooSumGoldens<A, B, C>(
  x: A,
  y: B,
  z: C,
): LbfFooBar.FooSum<A, B, C>[] {
  return [
    { name: "Foo", fields: [x, y, z] },
    { name: "Bar", fields: [x, y] },
    { name: "Baz", fields: y },
    { name: "Qax" },
    { name: "Faz", fields: 0n },
  ];
}

/**
 * Returns a hardcoded unit test of the {@link A} type
 */
export function aGoldens(): LbfFoo.A[] {
  return fooSumGoldens(1337n, false, someBytes());
}

/**
 * Returns a hardcoded unit test of the {@link FooProd} type
 */
export function fooProdGoldens<A, B, C>(
  x: A,
  y: B,
  z: C,
): LbfFooBar.FooProd<A, B, C>[] {
  return [[x, y, z, 1337n]];
}

/**
 * Returns a hardcoded unit test of some {@link B} type
 */
export function bGoldens(): LbfFoo.B[] {
  return fooProdGoldens(1337n, false, someBytes());
}

/**
 * A hardcoded unit test of some {@link FooRec} type
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

/**
 * A hardcoded unit test of some {@link C} type
 */
export function cGoldens(): LbfFoo.C[] {
  return fooRecGoldens(1337n, false, someBytes());
}

/**
 * A hardcoded unit test of some {@link D} type
 */
export function dGoldens(): LbfFoo.D[] {
  const result: LbfFoo.D[] = [];
  for (const fooSum of fooSumGoldens(1337n, false, someBytes())) {
    for (const fooProd of fooProdGoldens(1337n, false, someBytes())) {
      for (const fooRec of fooRecGoldens(1337n, false, someBytes())) {
        result.push(
          { sum: fooSum, prod: fooProd, rec: fooRec },
        );
      }
    }
  }
  return result;
}

/**
 * A hardcoded unit test of a {@link FInt} type
 */
export function fIntGoldens(): LbfFoo.FInt[] {
  return [
    { name: "Nil" },
    { name: "Rec", fields: { name: "Rec", fields: { name: "Nil" } } },
  ];
}

/**
 * A hardcoded unit test of a {@link GInt} type
 */
export function gIntGoldens(): LbfFoo.GInt[] {
  return [
    { name: "Nil" },
    { name: "Rec", fields: { name: "Rec", fields: { name: "Nil" } } },
  ];
}

/**
 * A hardcoded unit test of some {@link Day} type
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
 * A hardcoded unit test of some {@link WorkDay} type
 */
export function workDayGoldens(): LbfDays.WorkDay[] {
  return [{ name: "Monday" }, { name: "Tuesday" }, { name: "Wednesday" }, {
    name: "Thursday",
  }, { name: "Friday" }];
}

/**
 * A hardcoded unit test of some {@link FreeDay} type
 */
export function freeDayGoldens(): LbfDays.FreeDay[] {
  return [{ day: { name: "Saturday" } }, { day: { name: "Sunday" } }];
}

/**
 * A hardcoded unit test of some {@link Bool} type
 */
export function boolGoldens(): LbrPrelude.Bool[] {
  return [true, false];
}

/**
 * A hardcoded unit test of some {@link Integer} type
 */
export function integerGoldens(): LbrPrelude.Integer[] {
  return [
    0n,
    1n,
    -1n,
    2n ** 32n,
    -(2n ** 32n),
    2n ** (64n),
    -(2n ** (64n)),
    2n ** (128n),
    -(2n ** (128n)),
    2n ** (256n),
    -(2n ** (256n)),
  ];
}

/**
 * A hardcoded unit test of some {@link Bytes} types
 */
export function bytesGoldens(): LbrPrelude.Bytes[] {
  return [Uint8Array.from([]), Uint8Array.from([0]), someBytes()];
}

/**
 * A hardcoded unit test of some {@link Char} types
 */
export function charGoldens(): LbrPrelude.Char[] {
  return ["\u{0}", "\u{A}", "\u{1f643}"] as LbrPrelude.Char[];
}

/**
 * A hardcoded unit test of some {@link Text} types
 */
export function textGoldens(): LbrPrelude.Text[] {
  return ["", "\n", "dražen popović"];
}

/**
 * A hardcoded unit test of some {@link Maybe} types
 */
export function maybeGoldens(): LbrPrelude.Maybe<LbrPrelude.Bool>[] {
  return [{ name: "Nothing" }, { name: "Just", fields: true }, {
    name: "Just",
    fields: false,
  }];
}

/**
 * A hardcoded unit test of some {@link Either} types
 */
export function eitherGoldens(): LbrPrelude.Either<
  LbrPrelude.Bool,
  LbrPrelude.Text
>[] {
  return [{ name: "Left", fields: true }, { name: "Left", fields: false }, {
    name: "Right",
    fields: "this is right",
  }];
}

/**
 * A hardcoded unit test of some {@link List} types
 */
export function listGoldens(): LbrPrelude.List<LbrPrelude.Bool>[] {
  return [[], [true], [false], [true, true, false, false]];
}

/**
 * A hardcoded unit test of some {@link Set} types
 */
export function setGoldens(): LbrPrelude.Set<LbrPrelude.Bool>[] {
  const set1: PreludeSet.Set<LbrPrelude.Bool> = new PreludeSet.Set();
  const set2: PreludeSet.Set<LbrPrelude.Bool> = new PreludeSet.Set();
  PreludeSet.insert(Prelude.ordBool, true, set2);

  const set3: PreludeSet.Set<LbrPrelude.Bool> = new PreludeSet.Set();
  PreludeSet.insert(Prelude.ordBool, true, set3);
  PreludeSet.insert(Prelude.ordBool, false, set3);

  return [set1, set2, set3];
}

/**
 * A hardcoded unit test of some {@link Map} types
 */
export function mapGoldens(): LbrPrelude.Map<
  LbrPrelude.Bool,
  LbrPrelude.Bool
>[] {
  const map1: PreludeMap.Map<LbrPrelude.Bool, LbrPrelude.Bool> = new PreludeMap
    .Map();
  const map2: PreludeMap.Map<LbrPrelude.Bool, LbrPrelude.Bool> = new PreludeMap
    .Map();
  PreludeMap.insert(Prelude.ordBool, true, true, map2);

  const map3: PreludeMap.Map<LbrPrelude.Bool, LbrPrelude.Bool> = new PreludeMap
    .Map();
  PreludeMap.insert(Prelude.ordBool, true, true, map3);
  PreludeMap.insert(Prelude.ordBool, false, false, map3);

  return [map1, map2, map3];
}
