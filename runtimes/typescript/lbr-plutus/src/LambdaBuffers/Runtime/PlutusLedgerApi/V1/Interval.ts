import { IsPlutusDataError } from "../../PlutusData.js";
import type { IsPlutusData } from "../../PlutusData.js";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { Bool, Eq, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs

/**
 * {@link Interval} of `A`s
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs#L49-L67}
 */
export type Interval<A> = { ivFrom: LowerBound<A>; ivTo: UpperBound<A> };

/**
 * {@link Eq} instance for {@link Interval}
 *
 * @remarks
 * This differs from the Haskell implementation in that this simply tests if the two intervals are
 * "structurally" equal (it does not do normalization for open / closed enumerable intervals).
 * This is identical to the behavior from CTL {@link https://github.com/Plutonomicon/cardano-transaction-lib/blob/3943ad5435c9e848368db9d6ff7cb9139a587afe/src/Internal/Types/Interval.purs#L200C25-L200C25}
 */
export function eqInterval<A>(dictA: Eq<A>): Eq<Interval<A>> {
  return {
    eq: (l, r) => {
      return eqLowerBound(dictA).eq(l.ivFrom, r.ivFrom) &&
        eqUpperBound(dictA).eq(l.ivTo, r.ivTo);
    },
    neq: (l, r) => {
      return eqLowerBound(dictA).neq(l.ivFrom, r.ivFrom) ||
        eqUpperBound(dictA).neq(l.ivTo, r.ivTo);
    },
  };
}

/**
 * {@link Json} instance for {@link Interval}
 */
export function jsonInterval<A>(dictA: Json<A>): Json<Interval<A>> {
  return {
    toJson: (interval) => {
      return {
        "from": jsonLowerBound(dictA).toJson(interval.ivFrom),
        "to": jsonUpperBound(dictA).toJson(interval.ivTo),
      };
    },
    fromJson: (value) => {
      const from = LbPrelude.caseFieldWithValue(
        "from",
        jsonLowerBound(dictA).fromJson,
        value,
      );
      const to = LbPrelude.caseFieldWithValue(
        "to",
        jsonUpperBound(dictA).fromJson,
        value,
      );
      return { "ivFrom": from, "ivTo": to };
    },
  };
}

/**
 * {@link IsPlutusData} instance for {@link Interval}
 */
export function isPlutusDataInterval<A>(
  dictA: IsPlutusData<A>,
): IsPlutusData<Interval<A>> {
  return {
    toData: (interval) => {
      return {
        name: "Constr",
        fields: [0n, [
          isPlutusDataLowerBound(dictA).toData(interval.ivFrom),
          isPlutusDataUpperBound(dictA).toData(interval.ivTo),
        ]],
      };
    },
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr": {
          if (
            plutusData.fields[0] === 0n && plutusData.fields[1].length === 2
          ) {
            const ivFrom = isPlutusDataLowerBound(dictA).fromData(
              plutusData.fields[1][0]!,
            );
            const ivTo = isPlutusDataUpperBound(dictA).fromData(
              plutusData.fields[1][1]!,
            );
            return { "ivFrom": ivFrom, "ivTo": ivTo };
          } else {
            break;
          }
        }
        default:
          break;
      }
      throw new IsPlutusDataError("Unexpected data");
    },
  };
}

/**
 * {@link Extended} is a set extended with positive and negative infinity.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs#L75-L78 }
 */
export type Extended<A> =
  | { name: "NegInf" }
  | { name: "PosInf" }
  | { name: "Finite"; fields: A };

/**
 * {@link Eq} instance for {@link Extended}
 */
export function eqExtended<A>(dictA: Eq<A>): Eq<Extended<A>> {
  return {
    eq: (l, r) => {
      if (l.name === "NegInf" && r.name === "NegInf") {
        return true;
      } else if (l.name === "PosInf" && r.name === "PosInf") {
        return true;
      } else if (l.name === "Finite" && r.name === "Finite") {
        return dictA.eq(l.fields, r.fields);
      } else {
        return false;
      }
    },
    neq: (l, r) => {
      if (l.name === "NegInf" && r.name === "NegInf") {
        return false;
      } else if (l.name === "PosInf" && r.name === "PosInf") {
        return false;
      } else if (l.name === "Finite" && r.name === "Finite") {
        return dictA.neq(l.fields, r.fields);
      } else {
        return true;
      }
    },
  };
}

/**
 * {@link Json} instance for {@link Extended}
 */
export function jsonExtended<A>(dictA: Json<A>): Json<Extended<A>> {
  return {
    toJson: (extended) => {
      switch (extended.name) {
        case "NegInf":
          return LbPrelude.jsonConstructor(extended.name, []);
        case "PosInf":
          return LbPrelude.jsonConstructor(extended.name, []);
        case "Finite":
          return LbPrelude.jsonConstructor(extended.name, [
            dictA.toJson(extended.fields),
          ]);
      }
    },
    fromJson: (value) => {
      return LbPrelude.caseJsonConstructor<Extended<A>>("Plutus.V1.Extended", {
        "NegInf": (ctorfields) => {
          if (ctorfields.length !== 0) {
            throw new JsonError("Expected JSON array with 0 elements");
          }

          return { name: "NegInf" };
        },
        "PosInf": (ctorfields) => {
          if (ctorfields.length !== 0) {
            throw new JsonError("Expected JSON array with 0 elements");
          }

          return { name: "PosInf" };
        },
        "Finite": (ctorfields) => {
          if (ctorfields.length !== 1) {
            throw new JsonError("Expected JSON array with 1 elements");
          }
          return { name: "Finite", fields: dictA.fromJson(ctorfields[0]!) };
        },
      }, value);
    },
  };
}

/**
 * {@link IsPlutusData} instance for {@link Extended}
 */
export function isPlutusDataExtended<A>(
  dictA: IsPlutusData<A>,
): IsPlutusData<Extended<A>> {
  return {
    toData: (extended) => {
      switch (extended.name) {
        case "NegInf":
          return { name: "Constr", fields: [0n, []] };
        case "Finite":
          return {
            name: "Constr",
            fields: [1n, [dictA.toData(extended.fields)]],
          };
        case "PosInf":
          return { name: "Constr", fields: [2n, []] };
      }
    },
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr": {
          if (
            plutusData.fields[0] === 0n && plutusData.fields[1].length === 0
          ) {
            return { name: "NegInf" };
          } else if (
            plutusData.fields[0] === 1n && plutusData.fields[1].length === 1
          ) {
            return {
              name: "Finite",
              fields: dictA.fromData(plutusData.fields[1][0]!),
            };
          } else if (
            plutusData.fields[0] === 2n && plutusData.fields[1].length === 0
          ) {
            return { name: "PosInf" };
          } else {
            break;
          }
        }
        default:
          break;
      }
      throw new IsPlutusDataError("Unexpected data");
    },
  };
}

/**
 * {@link LowerBound} is the lower bound of an interval
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs#L123-L126}
 */
export type LowerBound<A> = [Extended<A>, Closure];

/**
 * {@link Eq} instance for {@link LowerBound}
 */
export function eqLowerBound<A>(dictA: Eq<A>): Eq<LowerBound<A>> {
  return LbPrelude.eqPair(eqExtended(dictA), LbPrelude.eqBool);
}

/**
 * {@link Json} instance for {@link LowerBound}
 */
export function jsonLowerBound<A>(dictA: Json<A>): Json<LowerBound<A>> {
  return {
    toJson: (lowerBound) => {
      return {
        bound: jsonExtended(dictA).toJson(lowerBound[0]),
        closed: LbPrelude.jsonBool.toJson(lowerBound[1]),
      };
    },
    fromJson: (value) => {
      const bound = LbPrelude.caseFieldWithValue(
        "bound",
        jsonExtended(dictA).fromJson,
        value,
      );
      const closed = LbPrelude.caseFieldWithValue(
        "closed",
        LbPrelude.jsonBool.fromJson,
        value,
      );
      return [bound, closed];
    },
  };
}

/**
 * {@link IsPlutusData} instance for {@link LowerBound}
 */
export function isPlutusDataLowerBound<A>(
  dictA: IsPlutusData<A>,
): IsPlutusData<LowerBound<A>> {
  return LbPreludeInstances.isPlutusDataPairWithTag(
    isPlutusDataExtended(dictA),
    LbPreludeInstances.isPlutusDataBool,
  );
}

/**
 * {@link UpperBound} is an upper bound of an interval
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs#L94-L97}
 */
export type UpperBound<A> = [Extended<A>, Closure];

/**
 * {@link Eq} instance for {@link UpperBound}
 *
 * @privateremarks
 * This is identical to {@link eqLowerBound}
 */
export function eqUpperBound<A>(dictA: Eq<A>): Eq<UpperBound<A>> {
  return eqLowerBound(dictA);
}

/**
 * {@link Json} instance for {@link UpperBound}
 *
 * @privateremarks
 * This is identical to {@link jsonLowerBound}
 */
export function jsonUpperBound<A>(dictA: Json<A>): Json<UpperBound<A>> {
  return jsonLowerBound(dictA);
}

/**
 * {@link IsPlutusData} instance for {@link UpperBound}
 *
 * @privateremarks
 * This is identical to {@link isPlutusDataLowerBound}
 */
export function isPlutusDataUpperBound<A>(
  dictA: IsPlutusData<A>,
): IsPlutusData<UpperBound<A>> {
  return isPlutusDataLowerBound(dictA);
}

/**
 * {@link Closure} is an alias for {@link Bool} and indicates whether a bound is inclusive or not.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs#L90-L92}
 */
export type Closure = Bool;
