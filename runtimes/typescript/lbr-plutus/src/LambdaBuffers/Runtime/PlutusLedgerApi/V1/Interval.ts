import { FromDataError } from "../../PlutusData.js";
import type { FromData, ToData } from "../../PlutusData.js";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import type { Bool, Eq, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import { JsonError } from "lbr-prelude";

/**
 * {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Interval.hs}
 *
 * https://github.com/Plutonomicon/cardano-transaction-lib/blob/3943ad5435c9e848368db9d6ff7cb9139a587afe/src/Internal/Types/Interval.purs#L200C25-L200C25
 */

// -- PlutusLedgerApi.V1.Interval
// opaque Interval a
//
// instance PlutusData (Interval a) :- PlutusData a
// instance Eq (Interval a) :- Eq a
// instance Json (Interval a) :- Json a
export type Interval<A> = { ivFrom: LowerBound<A>; ivTo: UpperBound<A> };

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

export function toDataInterval<A>(dictA: ToData<A>): ToData<Interval<A>> {
  return {
    toData: (interval) => {
      return {
        name: "Constr",
        fields: [0n, [
          toDataLowerBound(dictA).toData(interval.ivFrom),
          toDataUpperBound(dictA).toData(interval.ivTo),
        ]],
      };
    },
  };
}
export function fromDataInterval<A>(dictA: FromData<A>): FromData<Interval<A>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr": {
          if (
            plutusData.fields[0] === 0n && plutusData.fields[1].length === 2
          ) {
            const ivFrom = fromDataLowerBound(dictA).fromData(
              plutusData.fields[1][0]!,
            );
            const ivTo = fromDataUpperBound(dictA).fromData(
              plutusData.fields[1][0]!,
            );
            return { "ivFrom": ivFrom, "ivTo": ivTo };
          } else {
            break;
          }
        }
        default:
          break;
      }
      throw new FromDataError("Unexpected data");
    },
  };
}

// opaque Extended a
//
// instance PlutusData (Extended a) :- PlutusData a
// instance Eq (Extended a) :- Eq a
// instance Json (Extended a) :- Json a
export type Extended<A> =
  | { name: "NegInf" }
  | { name: "PosInf" }
  | { name: "Finite"; fields: A };

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

export function toDataExtended<A>(dictA: ToData<A>): ToData<Extended<A>> {
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
  };
}

export function fromDataExtended<A>(dictA: FromData<A>): FromData<Extended<A>> {
  return {
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
      throw new FromDataError("Unexpected data");
    },
  };
}

// opaque LowerBound a
//
// instance PlutusData (LowerBound a) :- PlutusData a
// instance Eq (LowerBound a) :- Eq a
// instance Json (LowerBound a) :- Json a

export type LowerBound<A> = [Extended<A>, Closure];

export function eqLowerBound<A>(dictA: Eq<A>): Eq<LowerBound<A>> {
  return LbPrelude.eqPair(eqExtended(dictA), LbPrelude.eqBool);
}
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

export function toDataLowerBound<A>(dictA: ToData<A>): ToData<LowerBound<A>> {
  return LbPreludeInstances.toDataPairWithTag(
    toDataExtended(dictA),
    LbPreludeInstances.toDataBool,
  );
}

export function fromDataLowerBound<A>(
  dictA: FromData<A>,
): FromData<LowerBound<A>> {
  return LbPreludeInstances.fromDataPairWithTag(
    fromDataExtended(dictA),
    LbPreludeInstances.fromDataBool,
  );
}

// opaque UpperBound a
//
// instance PlutusData (UpperBound a) :- PlutusData a
// instance Eq (UpperBound a) :- Eq a
// instance Json (UpperBound a) :- Json a
export type UpperBound<A> = [Extended<A>, Closure];

export function eqUpperBound<A>(dictA: Eq<A>): Eq<UpperBound<A>> {
  return eqLowerBound(dictA);
}
export function jsonUpperBound<A>(dictA: Json<A>): Json<UpperBound<A>> {
  return jsonLowerBound(dictA);
}

export function toDataUpperBound<A>(dictA: ToData<A>): ToData<UpperBound<A>> {
  return toDataLowerBound(dictA);
}

export function fromDataUpperBound<A>(
  dictA: FromData<A>,
): FromData<UpperBound<A>> {
  return fromDataLowerBound(dictA);
}

export type Closure = Bool;
