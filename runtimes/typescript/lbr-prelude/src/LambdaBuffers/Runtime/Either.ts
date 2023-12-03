import * as LbJson from "./Json.js";
import type { Eq } from "./Eq.js";
import type { Ord } from "./Ord.js";
import type { Json } from "./Json.js";
import { JsonError } from "./Json.js";

/**
 * {@link Either} provides either `L` or `R` exclusively
 */
export type Either<L, R> =
  | { name: "Left"; fields: L }
  | { name: "Right"; fields: R };

/**
 * {@link Eq} instance for {@link Either}
 */
export function eqEither<L, R>(dict1: Eq<L>, dict2: Eq<R>): Eq<Either<L, R>> {
  return {
    eq: (l: Either<L, R>, r: Either<L, R>) => {
      if (l.name === "Left" && r.name === "Left") {
        return dict1.eq(l.fields, r.fields);
      } else if (l.name === "Right" && r.name === "Right") {
        return dict2.eq(l.fields, r.fields);
      } else {
        return false;
      }
    },
    neq: (l: Either<L, R>, r: Either<L, R>) => {
      if (l.name === "Left" && r.name === "Left") {
        return dict1.neq(l.fields, r.fields);
      } else if (l.name === "Right" && r.name === "Right") {
        return dict2.neq(l.fields, r.fields);
      } else {
        return true;
      }
    },
  };
}

/**
 * {@link Ord} instance for {@link Either}
 */
export function ordEither<L, R>(
  dict1: Ord<L>,
  dict2: Ord<R>,
): Ord<Either<L, R>> {
  return {
    eq: eqEither(dict1, dict2).eq,
    neq: eqEither(dict1, dict2).neq,
    compare: (l: Either<L, R>, r: Either<L, R>) => {
      if (l.name === "Left" && r.name === "Left") {
        return dict1.compare(l.fields, r.fields);
      } else if (l.name === "Right" && r.name === "Right") {
        return dict2.compare(l.fields, r.fields);
      } else if (l.name === "Left" && r.name === "Right") {
        return "LT";
      } //if (l.name === "Right" && r.name === "Left")
      else {
        return "GT";
      }
    },
  };
}

/**
 * {@link Json} instance for {@link Either}
 */
export function jsonEither<L, R>(
  dict1: Json<L>,
  dict2: Json<R>,
): Json<Either<L, R>> {
  return {
    toJson: (either) => {
      if (either.name === "Left") {
        return LbJson.jsonConstructor("Left", [
          dict1.toJson(either.fields),
        ]);
      } else {
        return LbJson.jsonConstructor("Right", [
          dict2.toJson(either.fields),
        ]);
      }
    },
    fromJson: (value) => {
      return LbJson.caseJsonConstructor<Either<L, R>>("Prelude.Either", {
        "Left": (ctorFields) => {
          if (ctorFields.length === 1) {
            return {
              name: "Left",
              fields: dict1.fromJson(ctorFields[0]!),
            };
          } else {
            throw new JsonError(
              "Expected JSON Array with 1 fields but got" +
                LbJson.stringify(value),
            );
          }
        },
        "Right": (ctorFields) => {
          if (ctorFields.length === 1) {
            return {
              name: "Right",
              fields: dict2.fromJson(ctorFields[0]!),
            };
          } else {
            throw new JsonError(
              "Expected JSON Array with 1 fields but got" +
                LbJson.stringify(value),
            );
          }
        },
      }, value);
    },
  };
}
