import * as LbJson from "./Json.js";
import type { Eq } from "./Eq.js";
import type { Ord } from "./Ord.js";
import type { Json } from "./Json.js";
import { JsonError } from "./Json.js";

/**
 * {@link Maybe} is either `"Just"` the value `A`, or `"Nothing"`.
 */
export type Maybe<A> =
  | { name: "Just"; fields: A }
  | { name: "Nothing" };

/**
 * {@link fromJust} returns `Just`'s `.fields` if it exists -- otherwise
 * throwing an error
 *
 * @throws {@link Error}
 * Thrown if the provided value has `.name` as `"Nothing"`
 */
export function fromJust<A>(maybe: Maybe<A>): A {
  if (maybe.name === "Just") {
    return maybe.fields;
  }
  throw new Error(`Expected Just but got Nothing`);
}

/**
 * {@link Eq} instance for {@link Maybe}
 */
export function eqMaybe<A>(dict: Eq<A>): Eq<Maybe<A>> {
  return {
    eq: (l, r) => {
      if (l.name === "Nothing" && r.name === "Nothing") {
        return true;
      } else if (l.name === "Just" && r.name === "Just") {
        return dict.eq(l.fields, r.fields);
      } else {
        return false;
      }
    },
    neq: (l, r) => {
      if (l.name === "Nothing" && r.name === "Nothing") {
        return false;
      } else if (l.name === "Just" && r.name === "Just") {
        return dict.neq(l.fields, r.fields);
      } else {
        return true;
      }
    },
  };
}

/**
 * {@link Ord} instance for {@link Maybe}
 */
export function ordMaybe<A>(dict: Ord<A>): Ord<Maybe<A>> {
  return {
    eq: eqMaybe(dict).eq,
    neq: eqMaybe(dict).neq,
    compare: (l, r) => {
      if (l.name === "Just" && r.name === "Just") {
        return dict.compare(l.fields, r.fields);
      }
      if (l.name === "Nothing" && r.name === "Just") {
        return "LT";
      }
      if (l.name === "Just" && r.name === "Nothing") {
        return "GT";
      }
      // if (l === undefined && r === undefined)
      return "EQ";
    },
  };
}

/**
 * {@link Json} instance for {@link Maybe}
 */
export function jsonMaybe<A>(dict: Json<A>): Json<Maybe<A>> {
  return {
    toJson: (maybe) => {
      if (maybe.name === "Nothing") {
        return LbJson.jsonConstructor("Nothing", []);
      } else {
        return LbJson.jsonConstructor("Just", [dict.toJson(maybe.fields)]);
      }
    },
    fromJson: (value) => {
      return LbJson.caseJsonConstructor<Maybe<A>>("Prelude.Maybe", {
        "Nothing": (ctorFields) => {
          if (ctorFields.length === 0) {
            return { name: "Nothing" };
          } else {
            throw new JsonError(
              "Expected JSON Array with 0 fields but got" +
                LbJson.stringify(value),
            );
          }
        },
        "Just": (ctorFields) => {
          if (ctorFields.length === 1) {
            return { name: "Just", fields: dict.fromJson(ctorFields[0]!) };
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
