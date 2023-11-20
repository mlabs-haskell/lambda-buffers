import type { Json } from "./Json.js";
import { JsonError, Scientific } from "./Json.js";
import * as LbJson from "./Json.js";
import type { Eq } from "./Eq.js";
import type { Ord } from "./Ord.js";
import { Buffer } from "node:buffer";
import * as LbMap from "./Map.js";
import * as LbSet from "./Set.js";

/**
 * `Eq` instance for primitive types. This simply wraps `===`.
 */
// deno-lint-ignore no-explicit-any
export const eqPrimitive: Eq<any> = {
  eq: (l, r) => {
    return l === r;
  },
  neq: (l, r) => {
    return l !== r;
  },
};

/**
 * `Ord` instance for primitive types. This simply wraps `<`, `>`
 */
// deno-lint-ignore no-explicit-any
export const ordPrimitive: Ord<any> = {
  compare: (l, r) => {
    if (l < r) {
      return "LT";
    } else if (l > r) {
      return "GT";
    } else {
      return "EQ";
    }
  },
  eq: eqPrimitive.eq,
  neq: eqPrimitive.neq,
};

/**
 * `Bool` wraps the primitive `boolean`
 */
export type Bool = boolean;

/**
 * `Eq` instance for `Bool`
 */
export const eqBool: Eq<Bool> = eqPrimitive;

/**
 * `Ord` instance for `Bool`
 */
export const ordBool: Ord<Bool> = ordPrimitive;

/**
 * `Json` instance for `Bool`
 */
export const jsonBool: Json<Bool> = {
  toJson: (arg) => {
    return arg;
  },
  fromJson: (value) => {
    if (typeof value === "boolean") {
      return value;
    } else {
      throw new JsonError("Expected bool");
    }
  },
};

/**
 * `Integer` wraps the primitive `bigint`
 */
export type Integer = bigint;

/**
 * `Eq` instance for `Integer`
 */
export const eqInteger: Eq<Integer> = eqPrimitive;

/**
 * `Ord` instance for `Integer`
 */
export const ordInteger: Ord<Integer> = ordPrimitive;

/**
 * `Json` instance for `Integer`
 */
export const jsonInteger: Json<Integer> = {
  toJson: (arg) => {
    return new Scientific(arg, 0n);
  },
  fromJson: (value) => {
    if (value && value instanceof Scientific) {
      if (value.base10Exponent < 0) {
        throw new JsonError("Expected Integer");
      }

      // TODO(jaredponn): verify that we're using the same limits as
      // scientific in Haskell
      if (value.base10Exponent > 372) {
        throw new JsonError("Integer too large");
      }

      return value.coefficient * 10n ** value.base10Exponent;
    } else {
      throw new JsonError("Expected Integer");
    }
  },
};

/**
 * `Bytes` wraps the object `Uint8Array`
 */
export type Bytes = Uint8Array;

/**
 * `bytesFrom` is an alias for {@link Buffer.from }.
 */
export const bytesFrom = Buffer.from;

/**
 * `Eq` instance for `Bytes` which is true iff the arrays are the same length
 * and each element is the same pairwise.
 */
export const eqBytes: Eq<Bytes> = {
  eq: (l, r) => {
    return Buffer.compare(l, r) === 0;
  },
  neq: (l, r) => {
    return Buffer.compare(l, r) !== 0;
  },
};

/**
 * `Ord` instance for `Bytes` which orders the bytes lexicographically
 */
export const ordBytes: Ord<Bytes> = {
  eq: eqBytes.eq,
  neq: eqBytes.neq,
  compare: (l, r) => {
    const res = Buffer.compare(l, r);
    if (res === -1) {
      return "LT";
    } else if (res === 1) {
      return "GT";
    } else {
      return "EQ";
    }
  },
};

/**
 * `Json` instance for `Bytes` encodes / decodes the bytes as a base64 string.
 */
export const jsonBytes: Json<Bytes> = {
  toJson: (bytes) => {
    // The copy is necessary otherwise the buffer gets mutated under the hood
    // and the tests break i.e., the following is broken
    // ```
    // const bufferBytes = Buffer.from(bytes.buffer);
    // ```
    const bufferBytes = Buffer.from(bytes);
    return bufferBytes.toString("base64");
  },
  fromJson: (value) => {
    if (typeof value === "string") {
      return Buffer.from(value, "base64");
    } else {
      throw new JsonError("JSON Value is not a string");
    }
  },
};

/**
 * `Char` wraps the primitive `string` which represents a single unicode
 * codepoint.
 *
 * See {@link charFromString} and {@link charToString} for type conversions.
 */
export type Char = string & { __compileTimeOnlyChar: Char };

/**
 * `charFromString` converts a string to a character
 *
 * @param str - A string which contains a single Unicode codepoint
 *
 * @returns The corresponding Unicode codepoint
 j
 * @throws {@link Error}
 * Thrown when the input string is not a single Unicode codepoint
 */
export function charFromString(str: string): Char {
  if (str.match(/^[^]$/u)) {
    return str as Char;
  } else {
    throw new Error(
      "Invalid character " + str + " should be exactly one codepoint",
    );
  }
}

/**
 * `charToString` converts a character (a Unicode codepoint) to a `string`.
 *
 * @param chr - A character that is Unicode codepoint
 *
 * @returns The corresponding string
 */
export function charToString(chr: Readonly<Char>): string {
  // Strange trick to satisfy the `Readonly`
  return `${chr}`;
}

/**
 * `Eq` instance for `Char`
 */
export const eqChar: Eq<Char> = eqPrimitive;

/**
 * `Ord` instance for `Char`
 */
export const ordChar: Ord<Char> = ordPrimitive;

/**
 * `Json` instance for `Char`
 */
export const jsonChar: Json<Char> = {
  toJson: (arg) => {
    return charToString(arg);
  },
  fromJson: (value) => {
    if (typeof value === "string") {
      return charFromString(value);
    } else {
      throw new JsonError("Expected string");
    }
  },
};

/**
 * `Text` wraps the primitive `string`
 */
export type Text = string;

/**
 * `Eq` instance for `Text`
 */
export const eqText: Eq<Text> = eqPrimitive;

/**
 * `Ord` instance for `Text`
 */
export const ordText: Ord<Text> = ordPrimitive;

/**
 * `Json` instance for `Text`
 */
export const jsonText: Json<Text> = {
  toJson: (arg) => {
    return arg;
  },
  fromJson: (value) => {
    if (typeof value === "string") {
      return value;
    } else {
      throw new JsonError("Expected string");
    }
  },
};

/**
 * `Maybe<A>` is either the value `A` or undefined.
 *
 * @remarks
 * Note this type is compatible with Typescript's _optional parameters_ `?`.
 *
 * @privateRemarks
 * TODO(jaredponn): an incompatibility with Haskell is that this definition
 * does _not_ allow `Maybe<undefined>` to have two distinct values as
 * `Just undefined ` and `Nothing` are both regarded as `undefined`.
 * In practise, we hope this will not be an issue.
 */
export type Maybe<A> = A | undefined;

/**
 * `Eq` instance for `Maybe<A>`
 */
export function eqMaybe<A>(dict: Eq<A>): Eq<Maybe<A>> {
  return {
    eq: (l, r) => {
      if (l === undefined && r === undefined) {
        return true;
      } else if (l !== undefined && r !== undefined) {
        return dict.eq(l, r);
      } else {
        return false;
      }
    },
    neq: (l, r) => {
      if (l === undefined && r === undefined) {
        return false;
      } else if (l !== undefined && r !== undefined) {
        return dict.neq(l, r);
      } else {
        return true;
      }
    },
  };
}

/**
 * `Ord` instance for `Maybe<A>`
 */
export function ordMaybe<A>(dict: Ord<A>): Ord<Maybe<A>> {
  return {
    eq: eqMaybe(dict).eq,
    neq: eqMaybe(dict).neq,
    compare: (l, r) => {
      if (l !== undefined && r !== undefined) {
        return dict.compare(l, r);
      }
      if (l === undefined && r !== undefined) {
        return "LT";
      }
      if (l !== undefined && r === undefined) {
        return "GT";
      }
      // if (l === undefined && r === undefined)
      return "EQ";
    },
  };
}

/**
 * `Json` instance for `Maybe<A>`
 */
export function jsonMaybe<A>(dict: Json<A>): Json<Maybe<A>> {
  return {
    toJson: (maybe) => {
      if (maybe === undefined) {
        return LbJson.jsonConstructor("Nothing", []);
      } else {
        return LbJson.jsonConstructor("Just", [dict.toJson(maybe)]);
      }
    },
    fromJson: (value) => {
      return LbJson.caseJsonConstructor<Maybe<A>>("Prelude.Maybe", {
        "Nothing": (ctorFields) => {
          if (ctorFields.length === 0) {
            return undefined;
          } else {
            throw new JsonError(
              "Expected JSON Array with 0 fields but got" +
                LbJson.stringify(value),
            );
          }
        },
        "Just": (ctorFields) => {
          if (ctorFields.length === 1) {
            return dict.fromJson(ctorFields[0]!);
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

/**
 * `Either<L,R>` provides either `L` or `R` exclusively
 */
export type Either<L, R> =
  | { name: "Left"; fields: L }
  | { name: "Right"; fields: R };

export function eqEither<L, R>(dict1: Eq<L>, dict2: Eq<R>): Eq<Either<L, R>> {
  return {
    eq: (l, r) => {
      if (l.name === "Left" && r.name === "Left") {
        return dict1.eq(l.fields, r.fields);
      } else if (l.name === "Right" && r.name === "Right") {
        return dict2.eq(l.fields, r.fields);
      } else {
        return false;
      }
    },
    neq: (l, r) => {
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

export function ordEither<L, R>(
  dict1: Ord<L>,
  dict2: Ord<R>,
): Ord<Either<L, R>> {
  return {
    eq: eqEither(dict1, dict2).eq,
    neq: eqEither(dict1, dict2).neq,
    compare: (l, r) => {
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

export function jsonEither<L, R>(
  dict1: Json<L>,
  dict2: Json<R>,
): Json<Either<L, R>> {
  return {
    toJson: (either) => {
      if (either.name === "Left") {
        return LbJson.jsonConstructor("Left", [dict1.toJson(either.fields)]);
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

export type Map<K, V> = LbMap.Map<K, V>;

/**
 * `Eq` instance for a map. Returns true iff
 * - Both maps have the same number of elements
 * - Every key value pair in one map is in the other.
 */
export function eqMap<K, V>(dictK: Eq<K>, dictV: Eq<V>): Eq<Map<K, V>> {
  return {
    eq: (l, r) => {
      return eqList(eqPair(dictK, dictV)).eq(LbMap.toList(l), LbMap.toList(r));
    },
    neq: (l, r) => {
      return eqList(eqPair(dictK, dictV)).neq(LbMap.toList(l), LbMap.toList(r));
    },
  };
}

export function ordMap<K, V>(dictK: Ord<K>, dictV: Ord<V>): Ord<Map<K, V>> {
  return {
    eq: eqMap(dictK, dictV).eq,
    neq: eqMap(dictK, dictV).neq,
    compare: (l, r) => {
      return ordList(ordPair(dictK, dictV)).compare(
        LbMap.toList(l),
        LbMap.toList(r),
      );
    },
  };
}

export function jsonMap<K, V>(
  ordDict: Ord<K>,
  dictK: Json<K>,
  dictV: Json<V>,
): Json<Map<K, V>> {
  return {
    toJson: (map) => {
      const kvs = LbMap.toList(map);
      return jsonList(jsonPair(dictK, dictV)).toJson(kvs);
    },
    fromJson: (value) => {
      return LbJson.caseJsonMap<K, V>("Map", ordDict, (arg) => {
        return [dictK.fromJson(arg[0]), dictV.fromJson(arg[1])];
      }, value);
    },
  };
}

export type Set<K> = LbSet.Set<K>;

/**
 * `Eq` instance for a set. Returns true iff
 * - Both sets have the same number of elements
 * - Every key pair in one set is in the other.
 *
 * @remarks
 * Warning; this uses the `Set`'s equality of keys ("shallow" equality)
 */
export function eqSet<K>(dictK: Eq<K>): Eq<Set<K>> {
  return {
    eq: (l, r) => {
      return eqList(dictK).eq(LbSet.toList(l), LbSet.toList(r));
    },
    neq: (l, r) => {
      return eqList(dictK).neq(LbSet.toList(l), LbSet.toList(r));
    },
  };
}

export function ordSet<K>(dictK: Ord<K>): Ord<Set<K>> {
  return {
    eq: eqSet(dictK).eq,
    neq: eqSet(dictK).neq,
    compare: (l, r) => {
      return ordList(dictK).compare(LbSet.toList(l), LbSet.toList(r));
    },
  };
}

/**
 * `Json` instance for a set.
 *
 * @remarks
 * Warning; this uses the `Set`'s equality of keys ("shallow" equality)
 * and non primitive set elements probably won't do what is expected.
 */
export function jsonSet<K>(ordDict: Ord<K>, dictK: Json<K>): Json<Set<K>> {
  return {
    toJson: (set) => {
      return jsonList(dictK).toJson(LbSet.toList(set));
    },
    fromJson: (value) => {
      const arr = LbJson.caseJsonArray<K>("Set", dictK.fromJson, value);

      const set: LbSet.Set<K> = new LbSet.Set();
      for (const k of arr) {
        LbSet.insert(ordDict, k, set);
      }

      if (LbSet.toList(set).length !== arr.length) {
        throw new JsonError(`Set should have unique keys`);
      }

      return set;
    },
  };
}

/**
 * `List<A>` wraps `A[]`
 */
export type List<A> = A[];

export function eqList<A>(dict: Eq<A>): Eq<List<A>> {
  return {
    eq: (l, r) => {
      if (!(l.length === r.length)) {
        return false;
      }
      for (let i = 0; i < l.length; ++i) {
        if (!dict.eq(l[i]!, r[i]!)) {
          return false;
        }
      }
      return true;
    },
    neq: (l, r) => {
      if (!(l.length === r.length)) {
        return true;
      }
      for (let i = 0; i < l.length; ++i) {
        if (dict.neq(l[i]!, r[i]!)) {
          return true;
        }
      }
      return false;
    },
  };
}

export function ordList<A>(dict: Ord<A>): Ord<List<A>> {
  return {
    eq: eqList(dict).eq,
    neq: eqList(dict).neq,
    compare: (l, r) => {
      let i = 0;
      let j = 0;
      for (; i < l.length && j < r.length; ++i, ++j) {
        const cmp = dict.compare(l[i]!, r[i]!);
        if (cmp !== "EQ") {
          return cmp;
        }
      }
      if (i === j) {
        return "EQ";
      }
      if (i < j) {
        return "LT";
      }

      // if (i > j)
      return "GT";
    },
  };
}

export function jsonList<A>(dict: Json<A>): Json<List<A>> {
  return {
    toJson: (list) => {
      return list.map(dict.toJson);
    },
    fromJson: (value) => {
      return LbJson.caseJsonArray("List", dict.fromJson, value);
    },
  };
}

/**
 * Pair is an array of a fixed size 2.
 */
export type Pair<L, R> = [L, R];

export function eqPair<L, R>(dict1: Eq<L>, dict2: Eq<R>): Eq<Pair<L, R>> {
  return {
    eq: (l, r) => {
      return dict1.eq(l[0], r[0]) && dict2.eq(l[1], r[1]);
    },
    neq: (l, r) => {
      return dict1.neq(l[0], r[0]) || dict2.neq(l[1], r[1]);
    },
  };
}

export function ordPair<L, R>(dict1: Ord<L>, dict2: Ord<R>): Ord<Pair<L, R>> {
  return {
    eq: eqPair(dict1, dict2).eq,
    neq: eqPair(dict1, dict2).neq,
    compare: (l, r) => {
      const res = dict1.compare(l[0], r[0]);
      if (res === "EQ") {
        return dict2.compare(l[1], r[1]);
      } else {
        return res;
      }
    },
  };
}

export function jsonPair<L, R>(
  dict1: Json<L>,
  dict2: Json<R>,
): Json<Pair<L, R>> {
  return {
    toJson: (pair) => {
      return [dict1.toJson(pair[0]), dict2.toJson(pair[1])];
    },
    fromJson: (value) => {
      if (!(LbJson.isJsonArray(value) && value.length === 2)) {
        throw new JsonError(
          "Expected JSON Array of length 2 but got " + LbJson.stringify(value),
        );
      }
      return [dict1.fromJson(value[0]!), dict2.fromJson(value[1]!)];
    },
  };
}
