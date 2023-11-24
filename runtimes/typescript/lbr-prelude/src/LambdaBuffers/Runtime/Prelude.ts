import type { Json } from "./Json.js";
import { JsonError, Scientific } from "./Json.js";
import * as LbJson from "./Json.js";
import type { Eq } from "./Eq.js";
import type { Ord } from "./Ord.js";
import * as LbMap from "./Map.js";
import * as LbSet from "./Set.js";

/**
 * {@link Eq} instance for primitive types. This simply wraps `===`.
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
 * {@link Ord} instance for primitive types. This simply wraps `<`, `>`
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
 * {@link Bool} wraps the primitive {@link boolean}
 */
export type Bool = boolean;

/**
 * {@link Eq} instance for {@link Bool}
 */
export const eqBool: Eq<Bool> = eqPrimitive;

/**
 * {@link Ord} instance for {@link Bool}
 */
export const ordBool: Ord<Bool> = ordPrimitive;

/**
 * {@link Json} instance for {@link Bool}
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
 * {@link Integer} wraps the primitive {@link bigint}
 */
export type Integer = bigint;

/**
 * {@link Eq} instance for {@link Integer}
 */
export const eqInteger: Eq<Integer> = eqPrimitive;

/**
 * {@link Ord} instance for {@link Integer}
 */
export const ordInteger: Ord<Integer> = ordPrimitive;

/**
 * {@link Json} instance for {@link Integer}
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
 * {@link Bytes} wraps the object {@link Uint8Array}
 */
export type Bytes = Uint8Array;

/**
 * {@link bytesFromOctets} converts a string of octets into a {@link Bytes}.
 * Values outside the range of an octet are clamped.
 */
export function bytesFromOctets(str: string): Bytes {
  return Uint8Array.from(str, (v: string, _ix: number) => v.codePointAt(0)!);
}

/**
 * {@link Eq} instance for {@link Bytes} which is true iff the arrays are the
 * same length and each element is the same pairwise.
 */
export const eqBytes: Eq<Bytes> = {
  eq: (l, r) => {
    if (!(l.length === r.length)) {
      return false;
    }
    for (let i = 0; i < l.length; ++i) {
      if (l[i]! !== r[i]!) {
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
      if (l[i]! !== r[i]!) {
        return true;
      }
    }
    return false;
  },
};

/**
 * {@link Ord} instance for {@link Bytes} which orders the bytes lexicographically
 */
export const ordBytes: Ord<Bytes> = {
  eq: eqBytes.eq,
  neq: eqBytes.neq,
  compare: (l, r) => {
    let i = 0;
    let j = 0;
    for (; i < l.length && j < r.length; ++i, ++j) {
      if (l[i]! !== r[i]!) {
        return l[i]! < r[i]! ? "LT" : "GT";
      }
    }
    if (l.length === r.length) {
      return "EQ";
    }
    if (l.length < r.length) {
      return "LT";
    }

    // if (i > j)
    return "GT";
  },
};

/**
 * {@link Json} instance for {@link Bytes} encodes / decodes the bytes as a base64 string.
 */
export const jsonBytes: Json<Bytes> = {
  toJson: (bytes) => {
    const binString = String.fromCodePoint(...bytes);
    return btoa(binString);
  },
  fromJson: (value) => {
    if (typeof value === "string") {
      const binString = atob(value);
      return bytesFromOctets(binString);
    } else {
      throw new JsonError("JSON Value is not a string");
    }
  },
};

/**
 * {@link Char} wraps the primitive {@link string} which represents a single unicode
 * codepoint.
 *
 * See {@link charFromString} and {@link charToString} for type conversions.
 */
export type Char = string & { __compileTimeOnlyChar: Char };

/**
 * {@link charFromString} converts a string to a character
 *
 * @param str - A string which contains a single Unicode codepoint
 *
 * @returns The corresponding Unicode codepoint
 *
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
 * {@link charToString} converts a character (a Unicode codepoint) to a `string`.
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
 * {@link Eq} instance for {@link Char}
 */
export const eqChar: Eq<Char> = eqPrimitive;

/**
 * {@link Ord} instance for {@link Char}
 */
export const ordChar: Ord<Char> = ordPrimitive;

/**
 * {@link Json} instance for {@link Char}
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
 * {@link Text} wraps the primitive {@link string}
 */
export type Text = string;

/**
 * {@link Eq} instance for {@link Text}
 */
export const eqText: Eq<Text> = eqPrimitive;

/**
 * {@link Ord} instance for {@link Text}
 */
export const ordText: Ord<Text> = ordPrimitive;

/**
 * {@link Json} instance for {@link Text}
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
 * {@link Map}
 */
export type Map<K, V> = LbMap.Map<K, V>;

/**
 * {@link Eq} instance for a {@link Map}. Returns true iff
 * - Both maps have the same number of elements
 * - Every key value pair in one map is in the other.
 */
export function eqMap<K, V>(dictK: Eq<K>, dictV: Eq<V>): Eq<Map<K, V>> {
  return {
    eq: (l, r) => {
      return eqList(eqPair(dictK, dictV)).eq(
        LbMap.toList(l),
        LbMap.toList(r),
      );
    },
    neq: (l, r) => {
      return eqList(eqPair(dictK, dictV)).neq(
        LbMap.toList(l),
        LbMap.toList(r),
      );
    },
  };
}

/**
 * {@link Ord} instance for a {@link Map} where the sorted lists of key value
 * pairs are compared lexicographically.
 */
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

/**
 * {@link Json} instance for {@link Map}
 */
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
 * {@link Eq} instance for a set. Returns true iff
 * - Both sets have the same number of elements
 * - Every key in one set is in the other.
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

/**
 * {@link Ord} instance for a {@link Set} where the sorted lists of keys are
 * compared lexicographically.
 */
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
 * {@link Json} instance for a {@link Set}
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
 * {@link List} wraps {@link Array}
 */
export type List<A> = Array<A>;

/**
 * {@link Eq} instance for {@link List}
 */
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

/**
 * {@link Ord} instance for {@link List}
 */
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
      if (l.length === r.length) {
        return "EQ";
      }
      if (l.length < r.length) {
        return "LT";
      }

      // if (i > j)
      return "GT";
    },
  };
}

/**
 * {@link Json} instance for {@link List}
 */
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
 * {@link Pair} is an array of fixed size 2
 */
export type Pair<L, R> = [L, R];

/**
 * {@link Eq} instance for {@link Pair}
 */
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

/**
 * {@link Ord} instance for {@link Pair}
 */
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

/**
 * {@link Json} instance for {@link Pair}
 */
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
          "Expected JSON Array of length 2 but got " +
            LbJson.stringify(value),
        );
      }
      return [dict1.fromJson(value[0]!), dict2.fromJson(value[1]!)];
    },
  };
}
