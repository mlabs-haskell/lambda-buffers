import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "./Prelude/Instances.js";
import type { Bool, Eq, Json, List, Maybe } from "lbr-prelude";
import { FromDataError } from "./PlutusData.js";
import type { FromData, PlutusData, ToData } from "./PlutusData.js";

/**
 * {@link Map | `Map<K,V>`} is an mapping from keys `K` to values `V` where `K` only needs an
 * {@link Eq} instance.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-tx/src/PlutusTx/AssocMap.hs }
 */
export type Map<K, V> = List<[K, V]> & { __compileTimeOnlyMap: Map<K, V> };

/**
 * {@link Eq} instance for {@link Map}. Note that we follow Haskell's implementation which
 * unintuitively is "structurally equal" so
 * ```
 * let mapA : Map<Integer,Integer> = []
 * insert(eqInteger, 1, 2, mapA)
 * insert(eqInteger, 2, 1, mapA)
 * ```
 * and
 * ```
 * let mapB : Map<Integer,Integer> = []
 * insert(eqInteger, 2, 1, mapB)
 * insert(eqInteger, 1, 2, mapB)
 * ```
 * are considered _not_ equal.
 */
export function eqMap<K, V>(dictK: Eq<K>, dictV: Eq<V>): Eq<Map<K, V>> {
  return LbPrelude.eqList(LbPrelude.eqPair(dictK, dictV));
}

/**
 * {@link Json} instance for {@link Map}
 *
 * @remarks
 * This copies the Haskell definition which uses {@link fromList} that does
 * _not_ verify uniqueness of the keys.
 */
export function jsonMap<K, V>(dictK: Json<K>, dictV: Json<V>): Json<Map<K, V>> {
  return {
    toJson: (map) => {
      return LbPrelude.jsonList(LbPrelude.jsonPair(dictK, dictV)).toJson(map);
    },
    fromJson: (value) => {
      return fromList(
        LbPrelude.jsonList(LbPrelude.jsonPair(dictK, dictV)).fromJson(value),
      );
    },
  };
}

/**
 * {@link ToData} instance for {@link Map}
 */
export function toDataMap<K, V>(
  dictK: ToData<K>,
  dictV: ToData<V>,
): ToData<Map<K, V>> {
  return {
    toData: (arg) => {
      return {
        name: "Map",
        fields: arg.map((kv) => {
          const kvData = LbPreludeInstances.toDataPairWithoutTag(dictK, dictV)
            .toData(kv);
          if (kvData.name === "List") {
            return kvData.fields as [PlutusData, PlutusData];
          } else {
            throw new Error(
              "Internal error: toDataPairWithoutTag didn't return a list",
            );
          }
        }),
      };
    },
  };
}

/**
 * {@link FromData} instance for {@link Map}
 *
 * @remarks
 * This copies the Haskell definition which uses {@link fromList} that does
 * _not_ verify uniqueness of the keys.
 */
export function fromDataMap<K, V>(
  dictK: FromData<K>,
  dictV: FromData<V>,
): FromData<Map<K, V>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Map":
          return fromList(plutusData.fields.map((kvData) => {
            const kv = LbPreludeInstances.fromDataPairWithoutTag(dictK, dictV)
              .fromData({ name: "List", fields: kvData });
            return kv;
          }));
        default:
          throw new FromDataError("Expected Map but got " + plutusData);
      }
    },
  };
}

/**
 * {@link fromList} translates a list of key value pairs to a {@link Map}.
 *
 * @remarks
 * This copies the Haskell function which does _not_ test whether the set of
 * keys are unique.
 */
export function fromList<K, V>(elems: Readonly<List<[K, V]>>): Map<K, V> {
  return elems as Map<K, V>;
}

/**
 * {@link toList} translates a {@link Map} to a list of key value pairs.
 *
 * @remarks
 * This copies the Haskell function which does _not_ test whether the list of
 * keys are unique.
 */
export function toList<K, V>(map: Readonly<Map<K, V>>): List<[K, V]> {
  return map;
}

/**
 * {@link lookup} looks up the provided key in the map (with the provided
 * {@link Eq} instance), and returns the corresponding value if it exists.
 *
 * Complexity: `O(n)`
 */
export function lookup<K, V>(
  eq: Eq<K>,
  key: K,
  map: Readonly<Map<K, V>>,
): Maybe<V> {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], key)) {
      return map[i]![1];
    }
  }
  return undefined;
}

/**
 * {@link member} calls {@link lookup} and returns true iff {@link lookup} does
 * not return `undefined`.
 *
 * Complexity: `O(n)`.
 */
export function member<K, V>(
  eq: Eq<K>,
  key: K,
  map: Readonly<Map<K, V>>,
): Bool {
  if (lookup(eq, key, map) === undefined) {
    return false;
  } else {
    return true;
  }
}

/**
 * {@link remove} removes the key value pair corresponding to the given key if
 * it exists. If not such key value pair exists, this does nothing.
 *
 * Complexity: `O(n)`.
 */
export function remove<K, V>(eq: Eq<K>, key: K, map: Map<K, V>): void {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], key)) {
      map.splice(i, 1);
      return;
    }
  }
  return;
}

/**
 * {@link insert} inserts the given key value pair in the map. If the key
 * already exists (as determined by the given {@link Eq} instance), then the
 * already existing key value pair is replaced.
 *
 * Complexity: `O(n)`.
 */
export function insert<K, V>(
  eq: Eq<K>,
  key: K,
  value: V,
  map: Map<K, V>,
): void {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], key)) {
      map[i] = [key, value];
      return;
    }
  }

  map.push([key, value]);
  return;
}
