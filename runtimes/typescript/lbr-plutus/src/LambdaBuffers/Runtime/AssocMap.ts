import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "./Prelude/Instances.js";
import type { Bool, Eq, Json, List, Maybe } from "lbr-prelude";
import { FromDataError } from "./PlutusData.js";
import type { FromData, PlutusData, ToData } from "./PlutusData.js";

/**
 * {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-tx/src/PlutusTx/AssocMap.hs }
 */
export type Map<K, V> = List<[K, V]>;

/**
 * `Eq` instance for `Map`. Note that we follow Haskell's implementation which
 * unintuitively is "structurally equal" so
 * ```
 * let mapA : Map<Integer,Integer> = []
 * insert(eqInteger, 1, 2, mapA)
 * insert(eqInteger, 2, 1, mapA)
 * ```
 * and
 * ```
 * let mapA : Map<Integer,Integer> = []
 * insert(eqInteger, 2, 1, mapA)
 * insert(eqInteger, 1, 2, mapA)
 * ```
 * are _not_ equal.
 */
export function eqMap<K, V>(dictK: Eq<K>, dictV: Eq<V>): Eq<Map<K, V>> {
  return LbPrelude.eqList(LbPrelude.eqPair(dictK, dictV));
}

export function jsonMap<K, V>(dictK: Json<K>, dictV: Json<V>): Json<Map<K, V>> {
  return LbPrelude.jsonList(LbPrelude.jsonPair(dictK, dictV));
}

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

export function fromDataMap<K, V>(
  dictK: FromData<K>,
  dictV: FromData<V>,
): FromData<Map<K, V>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Map":
          return plutusData.fields.map((kvData) => {
            const kv = LbPreludeInstances.fromDataPairWithoutTag(dictK, dictV)
              .fromData({ name: "List", fields: kvData });
            return kv;
          });
        default:
          throw new FromDataError("Expected Map but got " + plutusData);
      }
    },
  };
}

// -- PlutusTx.AssocMap
//
// opaque Map k v
//
// instance PlutusData (Map k v) :- PlutusData k, PlutusData v
// instance Eq (Map k v) :- Eq k, Eq v
// instance Json (Map k v) :- Json k, Json v

// TODO: make things readonly

export function fromList<K, V>(elems: List<[K, V]>) {
  return elems;
}

export function toList<K, V>(map: Map<K, V>) {
  return map;
}

export function lookup<K, V>(eq: Eq<K>, k: K, map: Map<K, V>): Maybe<V> {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], k)) {
      return map[i]![1];
    }
  }
  return undefined;
}

export function member<K, V>(eq: Eq<K>, k: K, map: Map<K, V>): Bool {
  if (lookup(eq, k, map) === undefined) {
    return false;
  } else {
    return true;
  }
}

/**
 * Copies the `delete` function in Haskell
 */
export function remove<K, V>(eq: Eq<K>, k: K, map: Map<K, V>): void {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], k)) {
      map.splice(i, 1);
      return;
    }
  }
  return;
}

export function insert<K, V>(eq: Eq<K>, k: K, v: V, map: Map<K, V>): void {
  for (let i = 0; i < map.length; ++i) {
    if (eq.eq(map[i]![0], k)) {
      map[i] = [map[i]![0], v];
      return;
    }
  }

  map.push([k, v]);
  return;
}
