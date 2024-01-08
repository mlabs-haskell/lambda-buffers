import type { Ord } from "./Ord.js";
import * as LbAvlTree from "./AvlTree.js";
import type { Node } from "./AvlTree.js";
import type { Maybe } from "./Maybe.js";

/**
 * A mapping from `K` to `V` where `K` must have a {@link Ord} instance.
 *
 * @example
 * ```ts
 * import * as LbPrelude from "lbr-prelude"
 *
 * let map : Map<string, string> = new Map();
 * insert(LbPrelude.ordString,  "a", "b", map)
 * lookup(LbPrelude.ordString,  "a", map) // returns `"b"`
 * map.length // is 1
 *
 * insert(LbPrelude.ordString. "a", "c", map)
 * lookup(LbPrelude.ordString,  "a", map) // returns `"c"`
 * map.length // is 1
 *
 * remove(LbPrelude.ordString,  "a", map)
 * lookup(LbPrelude.ordString,  "a", map) // returns `undefined`
 * map.length // is 0
 * ```
 */
export class Map<K, V> {
  tree: Node<[K, V]>;
  length: number;

  constructor() {
    this.tree = null;
    this.length = 0;
  }
}

/**
 * {@link ordOnFst} compares pairs on the first projection.
 *
 * @privateRemarks
 * This isn't a total order, but it is useful to implement a `Map` from a `Set`
 */
function ordOnFst<K, V>(ordDict: Ord<K>): Ord<[K, V]> {
  return {
    eq: (l, r) => {
      return ordDict.eq(l[0], r[0]);
    },
    neq: (l, r) => {
      return ordDict.neq(l[0], r[0]);
    },
    compare: (l, r) => {
      return ordDict.compare(l[0], r[0]);
    },
  };
}

/**
 * {@link insert} adds a key value pair in the map. If the key already exists, the
 * value is replaced with the new value.
 *
 * Complexity: `O(log n)`
 */
export function insert<K, V>(
  ordDict: Ord<K>,
  key: K,
  value: V,
  map: Map<K, V>,
): void {
  map.tree = LbAvlTree.alter(
    ordOnFst(ordDict),
    (arg) => {
      if (arg === undefined) {
        ++map.length;
        return [key, value];
      } else {
        return [key, value];
      }
    },
    [key, null as V],
    map.tree,
  );
}

/**
 * {@link remove} removes a key (and its corresponding value) in the map. If the key
 * does not exist, then this does nothing.
 *
 * Complexity: `O(log n)`
 */
export function remove<K, V>(ordDict: Ord<K>, key: K, map: Map<K, V>): void {
  map.tree = LbAvlTree.alter(
    ordOnFst(ordDict),
    (arg) => {
      if (arg === undefined) {
        return undefined;
      } else {
        --map.length;
        return undefined;
      }
    },
    [key, null as V],
    map.tree,
  );
}

/**
 * {@link lookup} lookups up the value corresponding to the given key returning
 * `Just` the corresponding value or `Nothing` if it does not exist.
 *
 * Complexity: `O(log n)`
 */
export function lookup<K, V>(
  ordDict: Ord<K>,
  key: K,
  map: Readonly<Map<K, V>>,
): Maybe<V> {
  const lkup: undefined | [K, V] = LbAvlTree.lookup(ordOnFst(ordDict), [
    key,
    null as V,
  ], map.tree);
  if (lkup === undefined) {
    return { name: "Nothing" };
  } else {
    return { name: "Just", fields: lkup[1] };
  }
}

/**
 * Returns a list of key value pairs in ascending order
 *
 * Complexity: `O(n)`
 */
export function toList<K, V>(map: Readonly<Map<K, V>>): [K, V][] {
  return LbAvlTree.toList(map.tree);
}

/**
 * Checks the invariants of the internal AVL tree.
 *
 * @internal
 */
export function checkInvariants<K, V>(
  ordDict: Ord<K>,
  map: Readonly<Map<K, V>>,
): void {
  return LbAvlTree.checkInvariants(ordOnFst(ordDict), map.tree);
}
