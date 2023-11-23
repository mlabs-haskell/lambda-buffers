import type { Ord } from "./Ord.js";
import * as LbAvlTree from "./AvlTree.js";
import type { Node } from "./AvlTree.js";

/**
 * A mapping from `K` to `V` where `K` must have a {@link Ord} instance.
 */
export class Map<K, V> {
  tree: Node<[K, V]>;

  constructor() {
    this.tree = null;
  }
}

/**
 * {@link ordOnFst} compares pairs on the first projection.
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
  map.tree = LbAvlTree.insert(ordOnFst(ordDict), [key, value], map.tree);
}

/**
 * {@link remove} removes a key (and its corresponding value) in the map. If the key
 * does not exist, then this does nothing.
 *
 * Complexity: `O(log n)`
 */
export function remove<K, V>(ordDict: Ord<K>, key: K, map: Map<K, V>): void {
  map.tree = LbAvlTree.remove(ordOnFst(ordDict), [key, null as V], map.tree);
}

/**
 * {@link lookup} lookups up the value corresponding to the given key, and returns
 * `undefined` if no such key exists.
 *
 * Complexity: `O(log n)`
 */
export function lookup<K, V>(
  ordDict: Ord<K>,
  key: K,
  map: Readonly<Map<K, V>>,
): V | undefined {
  const lkup: undefined | [K, V] = LbAvlTree.lookup(ordOnFst(ordDict), [
    key,
    null as V,
  ], map.tree);
  if (lkup === undefined) {
    return undefined;
  } else {
    return lkup[1];
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
