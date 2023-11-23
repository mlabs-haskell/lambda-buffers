import type { Ord } from "./Ord.js";
import * as LbAvlTree from "./AvlTree.js";
import type { Node } from "./AvlTree.js";

/**
 * A set of elements `K` where `K` must have a {@link Ord} instance.
 */
export class Set<K> {
  tree: Node<K>;

  constructor() {
    this.tree = null;
  }
}

/**
 * {@link insert} adds a key in the map.
 *
 * Complexity: `O(log n)`
 */
export function insert<K>(ordDict: Ord<K>, key: K, map: Set<K>): void {
  map.tree = LbAvlTree.insert(ordDict, key, map.tree);
}

/**
 * {@link remove} removes a key from the map. If the key does not exist, then this
 * does nothing.
 *
 * Complexity: `O(log n)`
 */
export function remove<K>(ordDict: Ord<K>, key: K, map: Set<K>): void {
  map.tree = LbAvlTree.remove(ordDict, key, map.tree);
}

/**
 * {@link member} returns true if the given key is in the set, and returns false otherwise
 *
 * Complexity: `O(log n)`
 */
export function member<K>(
  ordDict: Ord<K>,
  key: K,
  map: Readonly<Set<K>>,
): boolean {
  return LbAvlTree.lookup(ordDict, key, map.tree) !== undefined;
}

/**
 * Checks the invariants of the internal AVL tree.
 *
 * @internal
 */
export function checkInvariants<K>(
  ordDict: Ord<K>,
  map: Readonly<Set<K>>,
): void {
  return LbAvlTree.checkInvariants(ordDict, map.tree);
}

/**
 * Returns a list of key value pairs in ascending order
 *
 * Complexity: `O(n)`
 */
export function toList<K>(set: Readonly<Set<K>>): K[] {
  return LbAvlTree.toList(set.tree);
}
