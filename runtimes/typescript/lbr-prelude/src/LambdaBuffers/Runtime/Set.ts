import type { Ord } from "./Ord.js";
import * as LbAvlTree from "./AvlTree.js";
import type { Node } from "./AvlTree.js";

/**
 * A set of elements `K` where `K` must have a {@link Ord} instance.
 */
export class Set<K> {
  tree: Node<K>;
  length: number;

  constructor() {
    this.tree = null;
    this.length = 0;
  }
}

/**
 * {@link insert} adds a key in the set.
 *
 * Complexity: `O(log n)`
 */
export function insert<K>(
  ordDict: Ord<K>,
  key: K,
  set: Set<K>,
): void {
  set.tree = LbAvlTree.alter(
    ordDict,
    (arg) => {
      if (arg === undefined) {
        ++set.length;
        return key;
      } else {
        return key;
      }
    },
    key,
    set.tree,
  );
}

/**
 * {@link remove} removes a key from the set. If the key does not exist, then this
 * does nothing.
 *
 * Complexity: `O(log n)`
 */
export function remove<K>(ordDict: Ord<K>, key: K, set: Set<K>): void {
  set.tree = LbAvlTree.alter(
    ordDict,
    (arg) => {
      if (arg === undefined) {
        return undefined;
      } else {
        --set.length;
        return undefined;
      }
    },
    key,
    set.tree,
  );
}

/**
 * {@link member} returns true if the given key is in the set, and returns false otherwise
 *
 * Complexity: `O(log n)`
 */
export function member<K>(
  ordDict: Ord<K>,
  key: K,
  set: Readonly<Set<K>>,
): boolean {
  return LbAvlTree.lookup(ordDict, key, set.tree) !== undefined;
}

/**
 * Checks the invariants of the internal AVL tree.
 *
 * @internal
 */
export function checkInvariants<K>(
  ordDict: Ord<K>,
  set: Readonly<Set<K>>,
): void {
  return LbAvlTree.checkInvariants(ordDict, set.tree);
}

/**
 * Returns a list of key value pairs in ascending order
 *
 * Complexity: `O(n)`
 */
export function toList<K>(set: Readonly<Set<K>>): K[] {
  return LbAvlTree.toList(set.tree);
}
