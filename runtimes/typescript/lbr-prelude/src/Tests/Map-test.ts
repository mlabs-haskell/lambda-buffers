import { describe } from "node:test";

import * as LbPrelude from "../LambdaBuffers/Prelude.js";
import { Map } from "../LambdaBuffers/Runtime/Map.js";
import * as LbMap from "../LambdaBuffers/Runtime/Map.js";
import type { Eq, Ord } from "../LambdaBuffers/Prelude.js";

/**
 * Inserts the element in the map and checks
 *  1. the invariants are satisfied
 *  2. the element is now in the map
 */
export function insertAndCheck<K, V>(
  ordDict: Ord<K>,
  eqDict: Eq<V>,
  k: K,
  v: V,
  map: Map<K, V>,
) {
  LbMap.insert(ordDict, k, v, map);
  LbMap.checkInvariants(ordDict, map);

  const lkup = LbMap.lookup(ordDict, k, map);

  if (!(lkup !== undefined && eqDict.eq(lkup, v))) {
    throw new Error(
      `Insertion for [${k}, ${v}] failed for map ${JSON.stringify(map.tree)}`,
    );
  }
}

export function removeAndCheck<K, V>(ordDict: Ord<K>, k: K, map: Map<K, V>) {
  LbMap.remove(ordDict, k, map);
  LbMap.checkInvariants(ordDict, map);

  if (LbMap.lookup(ordDict, k, map) !== undefined) {
    throw new Error(
      `Removal failed for ${k} for map ${JSON.stringify(map.tree)}`,
    );
  }
}

function randomIntUpToButNotIncluding(max: number) {
  return Math.floor(Math.random() * max);
}

describe("Map tests", () => {
  describe("Ascending insertion invariant checks", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) === undefined) {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) !== undefined) {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Descending insertion invariant checks", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) === undefined) {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) !== undefined) {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Assorted insertion invariant checks 1.", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) === undefined) {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) !== undefined) {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Assorted insertion invariant checks 2.", () => {
    const map: Map<number, number> = new Map();
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) === undefined) {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map) !== undefined) {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Large random insertion invariant checks 1. ", () => {
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS; ++i) {
      const k = randomIntUpToButNotIncluding(NUM_INSERTIONS);
      const v = randomIntUpToButNotIncluding(NUM_INSERTIONS);
      insertions[k] = v;

      insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, k, v, map);
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, key, map) !== value) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }
  });

  describe("Large random insertion invariant checks 2. ", () => {
    // Same test as above, but we insert a smaller set of keys
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS; ++i) {
      const k = randomIntUpToButNotIncluding(NUM_INSERTIONS / 4);
      const v = randomIntUpToButNotIncluding(NUM_INSERTIONS / 4);
      insertions[k] = v;

      insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, k, v, map);
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, key, map) !== value) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }
  });

  describe("Small ascending insertion / deletion invariant checks 1.", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);

    removeAndCheck(LbPrelude.ordPrimitive, 3, map);
    removeAndCheck(LbPrelude.ordPrimitive, 0, map);

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);

    removeAndCheck(LbPrelude.ordPrimitive, 2, map);
  });

  describe("Small ascending insertion / deletion invariant checks 2.", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    removeAndCheck(LbPrelude.ordPrimitive, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    removeAndCheck(LbPrelude.ordPrimitive, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    removeAndCheck(LbPrelude.ordPrimitive, 2, map);
  });

  describe("Large random insertion/deletion invariant checks 1.", () => {
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS_DELETIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS_DELETIONS; ++i) {
      console.log(`${JSON.stringify(map.tree)}`);
      if (Math.random() > 0.5) {
        const keys = Object.keys(insertions);
        if (keys.length === 0) {
          continue;
        }

        const randomkey: number =
          insertions[parseInt(keys[Math.floor(keys.length * Math.random())]!)]!;
        delete insertions[randomkey];

        removeAndCheck(LbPrelude.ordPrimitive, randomkey, map);
      } else {
        // insert
        const k = randomIntUpToButNotIncluding(NUM_INSERTIONS_DELETIONS);
        const v = randomIntUpToButNotIncluding(NUM_INSERTIONS_DELETIONS);
        insertions[k] = v;

        insertAndCheck(
          LbPrelude.ordPrimitive,
          LbPrelude.eqPrimitive,
          k,
          v,
          map,
        );
      }
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, key, map) !== value) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }
  });

  describe("Large random insertion/deletion invariant checks 2.", () => {
    // Same test as above, but we insert a smaller set of keys
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS_DELETIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS_DELETIONS; ++i) {
      console.log(`${JSON.stringify(map.tree)}`);
      if (Math.random() > 0.5) {
        const keys = Object.keys(insertions);
        if (keys.length === 0) {
          continue;
        }

        const randomkey: number =
          insertions[parseInt(keys[Math.floor(keys.length * Math.random())]!)]!;
        delete insertions[randomkey];

        removeAndCheck(LbPrelude.ordPrimitive, randomkey, map);
      } else {
        // insert
        const k = randomIntUpToButNotIncluding(NUM_INSERTIONS_DELETIONS / 4);
        const v = randomIntUpToButNotIncluding(NUM_INSERTIONS_DELETIONS / 4);
        insertions[k] = v;

        insertAndCheck(
          LbPrelude.ordPrimitive,
          LbPrelude.eqPrimitive,
          k,
          v,
          map,
        );
      }
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, key, map) !== value) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }
  });
});
