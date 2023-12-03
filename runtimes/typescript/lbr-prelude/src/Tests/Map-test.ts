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

  if (!(lkup.name === "Just" && eqDict.eq(lkup.fields, v))) {
    throw new Error(
      `Insertion for [${k}, ${v}] failed for map ${JSON.stringify(map.tree)}`,
    );
  }
}

export function removeAndCheck<K, V>(ordDict: Ord<K>, k: K, map: Map<K, V>) {
  LbMap.remove(ordDict, k, map);
  LbMap.checkInvariants(ordDict, map);

  if (LbMap.lookup(ordDict, k, map).name === "Just") {
    throw new Error(
      `Removal failed for ${k} for map ${JSON.stringify(map.tree)}`,
    );
  }
}

export function checkMapLength<K, V>(map: Map<K, V>, expectedLength: number) {
  if (map.length !== expectedLength) {
    throw new Error(
      `Expected map of length ${expectedLength} but got ${map.length} for map ${
        JSON.stringify(map.tree)
      }`,
    );
  }
}

export function randomIntUpToButNotIncluding(max: number) {
  return Math.floor(Math.random() * max);
}

describe("Map tests", () => {
  describe("Ascending insertion invariant checks", () => {
    const map: Map<number, number> = new Map();

    checkMapLength(map, 0);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 1);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 3);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    checkMapLength(map, 4);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    checkMapLength(map, 5);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    checkMapLength(map, 6);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    checkMapLength(map, 7);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    checkMapLength(map, 8);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    checkMapLength(map, 9);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    checkMapLength(map, 10);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    checkMapLength(map, 11);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Nothing") {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
      // lookups shouldn't change the length
      checkMapLength(map, 11);
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Just") {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Descending insertion invariant checks", () => {
    const map: Map<number, number> = new Map();

    checkMapLength(map, 0);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    checkMapLength(map, 1);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    checkMapLength(map, 3);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    checkMapLength(map, 4);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    checkMapLength(map, 5);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    checkMapLength(map, 6);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    checkMapLength(map, 7);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    checkMapLength(map, 8);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 9);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    checkMapLength(map, 10);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 11);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Nothing") {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
      checkMapLength(map, 11);
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Just") {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Insert replaces elements", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 1, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 2, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 4, map);
  });

  describe("Assorted insertion invariant checks 1.", () => {
    const map: Map<number, number> = new Map();

    checkMapLength(map, 0);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    checkMapLength(map, 1);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    checkMapLength(map, 3);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    checkMapLength(map, 4);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    checkMapLength(map, 5);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    checkMapLength(map, 6);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    checkMapLength(map, 7);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    checkMapLength(map, 8);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 9);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    checkMapLength(map, 10);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 11);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Nothing") {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
      checkMapLength(map, 11);
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Just") {
        throw new Error(`${i} is in the map: ${JSON.stringify(map.tree)}`);
      }
    }
  });

  describe("Assorted insertion invariant checks 2.", () => {
    const map: Map<number, number> = new Map();
    checkMapLength(map, 0);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 1);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 4, 4, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 3);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    checkMapLength(map, 4);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 6, 6, map);
    checkMapLength(map, 5);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    checkMapLength(map, 6);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 5, 5, map);
    checkMapLength(map, 7);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 10, 10, map);
    checkMapLength(map, 8);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 8, 8, map);
    checkMapLength(map, 9);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 7, 7, map);
    checkMapLength(map, 10);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 9, 9, map);
    checkMapLength(map, 11);

    for (let i = 0; i <= 10; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Nothing") {
        throw new Error(
          `${i} not found in the map: ${JSON.stringify(map.tree)}`,
        );
      }
      checkMapLength(map, 11);
    }

    for (let i = 11; i <= 20; ++i) {
      if (LbMap.lookup(LbPrelude.ordPrimitive, i, map).name === "Just") {
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
      if (
        LbPrelude.eqMaybe(LbPrelude.eqPrimitive).neq(
          LbMap.lookup(LbPrelude.ordPrimitive, key, map),
          { name: "Just", fields: value },
        )
      ) {
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
      if (
        LbPrelude.eqMaybe(LbPrelude.eqPrimitive).neq(
          LbMap.lookup(LbPrelude.ordPrimitive, key, map),
          { name: "Just", fields: value },
        )
      ) {
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
    checkMapLength(map, 4);

    removeAndCheck(LbPrelude.ordPrimitive, 3, map);
    checkMapLength(map, 3);

    removeAndCheck(LbPrelude.ordPrimitive, 0, map);
    checkMapLength(map, 2);

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 3);

    removeAndCheck(LbPrelude.ordPrimitive, 2, map);
    checkMapLength(map, 2);
  });

  describe("Small ascending insertion / deletion invariant checks 2.", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 3);
    removeAndCheck(LbPrelude.ordPrimitive, 3, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 1, 1, map);
    checkMapLength(map, 3);
    removeAndCheck(LbPrelude.ordPrimitive, 0, map);
    checkMapLength(map, 2);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    checkMapLength(map, 3);
    removeAndCheck(LbPrelude.ordPrimitive, 2, map);
    checkMapLength(map, 2);
  });

  describe("Small ascending insertion / deletion invariant checks 3.", () => {
    const map: Map<number, number> = new Map();

    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 3, 3, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 0, 0, map);
    insertAndCheck(LbPrelude.ordPrimitive, LbPrelude.eqPrimitive, 2, 2, map);
    checkMapLength(map, 3);
    removeAndCheck(LbPrelude.ordPrimitive, 69, map);
    checkMapLength(map, 3);
  });

  describe("Large random insertion/deletion invariant checks 1.", () => {
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS_DELETIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS_DELETIONS; ++i) {
      if (Math.random() > 0.5) {
        const keys = Object.keys(insertions);
        if (keys.length === 0) {
          // just delete something random since nothing exists in the map

          const k = randomIntUpToButNotIncluding(NUM_INSERTIONS_DELETIONS);
          removeAndCheck(LbPrelude.ordPrimitive, k, map);
        } else {
          // delete something already existing in the map
          const randomkey: number = insertions[
            parseInt(keys[Math.floor(keys.length * Math.random())]!)
          ]!;
          delete insertions[randomkey];

          removeAndCheck(LbPrelude.ordPrimitive, randomkey, map);
        }
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
      checkMapLength(map, Object.keys(insertions).length);
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (
        LbPrelude.eqMaybe(LbPrelude.eqPrimitive).neq(
          LbMap.lookup(LbPrelude.ordPrimitive, key, map),
          { name: "Just", fields: value },
        )
      ) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }

    checkMapLength(map, Object.keys(insertions).length);
  });

  describe("Large random insertion/deletion invariant checks 2.", () => {
    // Same test as above, but we insert a smaller set of keys
    const map: Map<number, number> = new Map();
    const insertions: { [index: number]: number } = {};
    const NUM_INSERTIONS_DELETIONS = 5000;

    for (let i = 0; i < NUM_INSERTIONS_DELETIONS; ++i) {
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
      checkMapLength(map, Object.keys(insertions).length);
    }

    for (const [key, value] of Object.entries(insertions)) {
      if (
        LbPrelude.eqMaybe(LbPrelude.eqPrimitive).neq(
          LbMap.lookup(LbPrelude.ordPrimitive, key, map),
          { name: "Just", fields: value },
        )
      ) {
        throw new Error(
          `[${key}, ${value}] is either not in the map or the wrong value in the map: ${
            JSON.stringify(map.tree)
          }`,
        );
      }
    }

    checkMapLength(map, Object.keys(insertions).length);
  });

  describe("toList ascending string tests", () => {
    const map: Map<string, string> = new Map();

    insertAndCheck(
      LbPrelude.ordPrimitive,
      LbPrelude.eqPrimitive,
      "b",
      "c",
      map,
    );
    insertAndCheck(
      LbPrelude.ordPrimitive,
      LbPrelude.eqPrimitive,
      "a",
      "b",
      map,
    );
    insertAndCheck(
      LbPrelude.ordPrimitive,
      LbPrelude.eqPrimitive,
      "d",
      "f",
      map,
    );

    const old = "";
    for (const [k, _v] of LbMap.toList(map)) {
      if (!(old < k)) {
        throw new Error("toList not strictly increasing");
      }
    }
  });
});
