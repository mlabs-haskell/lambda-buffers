import * as LbAssocMap from "../LambdaBuffers/Runtime/AssocMap.js";
import * as LbPrelude from "lbr-prelude";
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";

import { describe, it } from "node:test";
import * as TestUtils from "./TestUtils.js";
import * as assert from "node:assert/strict";

import fc from "fast-check";

const map1: LbAssocMap.Map<bigint, bigint> = LbAssocMap.empty();
const map2: LbAssocMap.Map<bigint, bigint> = LbAssocMap.fromListSafe(
  LbPrelude.eqInteger,
  [[1n, 2n], [69n, 420n]],
);
const map3: LbAssocMap.Map<bigint, bigint> = LbAssocMap.fromListSafe(
  LbPrelude.eqInteger,
  [[1n, 2n], [69n, 420n], [-69n, -420n]],
);

export function fcAssocMap<K, V>(
  dict: LbPrelude.Eq<K>,
  arbK: fc.Arbitrary<K>,
  arbV: fc.Arbitrary<V>,
): fc.Arbitrary<LbAssocMap.Map<K, V>> {
  return fc.array(fc.tuple(arbK, arbV)).map((t) => {
    return LbAssocMap.fromListSafe(dict, t);
  });
}

describe("AssocMap tests", () => {
  describe("Eq AssocMap tests", () => {
    const dict = LbAssocMap.eqMap(LbPrelude.eqInteger, LbPrelude.eqInteger);

    TestUtils.eqIt(dict, map1, map1, true);
    TestUtils.neqIt(dict, map1, map1, false);

    TestUtils.eqIt(dict, map2, map2, true);
    TestUtils.neqIt(dict, map2, map2, false);

    TestUtils.eqIt(dict, map3, map3, true);
    TestUtils.neqIt(dict, map3, map3, false);

    TestUtils.eqIt(dict, map1, map3, false);
    TestUtils.neqIt(dict, map1, map3, true);

    TestUtils.eqIt(dict, map2, map3, false);
    TestUtils.neqIt(dict, map2, map3, true);

    TestUtils.eqIt(dict, map3, map2, false);
    TestUtils.neqIt(dict, map3, map2, true);

    TestUtils.eqIt(dict, map3, map1, false);
    TestUtils.neqIt(dict, map3, map1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcAssocMap(LbPrelude.eqInteger, fc.bigInt(), fc.bigInt()),
          fcAssocMap(LbPrelude.eqInteger, fc.bigInt(), fc.bigInt()),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json AssocMap tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbAssocMap.jsonMap(LbPrelude.jsonInteger, LbPrelude.jsonInteger),
      map1,
      [],
    );
    TestUtils.toJsonAndFromJsonIt(
      LbAssocMap.jsonMap(LbPrelude.jsonInteger, LbPrelude.jsonInteger),
      map2,
      [[new LbPrelude.Scientific(69n), new LbPrelude.Scientific(420n)], [
        new LbPrelude.Scientific(1n),
        new LbPrelude.Scientific(2n),
      ]],
    );
    TestUtils.toJsonAndFromJsonIt(
      LbAssocMap.jsonMap(LbPrelude.jsonInteger, LbPrelude.jsonInteger),
      map3,
      [[new LbPrelude.Scientific(-69n), new LbPrelude.Scientific(-420n)], [
        new LbPrelude.Scientific(69n),
        new LbPrelude.Scientific(420n),
      ], [new LbPrelude.Scientific(1n), new LbPrelude.Scientific(2n)]],
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcAssocMap(LbPrelude.eqInteger, fc.bigInt(), fc.bigInt()),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbAssocMap.jsonMap(LbPrelude.jsonInteger, LbPrelude.jsonInteger),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("IsPlutusData AssocMap tests", () => {
    TestUtils.isPlutusDataIt(
      LbAssocMap.isPlutusDataMap(
        LbV1.isPlutusDataInteger,
        LbV1.isPlutusDataInteger,
      ),
      map1,
      { name: "Map", fields: [] },
    );

    TestUtils.isPlutusDataIt(
      LbAssocMap.isPlutusDataMap(
        LbV1.isPlutusDataInteger,
        LbV1.isPlutusDataInteger,
      ),
      map2,
      {
        name: "Map",
        fields: [
          [
            LbV1.isPlutusDataInteger.toData(69n),
            LbV1.isPlutusDataInteger.toData(420n),
          ],
          [
            LbV1.isPlutusDataInteger.toData(1n),
            LbV1.isPlutusDataInteger.toData(2n),
          ],
        ],
      },
    );

    TestUtils.isPlutusDataIt(
      LbAssocMap.isPlutusDataMap(
        LbV1.isPlutusDataInteger,
        LbV1.isPlutusDataInteger,
      ),
      map3,
      {
        name: "Map",
        fields: [
          [
            LbV1.isPlutusDataInteger.toData(-69n),
            LbV1.isPlutusDataInteger.toData(-420n),
          ],
          [
            LbV1.isPlutusDataInteger.toData(69n),
            LbV1.isPlutusDataInteger.toData(420n),
          ],
          [
            LbV1.isPlutusDataInteger.toData(1n),
            LbV1.isPlutusDataInteger.toData(2n),
          ],
        ],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcAssocMap(LbPrelude.eqInteger, fc.bigInt(), fc.bigInt()),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbAssocMap.isPlutusDataMap(
                LbV1.isPlutusDataInteger,
                LbV1.isPlutusDataInteger,
              ),
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("AssocMap insertion / lookup / member tests", () => {
    // Loosely, this tests whether a sequence of insertions and deletions
    // is the "same" as the map where the parameter is
    // ```
    // [ boolean        true ==> insert; false ==> delete
    // , number         key
    // , number         value
    // ][]              list of test cases
    // ```
    function sequenceOfInsertionsAndDeletionsCommutesWithMap(
      arr: [boolean, number, number][],
    ) {
      // `answers` will be the reference
      // implementation that we want to copy (we assume
      // it is correct)
      const answers: { [index: number]: number } = {};
      const assocMap: LbAssocMap.Map<bigint, bigint> = LbAssocMap.empty();

      for (const [insertOrDelete, k, v] of arr) {
        if (insertOrDelete) {
          answers[k] = v;
          LbAssocMap.insert(
            LbPrelude.eqInteger,
            BigInt(k),
            BigInt(v),
            assocMap,
          );

          // Assert that the key value pair is in
          // the map i.e.,  we can look it up
          assert.deepStrictEqual(
            LbAssocMap.lookup(LbPrelude.eqInteger, BigInt(k), assocMap),
            { name: "Just", fields: BigInt(v) },
          );

          // Assert that the key is a member of
          // the map
          assert.deepStrictEqual(
            LbAssocMap.member(LbPrelude.eqInteger, BigInt(k), assocMap),
            true,
          );
        } else {
          // delete
          delete answers[k];
          LbAssocMap.remove(LbPrelude.eqInteger, BigInt(k), assocMap);

          // Assert that the key value pair is in
          // the map i.e.,  we can look it up
          assert.deepStrictEqual(
            LbAssocMap.lookup(LbPrelude.eqInteger, BigInt(k), assocMap),
            { name: "Nothing" },
          );

          // Assert that the key is a member of
          // the map
          assert.deepStrictEqual(
            LbAssocMap.member(LbPrelude.eqInteger, BigInt(k), assocMap),
            false,
          );
        }

        // Verify the lengths are the same
        assert.deepStrictEqual(assocMap.length, Object.keys(answers).length);

        // Verify that the `answers` and our map
        // are the  "same"
        //
        // 1. Every element of `answers` is in  `assocMap`
        //
        // 2. Every element of `assocMap` is in  `answers`
        for (const [ka, va] of Object.entries(answers)) {
          assert.deepStrictEqual(
            LbAssocMap.lookup(LbPrelude.eqInteger, BigInt(ka), assocMap),
            { name: "Just", fields: BigInt(va) },
          );

          assert.deepStrictEqual(
            LbAssocMap.member(LbPrelude.eqInteger, BigInt(ka), assocMap),
            true,
          );
        }

        for (const [ka, va] of assocMap) {
          assert.deepStrictEqual(
            BigInt(answers[Number(ka)!]!),
            va,
          );
        }
      }
    }

    it(`Sequence of key value pair insertions and deletions`, () => {
      fc.assert(
        fc.property(
          fc.array(
            fc.tuple(
              fc.boolean(),
              fc.integer({ min: 0, max: 1000 }),
              fc.integer({ min: 0, max: 1000 }),
            ),
            { minLength: 100, maxLength: 500 },
          ),
          sequenceOfInsertionsAndDeletionsCommutesWithMap,
        ),
        {},
      );
    });

    it(`Sequence of key value pair insertions and deletions (small key space)`, () => {
      fc.assert(
        fc.property(
          fc.array(
            fc.tuple(
              fc.boolean(),
              fc.integer({ min: 0, max: 50 }),
              fc.integer({ min: 0, max: 1000 }),
            ),
            { minLength: 100, maxLength: 500 },
          ),
          sequenceOfInsertionsAndDeletionsCommutesWithMap,
        ),
        {},
      );
    });
  });
});
