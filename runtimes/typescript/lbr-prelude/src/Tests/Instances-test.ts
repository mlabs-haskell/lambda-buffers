// Unit tests for functionality in `src/LambdaBuffers/Runtime/Prelude.ts`
import { describe, it } from "node:test";
import * as assert from "node:assert/strict";

import * as LbMap from "../LambdaBuffers/Runtime/Map.js";
import * as LbSet from "../LambdaBuffers/Runtime/Set.js";
import * as LbPrelude from "../LambdaBuffers/Prelude.js";
import { Scientific } from "../LambdaBuffers/Prelude.js";
import type {
  Bool,
  Char,
  Eq,
  Integer,
  Json,
  Value,
} from "../LambdaBuffers/Prelude.js";

/**
 * `eqInstanceIt` wraps `it` for verifying that the Eq instance is as expected
 */
function eqInstanceIt<A>(dict: Eq<A>, l: A, r: A, expected: Bool) {
  it(`${l} and ${r}`, () => {
    assert.deepStrictEqual(dict.eq(l, r), expected);
    assert.deepStrictEqual(dict.neq(l, r), !expected);
  });
}

/**
 * `jsonInstanceIt` wraps `it` for verifying that the Json instance is as
 * expected
 */
function jsonInstanceIt<A>(dict: Json<A>, arg: A, value: Value) {
  it(`${arg}`, () => {
    assert.deepStrictEqual(dict.toJson(arg), value);
    assert.deepStrictEqual(dict.fromJson(value), arg);
  });
}

describe("Bool instance tests", () => {
  describe("Eq Bool", () => {
    eqInstanceIt(LbPrelude.eqBool, true, true, true);
    eqInstanceIt(LbPrelude.eqBool, false, true, false);
    eqInstanceIt(LbPrelude.eqBool, true, false, false);
    eqInstanceIt(LbPrelude.eqBool, false, false, true);
  });

  describe("Json Bool", () => {
    jsonInstanceIt(LbPrelude.jsonBool, true, true);
    jsonInstanceIt(LbPrelude.jsonBool, false, false);
  });
});

describe("Integer instance tests", () => {
  describe("Eq Integer", () => {
    eqInstanceIt(LbPrelude.eqInteger, 0n, 0n, true);
    eqInstanceIt(LbPrelude.eqInteger, 10n, 10n, true);
    eqInstanceIt(LbPrelude.eqInteger, -10n, -10n, true);
    eqInstanceIt(LbPrelude.eqInteger, -10n, -13n, false);
    eqInstanceIt(LbPrelude.eqInteger, 3n, -13n, false);
  });

  describe("Json Integer", () => {
    jsonInstanceIt(LbPrelude.jsonInteger, 10n, Scientific.fromString("10"));
    jsonInstanceIt(LbPrelude.jsonInteger, 13n, Scientific.fromString("13"));
    jsonInstanceIt(
      LbPrelude.jsonInteger,
      -10n,
      Scientific.fromString("-10"),
    );
    jsonInstanceIt(
      LbPrelude.jsonInteger,
      -13n,
      Scientific.fromString("-13"),
    );
  });
});

describe("Bytes instance tests", () => {
  describe("Eq Bytes", () => {
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("a"),
      LbPrelude.bytesFromOctets("a"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("b"),
      LbPrelude.bytesFromOctets("b"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("1234"),
      LbPrelude.bytesFromOctets("1234"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("ab"),
      LbPrelude.bytesFromOctets("ab"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("ab"),
      LbPrelude.bytesFromOctets("a"),
      false,
    );
    eqInstanceIt(
      LbPrelude.eqBytes,
      LbPrelude.bytesFromOctets("b"),
      LbPrelude.bytesFromOctets("a"),
      false,
    );
  });

  describe("Json Bytes", () => {
    jsonInstanceIt(
      LbPrelude.jsonBytes,
      LbPrelude.bytesFromOctets("ilikedogs"),
      "aWxpa2Vkb2dz",
    );
    jsonInstanceIt(
      LbPrelude.jsonBytes,
      LbPrelude.bytesFromOctets("Your midas touch on your chevy door"),
      "WW91ciBtaWRhcyB0b3VjaCBvbiB5b3VyIGNoZXZ5IGRvb3I=",
    );
  });
});

describe("Char instance tests", () => {
  describe("Eq Char", () => {
    eqInstanceIt(
      LbPrelude.eqChar,
      LbPrelude.charFromString("a"),
      LbPrelude.charFromString("a"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqChar,
      LbPrelude.charFromString("b"),
      LbPrelude.charFromString("b"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqChar,
      LbPrelude.charFromString("1"),
      LbPrelude.charFromString("1"),
      true,
    );
    eqInstanceIt(
      LbPrelude.eqChar,
      LbPrelude.charFromString("a"),
      LbPrelude.charFromString("b"),
      false,
    );
    eqInstanceIt(
      LbPrelude.eqChar,
      LbPrelude.charFromString("b"),
      LbPrelude.charFromString("a"),
      false,
    );
  });

  describe("Json Char", () => {
    jsonInstanceIt(LbPrelude.jsonChar, LbPrelude.charFromString("i"), "i");
    jsonInstanceIt(
      LbPrelude.jsonChar,
      LbPrelude.charFromString("𠮷"),
      "𠮷",
    );
  });
});

describe("Text instance tests", () => {
  describe("Eq Text", () => {
    eqInstanceIt(LbPrelude.eqText, "aa", "aa", true);
    eqInstanceIt(LbPrelude.eqText, "bb", "bb", true);
    eqInstanceIt(LbPrelude.eqText, "1", "1", true);
    eqInstanceIt(LbPrelude.eqText, "a", "b", false);
    eqInstanceIt(LbPrelude.eqText, "b", "a", false);
  });

  describe("Json Text", () => {
    jsonInstanceIt(LbPrelude.jsonText, "aabbbc", "aabbbc");
  });
});

describe("Maybe instance tests", () => {
  describe("Eq Maybe", () => {
    eqInstanceIt(
      LbPrelude.eqMaybe(LbPrelude.eqInteger),
      { name: "Just", fields: 212n },
      { name: "Just", fields: 212n },
      true,
    );
    eqInstanceIt(
      LbPrelude.eqMaybe(LbPrelude.eqInteger),
      { name: "Just", fields: -212n },
      { name: "Just", fields: -212n },
      true,
    );
    eqInstanceIt(
      LbPrelude.eqMaybe(LbPrelude.eqInteger),
      { name: "Nothing" },
      { name: "Nothing" },
      true,
    );
    eqInstanceIt(
      LbPrelude.eqMaybe(LbPrelude.eqInteger),
      { name: "Nothing" },
      { name: "Just", fields: 212n },
      false,
    );
    eqInstanceIt(
      LbPrelude.eqMaybe(LbPrelude.eqInteger),
      { name: "Just", fields: 212n },
      { name: "Nothing" },
      false,
    );
  });

  describe("Json Maybe", () => {
    jsonInstanceIt(LbPrelude.jsonMaybe(LbPrelude.jsonInteger), {
      name: "Nothing",
    }, {
      name: "Nothing",
      fields: [],
    });
    jsonInstanceIt(LbPrelude.jsonMaybe(LbPrelude.jsonInteger), {
      name: "Just",
      fields: 12n,
    }, {
      name: "Just",
      fields: [Scientific.fromString("12")],
    });
  });
});

describe("Either instance tests", () => {
  describe("Eq Either", () => {
    eqInstanceIt(
      LbPrelude.eqEither(LbPrelude.eqInteger, LbPrelude.eqText),
      { name: "Left", fields: 212n },
      { name: "Left", fields: 212n },
      true,
    );
    eqInstanceIt(
      LbPrelude.eqEither(LbPrelude.eqInteger, LbPrelude.eqText),
      { name: "Right", fields: "ever green" },
      { name: "Right", fields: "ever green" },
      true,
    );
    eqInstanceIt(
      LbPrelude.eqEither(LbPrelude.eqInteger, LbPrelude.eqText),
      { name: "Left", fields: 212n },
      { name: "Right", fields: "ever green" },
      false,
    );
    eqInstanceIt(
      LbPrelude.eqEither(LbPrelude.eqInteger, LbPrelude.eqText),
      { name: "Right", fields: "ever green" },
      { name: "Left", fields: 212n },
      false,
    );
    describe("Json Either", () => {
      jsonInstanceIt(
        LbPrelude.jsonEither(LbPrelude.jsonInteger, LbPrelude.jsonText),
        { name: "Right", fields: "ever green" },
        { name: "Right", fields: ["ever green"] },
      );
      jsonInstanceIt(
        LbPrelude.jsonEither(LbPrelude.jsonInteger, LbPrelude.jsonText),
        { name: "Left", fields: 212n },
        { name: "Left", fields: [Scientific.fromString("212")] },
      );
    });
  });
});

describe("List instance tests", () => {
  describe("Eq List", () => {
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [1n],
      [1n],
      true,
    );
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [1n, 2n],
      [1n, 2n],
      true,
    );
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [],
      [],
      true,
    );
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [1n],
      [],
      false,
    );
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [1n, 2n],
      [2n, 1n],
      false,
    );
    eqInstanceIt(
      LbPrelude.eqList(LbPrelude.eqInteger),
      [],
      [1n],
      false,
    );
  });

  describe("Json List", () => {
    jsonInstanceIt(
      LbPrelude.jsonList(LbPrelude.jsonInteger),
      [1n, 2n, 3n],
      [
        Scientific.fromString("1"),
        Scientific.fromString("2"),
        Scientific.fromString("3"),
      ],
    );
    jsonInstanceIt(
      LbPrelude.jsonList(LbPrelude.jsonInteger),
      [],
      [],
    );
  });
});

describe("Pair instance tests", () => {
  describe("Eq Pair", () => {
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqInteger),
      [1n, 69n],
      [1n, 69n],
      true,
    );
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqInteger),
      [-69n, 2n],
      [-69n, 2n],
      true,
    );
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqChar),
      [-69n, LbPrelude.charFromString("a")],
      [-69n, LbPrelude.charFromString("a")],
      true,
    );

    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqInteger),
      [1n, 69n],
      [0n, 69n],
      false,
    );
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqInteger),
      [-69n, 2n],
      [-69n, 3n],
      false,
    );
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqChar),
      [-69n, LbPrelude.charFromString("b")],
      [-69n, LbPrelude.charFromString("a")],
      false,
    );
    eqInstanceIt(
      LbPrelude.eqPair(LbPrelude.eqInteger, LbPrelude.eqChar),
      [-69n, LbPrelude.charFromString("b")],
      [-68n, LbPrelude.charFromString("b")],
      false,
    );
  });

  describe("Json Pair", () => {
    jsonInstanceIt(
      LbPrelude.jsonPair(LbPrelude.jsonInteger, LbPrelude.jsonInteger),
      [1n, 2n] as [Integer, Integer],
      [
        Scientific.fromString("1"),
        Scientific.fromString("2"),
      ],
    );
    jsonInstanceIt(
      LbPrelude.jsonPair(LbPrelude.jsonInteger, LbPrelude.jsonChar),
      [1n, LbPrelude.charFromString("a")] as [Integer, Char],
      [
        Scientific.fromString("1"),
        "a",
      ],
    );
  });
});

describe("Set instance tests", () => {
  describe("Eq Set", () => {
    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      const set2: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 0n, set1);
      LbSet.insert(LbPrelude.ordInteger, 0n, set2);
      eqInstanceIt(
        LbPrelude.eqSet(LbPrelude.eqInteger),
        set1,
        set2,
        true,
      );
    }

    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      const set2: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);
      LbSet.insert(LbPrelude.ordInteger, 1n, set2);
      LbSet.insert(LbPrelude.ordInteger, 0n, set1);
      LbSet.insert(LbPrelude.ordInteger, 0n, set2);
      eqInstanceIt(
        LbPrelude.eqSet(LbPrelude.eqInteger),
        set1,
        set2,
        true,
      );
    }
    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      const set2: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);
      LbSet.insert(LbPrelude.ordInteger, 0n, set2);
      LbSet.insert(LbPrelude.ordInteger, 0n, set1);
      LbSet.insert(LbPrelude.ordInteger, 0n, set2);
      eqInstanceIt(
        LbPrelude.eqSet(LbPrelude.eqInteger),
        set1,
        set2,
        false,
      );
    }
  });

  describe("Json Set", () => {
    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);
      LbSet.insert(LbPrelude.ordInteger, 2n, set1);

      jsonInstanceIt(
        LbPrelude.jsonSet(LbPrelude.ordInteger, LbPrelude.jsonInteger),
        set1,
        [
          Scientific.fromString("1"),
          Scientific.fromString("2"),
        ],
      );
    }

    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);

      jsonInstanceIt(
        LbPrelude.jsonSet(LbPrelude.ordInteger, LbPrelude.jsonInteger),
        set1,
        [
          Scientific.fromString("1"),
        ],
      );
    }

    {
      const set1: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);
      LbSet.insert(LbPrelude.ordInteger, 1n, set1);

      const set2: LbSet.Set<Integer> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordInteger, 1n, set2);
      LbSet.insert(LbPrelude.ordInteger, 1n, set2);

      const set3: LbSet.Set<LbSet.Set<Integer>> = new LbSet.Set();
      LbSet.insert(LbPrelude.ordSet(LbPrelude.ordInteger), set1, set3);
      LbSet.insert(LbPrelude.ordSet(LbPrelude.ordInteger), set2, set3);

      jsonInstanceIt(
        LbPrelude.jsonSet(
          LbPrelude.ordSet(LbPrelude.ordInteger),
          LbPrelude.jsonSet(LbPrelude.ordInteger, LbPrelude.jsonInteger),
        ),
        set3,
        [
          [Scientific.fromString("1")],
        ],
      );
    }
  });
});

describe("Map instance tests", () => {
  describe("Eq Map", () => {
    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      const map2: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 0n, 0n, map1);
      LbMap.insert(LbPrelude.ordInteger, 0n, 0n, map2);
      eqInstanceIt(
        LbPrelude.eqMap(LbPrelude.eqInteger, LbPrelude.eqInteger),
        map1,
        map2,
        true,
      );
    }

    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      const map2: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map1);
      LbMap.insert(LbPrelude.ordInteger, 0n, 1n, map1);

      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map2);
      LbMap.insert(LbPrelude.ordInteger, 0n, 1n, map2);
      eqInstanceIt(
        LbPrelude.eqMap(LbPrelude.eqInteger, LbPrelude.eqInteger),
        map1,
        map2,
        true,
      );
    }
    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      const map2: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 0n, map1);
      LbMap.insert(LbPrelude.ordInteger, 0n, 1n, map1);

      LbMap.insert(LbPrelude.ordInteger, 0n, 0n, map2);
      LbMap.insert(LbPrelude.ordInteger, 0n, 1n, map2);
      eqInstanceIt(
        LbPrelude.eqMap(LbPrelude.eqInteger, LbPrelude.eqInteger),
        map1,
        map2,
        false,
      );
    }

    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      const map2: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 0n, 0n, map1);
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map1);

      LbMap.insert(LbPrelude.ordInteger, 0n, 0n, map2);
      LbMap.insert(LbPrelude.ordInteger, 1n, 0n, map2);
      eqInstanceIt(
        LbPrelude.eqMap(LbPrelude.eqInteger, LbPrelude.eqInteger),
        map1,
        map2,
        false,
      );
    }
  });

  describe("Json Map", () => {
    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 2n, map1);
      LbMap.insert(LbPrelude.ordInteger, 2n, 3n, map1);

      jsonInstanceIt(
        LbPrelude.jsonMap(
          LbPrelude.ordInteger,
          LbPrelude.jsonInteger,
          LbPrelude.jsonInteger,
        ),
        map1,
        [
          [Scientific.fromString("1"), Scientific.fromString("2")],
          [Scientific.fromString("2"), Scientific.fromString("3")],
        ],
      );
    }

    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 12n, map1);
      LbMap.insert(LbPrelude.ordInteger, 1n, 69n, map1);

      jsonInstanceIt(
        LbPrelude.jsonMap(
          LbPrelude.ordInteger,
          LbPrelude.jsonInteger,
          LbPrelude.jsonInteger,
        ),
        map1,
        [
          [Scientific.fromString("1"), Scientific.fromString("69")],
        ],
      );
    }

    {
      const map1: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map1);
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map1);

      const map2: LbMap.Map<Integer, Integer> = new LbMap.Map();
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map2);
      LbMap.insert(LbPrelude.ordInteger, 1n, 1n, map2);

      const map3: LbMap.Map<LbMap.Map<Integer, Integer>, Integer> = new LbMap
        .Map();
      LbMap.insert(
        LbPrelude.ordMap(LbPrelude.ordInteger, LbPrelude.ordInteger),
        map1,
        0n,
        map3,
      );
      LbMap.insert(
        LbPrelude.ordMap(LbPrelude.ordInteger, LbPrelude.ordInteger),
        map2,
        0n,
        map3,
      );

      jsonInstanceIt(
        LbPrelude.jsonMap(
          LbPrelude.ordMap(LbPrelude.ordInteger, LbPrelude.ordInteger),
          LbPrelude.jsonMap(
            LbPrelude.ordInteger,
            LbPrelude.jsonInteger,
            LbPrelude.jsonInteger,
          ),
          LbPrelude.jsonInteger,
        ),
        map3,
        [
          [
            [[Scientific.fromString("1"), Scientific.fromString("1")]],
            Scientific.fromString("0"),
          ],
        ],
      );
    }
  });
});
