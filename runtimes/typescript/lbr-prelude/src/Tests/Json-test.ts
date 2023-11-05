// Unit tests for functionality in `src/LambdaBuffers/Runtime/Json.ts`
import { describe, it } from "node:test";
import * as assert from "node:assert/strict";

import * as LbPrelude from "../LambdaBuffers/Prelude.js";
import { Scientific } from "../LambdaBuffers/Prelude.js";
import type { Value } from "../LambdaBuffers/Prelude.js";

describe("Scientific tests", () => {
  // `scientificIt` wraps `it` to verify that the Scientific's  value is as
  // expected
  function scientificIt(
    scientific: Scientific,
    expectedCoefficient: bigint,
    expectedBase10: bigint,
  ): void {
    it(`${scientific.toString()} is ${expectedCoefficient}e${expectedBase10}`, () => {
      assert.strictEqual(scientific.coefficient, expectedCoefficient);
      assert.strictEqual(scientific.base10Exponent, expectedBase10);
    });
  }

  // Sanity tests for positive base10Exponent
  scientificIt(new Scientific(12n, 1n), 12n, 1n);
  scientificIt(new Scientific(120n, 1n), 12n, 2n);
  scientificIt(new Scientific(-120n, 1n), -12n, 2n);
  scientificIt(new Scientific(10000n, 0n), 1n, 4n);
  scientificIt(new Scientific(10010n, 0n), 1001n, 1n);
  scientificIt(new Scientific(-12000n, 3n), -12n, 6n);

  // Sanity tests for negative base10Exponent
  scientificIt(new Scientific(12n, -2n), 12n, -2n);
  scientificIt(new Scientific(120n, -1n), 12n, 0n);
  scientificIt(new Scientific(-120n, -5n), -12n, -4n);
  scientificIt(new Scientific(10000n, -4n), 1n, 0n);
  scientificIt(new Scientific(10010n, -4n), 1001n, -3n);

  // Edges case of normalizing 0
  scientificIt(new Scientific(0n, 0n), 0n, 0n);
  scientificIt(new Scientific(0n, 1n), 0n, 0n);
  scientificIt(new Scientific(0n, -1n), 0n, 0n);
  scientificIt(new Scientific(0n, 322344233421n), 0n, 0n);
  scientificIt(new Scientific(0n, -1234432423n), 0n, 0n);
});

describe("JSON Parsing Tests", () => {
  // `jsonFromStrIt` wraps `it` to verify that the parsed value is as
  // expected.
  function jsonFromStrIt(str: string, v: Value): void {
    it(str, () => {
      assert.deepStrictEqual(LbPrelude.parseJson(str), v);
    });
  }

  // Basic number parsing tests
  jsonFromStrIt(
    "1321432143211342342342114322312321241234123123432",
    new Scientific(1321432143211342342342114322312321241234123123432n, 0n),
  );
  jsonFromStrIt(
    "-1321432143211342342342114322312321241234123123432",
    new Scientific(-1321432143211342342342114322312321241234123123432n, 0n),
  );
  jsonFromStrIt(
    "99999999999999999999999990",
    new Scientific(9999999999999999999999999n, 1n),
  );
  jsonFromStrIt("1.25", new Scientific(125n, -2n));
  jsonFromStrIt("1.0", new Scientific(1n, -0n));
  jsonFromStrIt("0", new Scientific(0n, -0n));
  jsonFromStrIt("1.0000001", new Scientific(10000001n, -7n));
  jsonFromStrIt("1.0000000", new Scientific(1n, 0n));
  jsonFromStrIt("0.6969", new Scientific(6969n, -4n));
  jsonFromStrIt("696900", new Scientific(6969n, 2n));
  jsonFromStrIt(
    "0.0999999999999999999999999999999999999",
    new Scientific(999999999999999999999999999999999999n, -37n),
  );
  jsonFromStrIt("10e0000000", new Scientific(1n, 1n));
  jsonFromStrIt("1E0000000", new Scientific(1n, 0n));
  jsonFromStrIt("10e-0000000", new Scientific(1n, 1n));
  jsonFromStrIt("1E-0000000", new Scientific(1n, 0n));
  jsonFromStrIt("69e-5", new Scientific(69n, -5n));
  jsonFromStrIt("420e-5", new Scientific(42n, -4n));
  jsonFromStrIt("69e5", new Scientific(69n, 5n));
  jsonFromStrIt("420e5", new Scientific(42n, 6n));
  jsonFromStrIt("69e+5", new Scientific(69n, 5n));
  jsonFromStrIt("420e+5", new Scientific(42n, 6n));
  jsonFromStrIt("69E-5", new Scientific(69n, -5n));
  jsonFromStrIt("420E-5", new Scientific(42n, -4n));
  jsonFromStrIt("69E5", new Scientific(69n, 5n));
  jsonFromStrIt("420E5", new Scientific(42n, 6n));
  jsonFromStrIt("69E+5", new Scientific(69n, 5n));
  jsonFromStrIt("420E+5", new Scientific(42n, 6n));
  jsonFromStrIt("-69E-5", new Scientific(-69n, -5n));
  jsonFromStrIt("-420E-5", new Scientific(-42n, -4n));
  jsonFromStrIt("-0", new Scientific(-0n, -0n));
  jsonFromStrIt("-0.1", new Scientific(-1n, -1n));

  // Basic string parsing tests
  jsonFromStrIt('""', "");
  jsonFromStrIt('"\\""', '"');
  jsonFromStrIt('"\\\\"', "\\");
  jsonFromStrIt('"\\b"', "\b");
  jsonFromStrIt('"\\f"', "\f");
  jsonFromStrIt('"\\n"', "\n");
  jsonFromStrIt('"\\r"', "\r");
  jsonFromStrIt('"\\t"', "\t");
  jsonFromStrIt('"\\u0a0a"', "\u0a0a");
  jsonFromStrIt('"\\u6969"', "\u6969");
  jsonFromStrIt('"\\uFFFF"', "\uFFFF");
  jsonFromStrIt('"\\uDEAD"', "\uDEAD");
  jsonFromStrIt('"\\"  "  ', '"  ');
  jsonFromStrIt('"\\\\  "', "\\  ");
  jsonFromStrIt('"\\b  "', "\b  ");
  jsonFromStrIt('"\\f  "', "\f  ");
  jsonFromStrIt('"\\n  "', "\n  ");
  jsonFromStrIt('"\\r  "', "\r  ");
  jsonFromStrIt('"\\t  "', "\t  ");
  jsonFromStrIt('"\\u0a0a"', "\u0a0a");
  jsonFromStrIt('"\\u6969"', "\u6969");
  jsonFromStrIt('"\\uFFFF"', "\uFFFF");
  jsonFromStrIt('"\\uDEAD"', "\uDEAD");
  jsonFromStrIt('"dog"', "dog");
  jsonFromStrIt('"pomeranian"', "pomeranian");
  jsonFromStrIt('"yorkie"', "yorkie");
  jsonFromStrIt('"狗"', "狗");
  jsonFromStrIt(
    '"So casually cruel in the name of being honest"',
    "So casually cruel in the name of being honest",
  );
  jsonFromStrIt('"我喜欢吃包子"', "我喜欢吃包子");

  // true / false / null tests
  jsonFromStrIt("true", true);
  jsonFromStrIt("false", false);
  jsonFromStrIt("null", null);
  jsonFromStrIt("  true", true);
  jsonFromStrIt("  false", false);
  jsonFromStrIt("  null", null);
  jsonFromStrIt("  true ", true);
  jsonFromStrIt("  false  ", false);
  jsonFromStrIt("  null  ", null);

  // Array tests
  jsonFromStrIt("[] ", []);
  jsonFromStrIt("[ ] ", []);
  jsonFromStrIt('[ " A" ] ', [" A"]);
  jsonFromStrIt('[ " A", "B" ] ', [" A", "B"]);
  jsonFromStrIt('[ " A", []] ', [" A", []]);
  jsonFromStrIt('[ [" A"], []] ', [[" A"], []]);
  jsonFromStrIt('[ [" A"], ["B"]] ', [[" A"], ["B"]]);
  jsonFromStrIt('[ [" A"], ["B",123], 123] ', [[" A"], [
    "B",
    new Scientific(123n, 0n),
  ], new Scientific(123n, 0n)]);
  jsonFromStrIt("[ true, true, false , false, null, [[],null]] ", [
    true,
    true,
    false,
    false,
    null,
    [[], null],
  ]);
  jsonFromStrIt('[ { "woohoo" : true, "wahoo": false} ] ', [{
    "woohoo": true,
    "wahoo": false,
  }]);

  // Object tests
  jsonFromStrIt("{}", {});
  jsonFromStrIt("{ }", {});
  jsonFromStrIt('{"dog":12}', { dog: new Scientific(12n, 0n) });
  jsonFromStrIt(' {"dog":12}', { dog: new Scientific(12n, 0n) });
  jsonFromStrIt(' { "dog":12}', { dog: new Scientific(12n, 0n) });
  jsonFromStrIt(' { "dog" :12}', { dog: new Scientific(12n, 0n) });
  jsonFromStrIt(' { "dog" : 12}', { dog: new Scientific(12n, 0n) });
  jsonFromStrIt(' { "dog" : 12, "cat" : null}', {
    dog: new Scientific(12n, 0n),
    cat: null,
  });
  jsonFromStrIt(' { "dog" : "kitty", "cat" : [ {  } ]   }  ', {
    dog: "kitty",
    cat: [{}],
  });

  // Whitespace
  jsonFromStrIt("\t\r\n true ", true);
  jsonFromStrIt("true \t\r\n ", true);
});

describe("JSON Parsing Invalid Tests", () => {
  // `jsonFromStrIt` wraps `it` to verify that the parsed value is as
  // expected.
  function jsonFromStrIt(str: string): void {
    it(str, () => {
      try {
        LbPrelude.parseJson(str);
      } catch (_error) {
        return;
      }
      throw new Error("Parse should be invalid.");
    });
  }

  jsonFromStrIt("{");
  jsonFromStrIt("}");
  jsonFromStrIt("[ true true true");
  jsonFromStrIt("flse");
  jsonFromStrIt("falsee");
  jsonFromStrIt("nill");
  jsonFromStrIt("nuull");
  jsonFromStrIt("+12");
  jsonFromStrIt("}{");
  jsonFromStrIt("truee");
  jsonFromStrIt("{ missingquotes : 32 }");
  jsonFromStrIt("01"); // leading 0s are not allowed
  jsonFromStrIt("01e001");

  // The following _must_ be escaped for strings
  jsonFromStrIt('"\n"');
  jsonFromStrIt('"""');

  // TODO(jaredponn): Change the test runner / PR node a fix.. apparently the
  // node's test runner doesn't support having tests with such garbage
  // characters since some intermediate lexer will throw an exception...
  // jsonFromStrIt('"\u0000"');
  // jsonFromStrIt('"\u0001"');
  // jsonFromStrIt('"\u0002"');
  // jsonFromStrIt('"\u0003"');
  // jsonFromStrIt('"\u0004"');
  // jsonFromStrIt('"\u0005"');
  // jsonFromStrIt('"\u0006"');
  // jsonFromStrIt('"\u0007"');
  // jsonFromStrIt('"\u0008"');
  // jsonFromStrIt('"\u0009"');
  // jsonFromStrIt('"\u000a"');
  // jsonFromStrIt('"\u000b"');
  // jsonFromStrIt('"\u000c"');
  // jsonFromStrIt('"\u000d"');
  // jsonFromStrIt('"\u000e"');
  // jsonFromStrIt('"\u000f"');
  // jsonFromStrIt('"\u0010"');
  // jsonFromStrIt('"\u0011"');
  // jsonFromStrIt('"\u0012"');
  // jsonFromStrIt('"\u0013"');
  // jsonFromStrIt('"\u0014"');
  // jsonFromStrIt('"\u0015"');
  // jsonFromStrIt('"\u0016"');
  // jsonFromStrIt('"\u0017"');
  // jsonFromStrIt('"\u0018"');
  // jsonFromStrIt('"\u0019"');
  // jsonFromStrIt('"\u001a"');
  // jsonFromStrIt('"\u001b"');
  // jsonFromStrIt('"\u001c"');
  // jsonFromStrIt('"\u001d"');
  // jsonFromStrIt('"\u001e"');
  // jsonFromStrIt('"\u001f"');
});

describe("JSON Parsing Value Roundtrip Tests", () => {
  // `jsonIt` wraps `it` to verify that `this map` in the following diagram
  // is the identity map
  //  ```
  //  Value -stringify()-> string
  //     \                   |
  //     this map         parseJson()
  //         \               |
  //           \             \/
  //             ---------> Value
  //  ```
  function jsonIt(val: Value): void {
    it(`${LbPrelude.stringify(val)} round trip`, () => {
      assert.deepStrictEqual(
        val,
        LbPrelude.parseJson(LbPrelude.stringify(val)),
      );
    });
  }

  jsonIt({ "dog": true });
  jsonIt(
    new Scientific(
      1234123413241264322682167126432121673196146743126149766431643214127614362416349423192n,
      1000n,
    ),
  );
  jsonIt(
    new Scientific(
      -1234123413241264322682167126432121673196146743126149766431643214127614362416349423192n,
      -1000n,
    ),
  );
  jsonIt(
    new Scientific(
      -1234123413241264322682167126432121673196146743126149766431643214127614362416349423192n,
      1000n,
    ),
  );
  jsonIt([true, false, null]);
  jsonIt({
    "woohoo": Scientific.fromString("-100e2"),
    "wahoo": [null],
    "wohooo": null,
    "wahooo": "some string   ",
  });
  jsonIt('\n\t \\ " ');
  jsonIt({
    "one": ["for", "the", "money"],
    "two": { "for": "the", "show": [] },
    "I was": { "never": { "ready": { "so": "I" } } },
    "watch": ["you", "go"],
  });
  jsonIt("我喜欢吃包子");
  jsonIt("\u0000");
});
