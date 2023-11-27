// Tests for the instances for `ScriptHash`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

// `scriptHash1` is distinct from `scriptHash2`
export const scriptHash1: LbV1.ScriptHash = LbPrelude.fromJust(
  LbV1.scriptHashFromBytes(
    Uint8Array.from([
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
    ]),
  ),
);
export const scriptHash2: LbV1.ScriptHash = LbPrelude.fromJust(
  LbV1.scriptHashFromBytes(Uint8Array.from(
    [
      28,
      27,
      26,
      25,
      24,
      23,
      22,
      21,
      20,
      19,
      18,
      17,
      16,
      15,
      14,
      13,
      12,
      11,
      10,
      9,
      8,
      7,
      6,
      5,
      4,
      3,
      2,
      1,
    ],
  )),
);

export function fcScriptHash(): fc.Arbitrary<LbV1.ScriptHash> {
  return fc.uint8Array({ minLength: 28, maxLength: 28 }).map((t) => {
    return LbPrelude.fromJust(LbV1.scriptHashFromBytes(t));
  });
}

describe("ScriptHash tests", () => {
  describe("Eq ScriptHash tests", () => {
    const dict = LbV1.eqScriptHash;

    TestUtils.eqIt(dict, scriptHash1, scriptHash1, true);
    TestUtils.eqIt(dict, scriptHash2, scriptHash1, false);
    TestUtils.eqIt(dict, scriptHash1, scriptHash2, false);
    TestUtils.eqIt(dict, scriptHash2, scriptHash2, true);

    TestUtils.neqIt(dict, scriptHash1, scriptHash1, false);
    TestUtils.neqIt(dict, scriptHash2, scriptHash1, true);
    TestUtils.neqIt(dict, scriptHash1, scriptHash2, true);
    TestUtils.neqIt(dict, scriptHash2, scriptHash2, false);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcScriptHash(),
          fcScriptHash(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [[scriptHash1, scriptHash1], [scriptHash1, scriptHash2], [
            scriptHash2,
            scriptHash1,
          ], [scriptHash2, scriptHash2]],
        },
      );
    });
  });

  describe("Json ScriptHash tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonScriptHash,
      scriptHash2,
      "1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201",
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonScriptHash,
      scriptHash1,
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c",
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcScriptHash(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonScriptHash, data);
          },
        ),
        { examples: [[scriptHash1], [scriptHash2]] },
      );
    });
  });

  describe("ToData/FromData ScriptHash tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataScriptHash,
      LbV1.fromDataScriptHash,
      scriptHash2,
      { name: "Bytes", fields: scriptHash2 },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataScriptHash,
      LbV1.fromDataScriptHash,
      scriptHash1,
      { name: "Bytes", fields: scriptHash1 },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcScriptHash(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataScriptHash,
              LbV1.fromDataScriptHash,
              data,
            );
          },
        ),
        { examples: [[scriptHash1], [scriptHash2]] },
      );
    });
  });
});
