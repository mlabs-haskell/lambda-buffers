// // Tests for the instances for `Credential`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

import * as TestPubKeyHash from "./PubKeyHashInstances-test.js";
import * as TestScriptHash from "./ScriptHashInstances-test.js";

const pubKeyHash1 = LbPrelude.fromJust(
  LbV1.pubKeyHashFromBytes(
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

const pubKeyHash2 = LbPrelude.fromJust(
  LbV1.pubKeyHashFromBytes(Uint8Array.from(
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

const credential1: LbV1.Credential = {
  name: "PubKeyCredential",
  fields: pubKeyHash1,
};
const credential2: LbV1.Credential = {
  name: "PubKeyCredential",
  fields: pubKeyHash2,
};
const credential3: LbV1.Credential = {
  name: "ScriptCredential",
  fields: scriptHash1,
};
const credential4: LbV1.Credential = {
  name: "ScriptCredential",
  fields: scriptHash2,
};

export function fcCredential(): fc.Arbitrary<LbV1.Credential> {
  const { credential } = fc.letrec((tie) => ({
    credential: fc.oneof({}, tie("PubKeyCredential"), tie("ScriptCredential")),
    PubKeyCredential: fc.record({
      name: fc.constant("PubKeyCredential"),
      fields: TestPubKeyHash.fcPubKeyHash(),
    }),
    ScriptCredential: fc.record({
      name: fc.constant("ScriptCredential"),
      fields: TestScriptHash.fcScriptHash(),
    }),
  }));

  return credential as fc.Arbitrary<LbV1.Credential>;
}

describe("Credential tests", () => {
  /*
    // Statistics on the generated data
    fc.statistics( fcCredential(),
                    (credential) => {
                        return `${credential.name}`
                    }
                        ,
                        { numRuns: 100 },
                 );
    */
  describe("Eq Credential tests", () => {
    const dict = LbV1.eqCredential;

    // Same credentials
    TestUtils.eqIt(dict, credential1, credential1, true);
    TestUtils.neqIt(dict, credential1, credential1, false);

    TestUtils.eqIt(dict, credential2, credential2, true);
    TestUtils.neqIt(dict, credential2, credential2, false);

    TestUtils.eqIt(dict, credential3, credential3, true);
    TestUtils.neqIt(dict, credential3, credential3, false);

    TestUtils.eqIt(dict, credential4, credential4, true);
    TestUtils.neqIt(dict, credential4, credential4, false);

    // Mixing the credentials
    TestUtils.eqIt(dict, credential1, credential2, false);
    TestUtils.neqIt(dict, credential1, credential2, true);

    TestUtils.eqIt(dict, credential2, credential1, false);
    TestUtils.neqIt(dict, credential2, credential1, true);

    TestUtils.eqIt(dict, credential3, credential1, false);
    TestUtils.neqIt(dict, credential3, credential1, true);

    TestUtils.eqIt(dict, credential1, credential3, false);
    TestUtils.neqIt(dict, credential1, credential3, true);

    TestUtils.eqIt(dict, credential4, credential1, false);
    TestUtils.neqIt(dict, credential4, credential1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcCredential(),
          fcCredential(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [
            [credential1, credential1],
            [credential2, credential2],
            [credential3, credential3],
            [credential4, credential4],
            [credential1, credential4],
            [credential4, credential1],
          ],
        },
      );
    });
  });

  describe("Json Credential tests", () => {
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonCredential, credential1, {
      fields: [LbV1.jsonPubKeyHash.toJson(pubKeyHash1)],
      name: "PubKeyCredential",
    });
    TestUtils.toJsonAndFromJsonIt(LbV1.jsonCredential, credential3, {
      fields: [LbV1.jsonPubKeyHash.toJson(pubKeyHash1)],
      name: "ScriptCredential",
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcCredential(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonCredential, data);
          },
        ),
        {
          examples: [[credential1], [credential2], [credential3], [
            credential4,
          ]],
        },
      );
    });
  });

  describe("IsPlutusData Credential tests", () => {
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataCredential,
      credential1,
      {
        name: "Constr",
        fields: [0n, [LbV1.isPlutusDataPubKeyHash.toData(pubKeyHash1)]],
      },
    );
    TestUtils.isPlutusDataIt(
      LbV1.isPlutusDataCredential,
      credential3,
      {
        name: "Constr",
        fields: [1n, [LbV1.isPlutusDataScriptHash.toData(scriptHash1)]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcCredential(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbV1.isPlutusDataCredential,
              data,
            );
          },
        ),
        {
          examples: [[credential1], [credential2], [credential3], [
            credential4,
          ]],
        },
      );
    });
  });
});
