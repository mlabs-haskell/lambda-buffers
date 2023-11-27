// Tests for the instances for `StakingCredential`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";
import * as LbPrelude from "lbr-prelude";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

import * as TestCredential from "./CredentialInstances-test.js";

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
const credential1: LbV1.Credential = {
  name: "PubKeyCredential",
  fields: pubKeyHash1,
};
const credential2: LbV1.Credential = {
  name: "ScriptCredential",
  fields: scriptHash1,
};

const stakingCredential1: LbV1.StakingCredential = {
  name: "StakingHash",
  fields: credential1,
};
const stakingCredential2: LbV1.StakingCredential = {
  name: "StakingHash",
  fields: credential2,
};
const stakingCredential3: LbV1.StakingCredential = {
  name: "StakingPtr",
  fields: [69n, 420n, -69n],
};

export function fcStakingCredential(): fc.Arbitrary<LbV1.StakingCredential> {
  const { stakingCredential } = fc.letrec((tie) => ({
    stakingCredential: fc.oneof({}, tie("StakingHash"), tie("StakingPtr")),
    StakingHash: fc.record({
      name: fc.constant("StakingHash"),
      fields: TestCredential.fcCredential(),
    }),
    StakingPtr: fc.record({
      name: fc.constant("StakingPtr"),
      fields: fc.tuple(fc.bigInt(), fc.bigInt(), fc.bigInt()),
    }),
  }));

  return stakingCredential as fc.Arbitrary<LbV1.StakingCredential>;
}

describe("StakingCredential tests", () => {
  /*
    // Some statistics on the generated data
        fc.statistics( fcStakingCredential(),
                        (stakingCredential) => {
                            return `${stakingCredential.name}`
                        }
                            ,
                            { numRuns: 100 },
                     );
    */

  describe("Eq Credential tests", () => {
    const dict = LbV1.eqStakingCredential;

    // Same credentials
    TestUtils.eqIt(dict, stakingCredential1, stakingCredential1, true);
    TestUtils.neqIt(dict, stakingCredential1, stakingCredential1, false);

    TestUtils.eqIt(dict, stakingCredential2, stakingCredential2, true);
    TestUtils.neqIt(dict, stakingCredential2, stakingCredential2, false);

    TestUtils.eqIt(dict, stakingCredential3, stakingCredential3, true);
    TestUtils.neqIt(dict, stakingCredential3, stakingCredential3, false);

    // Mixing the credentials
    TestUtils.eqIt(dict, stakingCredential1, stakingCredential2, false);
    TestUtils.neqIt(dict, stakingCredential1, stakingCredential2, true);

    TestUtils.eqIt(dict, stakingCredential2, stakingCredential1, false);
    TestUtils.neqIt(dict, stakingCredential2, stakingCredential1, true);

    TestUtils.eqIt(dict, stakingCredential3, stakingCredential2, false);
    TestUtils.neqIt(dict, stakingCredential3, stakingCredential2, true);

    TestUtils.eqIt(dict, stakingCredential2, stakingCredential3, false);
    TestUtils.neqIt(dict, stakingCredential2, stakingCredential3, true);

    TestUtils.eqIt(dict, stakingCredential1, stakingCredential3, false);
    TestUtils.neqIt(dict, stakingCredential1, stakingCredential3, true);

    TestUtils.eqIt(dict, stakingCredential3, stakingCredential1, false);
    TestUtils.neqIt(dict, stakingCredential3, stakingCredential1, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcStakingCredential(),
          fcStakingCredential(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        {
          examples: [
            [stakingCredential1, stakingCredential1],
            [stakingCredential2, stakingCredential2],
            [stakingCredential3, stakingCredential3],
            [stakingCredential2, stakingCredential3],
            [stakingCredential1, stakingCredential3],
            [stakingCredential3, stakingCredential1],
          ],
        },
      );
    });
  });

  describe("Json StakingCredential tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonStakingCredential,
      stakingCredential1,
      {
        fields: [LbV1.jsonCredential.toJson(credential1)],
        name: "StakingHash",
      },
    );

    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonStakingCredential,
      stakingCredential2,
      {
        fields: [LbV1.jsonCredential.toJson(credential2)],
        name: "StakingHash",
      },
    );
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonStakingCredential,
      stakingCredential3,
      {
        fields: [{
          "certificate_index": new LbPrelude.Scientific(-69n),
          "slot_number": new LbPrelude.Scientific(69n),
          "transaction_index": new LbPrelude.Scientific(420n),
        }],
        name: "StakingPtr",
      },
    );

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcStakingCredential(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonStakingCredential, data);
          },
        ),
        {
          examples: [[stakingCredential1], [stakingCredential2], [
            stakingCredential3,
          ]],
        },
      );
    });
  });

  describe("ToData/FromData StakingCredential tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataStakingCredential,
      LbV1.fromDataStakingCredential,
      stakingCredential1,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataCredential.toData(credential1),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataStakingCredential,
      LbV1.fromDataStakingCredential,
      stakingCredential2,
      {
        name: "Constr",
        fields: [0n, [
          LbV1.toDataCredential.toData(credential2),
        ]],
      },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataStakingCredential,
      LbV1.fromDataStakingCredential,
      stakingCredential3,
      {
        name: "Constr",
        fields: [1n, [
          LbV1.toDataInteger.toData(69n),
          LbV1.toDataInteger.toData(420n),
          LbV1.toDataInteger.toData(-69n),
        ]],
      },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fcStakingCredential(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataStakingCredential,
              LbV1.fromDataStakingCredential,
              data,
            );
          },
        ),
        {
          examples: [[stakingCredential1], [stakingCredential2], [
            stakingCredential3,
          ]],
        },
      );
    });
  });
});
