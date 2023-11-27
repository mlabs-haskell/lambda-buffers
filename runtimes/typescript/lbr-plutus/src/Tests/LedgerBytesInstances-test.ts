// Tests for the instances for `LedgerBytes`
import * as LbV1 from "../LambdaBuffers/Runtime/PlutusLedgerApi/V1.js";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

describe("LedgerBytes tests", () => {
  describe("Eq LedgerBytes tests", () => {
    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fc.uint8Array(),
          fc.uint8Array(),
          (l, r) => {
            TestUtils.negationTest(LbV1.eqLedgerBytes, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json LedgerBytes tests", () => {
    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonLedgerBytes,
      Uint8Array.from([0x00]),
      "00",
    );

    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonLedgerBytes,
      Uint8Array.from([0x00, 0xff]),
      "00ff",
    );

    TestUtils.toJsonAndFromJsonIt(
      LbV1.jsonLedgerBytes,
      Uint8Array.from([]),
      "",
    );
    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fc.uint8Array(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(LbV1.jsonLedgerBytes, data);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("ToData/FromData LedgerBytes tests", () => {
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLedgerBytes,
      LbV1.fromDataLedgerBytes,
      Uint8Array.from([0xff, 0x00]),
      { name: "Bytes", fields: Uint8Array.from([0xff, 0x00]) },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLedgerBytes,
      LbV1.fromDataLedgerBytes,
      Uint8Array.from([0x00]),
      { name: "Bytes", fields: Uint8Array.from([0x00]) },
    );
    TestUtils.toDataAndFromDataIt(
      LbV1.toDataLedgerBytes,
      LbV1.fromDataLedgerBytes,
      Uint8Array.from([]),
      { name: "Bytes", fields: Uint8Array.from([]) },
    );

    it(`toData/fromData property based tests`, () => {
      fc.assert(
        fc.property(
          fc.uint8Array(),
          (data) => {
            TestUtils.toDataFromDataRoundTrip(
              LbV1.toDataLedgerBytes,
              LbV1.fromDataLedgerBytes,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
