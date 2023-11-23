import type { FromData, ToData } from "../LambdaBuffers/Runtime/PlutusData.js";
import * as LbPlutusData from "../LambdaBuffers/Runtime/PlutusData.js";
import type { Bool, Eq, Json } from "lbr-prelude";

import { describe, it } from "node:test";
import * as assert from "node:assert/strict";

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
 * `jsonRoundTripIt` does a roundtrip test
 */
function jsonRoundTripIt<A>(dict: Json<A>, v: A) {
  const json = dict.toJson(v);
  assert.deepStrictEqual(dict.fromJson(json), v);
}

/**
 * `plutusDataRoundtrip` does a roundtrip test
 */
function plutusDataRoundtrip<A>(
  dictFrom: FromData<A>,
  dictTo: ToData<A>,
  v: A,
) {
  const plutusData = dictTo.toData(v);
  assert.deepStrictEqual(dictFrom.fromData(plutusData), v);
}

describe("PlutusData tests", () => {
  describe("Eq PlutusData tests", () => {
    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Constr", fields: [0n, []] },
      { name: "Constr", fields: [0n, []] },
      true,
    );

    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      true,
    );

    eqInstanceIt(LbPlutusData.eqPlutusData, { name: "Integer", fields: 1n }, {
      name: "Integer",
      fields: 1n,
    }, true);

    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Bytes", fields: Uint8Array.from([0xff]) },
      { name: "Bytes", fields: Uint8Array.from([0xff]) },
      true,
    );

    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Bytes", fields: Uint8Array.from([0xff]) },
      { name: "Integer", fields: 1n },
      false,
    );

    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [1n, [{ name: "Integer", fields: 1n }]] },
      false,
    );
    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 0n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      false,
    );

    eqInstanceIt(
      LbPlutusData.eqPlutusData,
      { name: "List", fields: [{ name: "Integer", fields: 0n }] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      false,
    );
    eqInstanceIt(LbPlutusData.eqPlutusData, { name: "Map", fields: [] }, {
      name: "Map",
      fields: [],
    }, true);
    eqInstanceIt(LbPlutusData.eqPlutusData, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, true);
    eqInstanceIt(LbPlutusData.eqPlutusData, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "List", fields: [] }, { name: "Integer", fields: 0n }]],
    }, false);
  });

  describe("Json PlutusData tests", () => {
    jsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Constr",
      fields: [0n, []],
    });

    jsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    });
    jsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "List",
      fields: [{ name: "Integer", fields: 0n }],
    });

    jsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Integer",
      fields: 1n,
    });

    jsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Bytes",
      fields: Uint8Array.from([0xff]),
    });
  });

  describe("FromData / ToData PlutusData tests", () => {
    plutusDataRoundtrip(
      LbPlutusData.fromDataPlutusData,
      LbPlutusData.toDataPlutusData,
      { name: "Constr", fields: [0n, []] },
    );

    plutusDataRoundtrip(
      LbPlutusData.fromDataPlutusData,
      LbPlutusData.toDataPlutusData,
      {
        name: "Map",
        fields: [[{ name: "Integer", fields: 0n }, {
          name: "List",
          fields: [],
        }]],
      },
    );
    plutusDataRoundtrip(
      LbPlutusData.fromDataPlutusData,
      LbPlutusData.toDataPlutusData,
      { name: "List", fields: [{ name: "Integer", fields: 0n }] },
    );

    plutusDataRoundtrip(
      LbPlutusData.fromDataPlutusData,
      LbPlutusData.toDataPlutusData,
      { name: "Integer", fields: 1n },
    );

    plutusDataRoundtrip(
      LbPlutusData.fromDataPlutusData,
      LbPlutusData.toDataPlutusData,
      { name: "Bytes", fields: Uint8Array.from([0xff]) },
    );
  });
});

// TODO: Add some reasonable tests to Plutus Ledger API types.
