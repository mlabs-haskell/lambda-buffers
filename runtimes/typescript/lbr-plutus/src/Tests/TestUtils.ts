// Some utility functions to make testing easier.

import type { IsPlutusData } from "../LambdaBuffers/Runtime/PlutusData.js";
import * as LbPlutusData from "../LambdaBuffers/Runtime/PlutusData.js";
import * as LbPrelude from "lbr-prelude";
import type { Bool, Eq, Json } from "lbr-prelude";

import { it } from "node:test";
import * as assert from "node:assert/strict";

/**
 * Wraps `JSON.stringify` but allows printing bigints
 */
export function stringify<A>(arg: A): string {
  return JSON.stringify(arg, (_key, value) => {
    if (typeof value === "bigint") {
      return value + "";
    }
    return value;
  });
}

/**
 * `eqIt` wraps `it` for verifying that the Eq.eq instance is as expected
 */
export function eqIt<A>(dict: Eq<A>, l: A, r: A, expected: Bool) {
  it(`eq \`${stringify(l)}\` and \`${stringify(r)}\` should be \`${expected}\``, () => {
    assert.deepStrictEqual(dict.eq(l, r), expected);
  });
}

/**
 * `neqIt` wraps `it` for verifying that the Eq.neq instance is as expected
 */
export function neqIt<A>(dict: Eq<A>, l: A, r: A, expected: Bool) {
  it(`neq \`${stringify(l)}\` and \`${stringify(r)}\` should be \`${expected}\``, () => {
    assert.deepStrictEqual(dict.neq(l, r), expected);
  });
}

/**
 *  Asserts that `dict.eq(l,r) === ~dict.neq(l,r)`
 */
export function negationTest<A>(dict: Eq<A>, l: A, r: A) {
  const eqResult = dict.eq(l, r);
  const neqResult = dict.neq(l, r);

  assert.deepStrictEqual(eqResult, !neqResult);
}

/**
 * `toJsonFromJsonRoundTripIt` wraps `toJsonFromJsonRoundTrip` with `it`
 */
export function toJsonFromJsonRoundTripIt<A>(dict: Json<A>, v: A) {
  it(`toJson/fromJson roundtrip test for \`${stringify(v)}\``, () => {
    toJsonFromJsonRoundTrip(dict, v);
  });
}

/**
 * Asserts that
 *  ```
 *    ---toJson--->
 *    \             |
 *     \         fromJson
 *      id          |
 *        \         \/
 *         ------>
 *  ```
 *  commutes
 */
export function toJsonFromJsonRoundTrip<A>(dict: Json<A>, v: A) {
  const json = dict.toJson(v);
  assert.deepStrictEqual(dict.fromJson(json), v);
}

/**
 * `toJsonAndFromJsonIt` is a unit test to test that
 *  - `v : A` encodes to  `json : LbPrelude.Value`; and
 *  - `json : LbPrelude.Value` decodes to  `v : A`.
 */
export function toJsonAndFromJsonIt<A>(
  dict: Json<A>,
  v: A,
  json: LbPrelude.Value,
) {
  toJsonIt(dict, v, json);
  fromJsonIt(dict, json, v);
}

/**
 * `toJsonIt` is a unit test to verify that the provided value encodes to the
 * provided Json value.
 */
export function toJsonIt<A>(
  dict: Json<A>,
  v: A,
  expectedJson: LbPrelude.Value,
) {
  it(`toJson \`${stringify(v)}\` should encode to \`${stringify(expectedJson)}\``, () => {
    const json = dict.toJson(v);
    assert.deepStrictEqual(json, expectedJson);
  });
}

/**
 * `fromJsonIt` is a unit test to verify that the provided JSON value decodes encodes to the
 * provided argument
 */
export function fromJsonIt<A>(
  dict: Json<A>,
  json: LbPrelude.Value,
  expectedA: A,
) {
  it(`fromJson \`${stringify(json)}\` should decode to \`${stringify(expectedA)}\``, () => {
    const a = dict.fromJson(json);
    assert.deepStrictEqual(a, expectedA);
  });
}

/**
 * `isPlutusDataIt` is a unit test to test that
 *  - `v : A` encodes to  `pData : LbPlutusData.PlutusData`; and
 *  - `pData : LbPlutusData.PlutusData` decodes to  `v : A`.
 */
export function isPlutusDataIt<A>(
  dict: IsPlutusData<A>,
  v: A,
  pData: LbPlutusData.PlutusData,
) {
  it(`toData \`${stringify(v)}\` should encode to \`${stringify(pData)}\``, () => {
    const plutusData = dict.toData(v);
    assert.deepStrictEqual(plutusData, pData);
  });

  it(`fromData \`${stringify(pData)}\` should decode to \`${stringify(v)}\``, () => {
    const a = dict.fromData(pData);
    assert.deepStrictEqual(a, v);
  });
}

/**
 * Asserts that
 *  ```
 *    ---toData--->
 *    \             |
 *     \         fromData
 *      id          |
 *        \         \/
 *         ------>
 *  ```
 *  commutes
 */
export function isPlutusDataRoundTrip<A>(
  dictIsPlutusData: IsPlutusData<A>,
  v: A,
) {
  const plutusData = dictIsPlutusData.toData(v);
  assert.deepStrictEqual(dictIsPlutusData.fromData(plutusData), v);
}
