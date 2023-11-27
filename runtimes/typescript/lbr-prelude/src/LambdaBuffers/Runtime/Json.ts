import type { Ord } from "./Ord.js";
import * as LbMap from "./Map.js";
/**
 * `Json<A>` is a type class to provide `toJson` and `fromJson` instances for
 * type `A`.
 *
 * @remarks
 * `fromJson` throws an exception (most likely {@link JsonError}) when it fails.
 */
export interface Json<A> {
  readonly toJson: (arg: Readonly<A>) => Value;
  readonly fromJson: (arg: Readonly<Value>) => A;
}

/**
 * `Value` is a JSON value.
 *
 * @remarks
 * This closely follows the Haskell library {@link https://hackage.haskell.org/package/aeson | aeson}.
 *
 * Note the custom type {@link Scientific} is used to represent arbitrarily large integers.
 */
export type Value =
  | { [index: string]: Value }
  | Value[]
  | string
  | Scientific
  | boolean
  | null;

export function isJsonObject(
  value: Readonly<Value>,
): value is { [index: string]: Value } {
  return value !== null && value instanceof Object &&
    !(value instanceof Scientific) && !(value instanceof Array);
}

export function isJsonArray(value: Readonly<Value>): value is Value[] {
  return value !== null && value instanceof Array;
}

export function isJsonString(value: Readonly<Value>): value is string {
  return typeof value === "string";
}

export function isJsonNumber(value: Readonly<Value>): value is Scientific {
  return value !== null && value instanceof Scientific;
}

export function isJsonNull(value: Readonly<Value>): value is null {
  return value === null;
}

export function isJsonBoolean(value: Readonly<Value>): value is boolean {
  return typeof value === "boolean";
}

/**
 * `JsonError` is an exception thrown when `fromJson` fails.
 */
export class JsonError extends Error {
  constructor(message: string) {
    super(message);
  }
}

/**
 * Loosely resembles {@link https://hackage.haskell.org/package/scientific-0.3.7.0 | scientific}
 *
 * A scientific number with `coefficient` and `base10Exponent` corresponds to the number
 * ```
 * coefficient * 10 ^ base10Exponent
 * ```
 *
 * Invariants
 *
 * - if `coefficient` is 0, then `base10Exponent` is 0.
 *
 * - Otherwise, `coefficient` has no trailing 0s
 *
 * @internal
 */
export class Scientific {
  // TODO(jaredponn): these probably should be private otherwise people can
  // break the invariants. But! This is _internal_ anyways..
  coefficient: bigint;
  base10Exponent: bigint;

  /**
   * @param coefficient - the coefficient
   * @param base10Exponent - the base 10 exponent (0 if not provided)
   */
  constructor(coefficient: bigint, base10Exponent?: bigint) {
    if (coefficient === 0n) {
      this.coefficient = 0n;
      this.base10Exponent = 0n;
    } else {
      if (base10Exponent === undefined) {
        base10Exponent = 0n;
      }
      // Remove the trailing 0s by "shifting" powers of 10 over.
      while (coefficient % 10n === 0n) {
        coefficient = coefficient / 10n;
        base10Exponent = base10Exponent + 1n;
      }

      this.coefficient = coefficient;
      this.base10Exponent = base10Exponent;
    }
  }

  /**
   * `toString()` returns
   * ```
   * <coefficient>e<base10Exponent>
   * ```
   * if `base10Exponent` is nonzero,
   * and
   * ```
   * <coefficient>
   * ```
   * otherwise.
   */
  toString(): string {
    return this.coefficient.toString() +
      (this.base10Exponent === 0n
        ? ""
        : ("e" + this.base10Exponent.toString()));
  }

  /**
   * `fromString()` uses {@link parseJson} to parse a Scientific number
   */
  static fromString(str: string): Scientific {
    const val: Value = parseJson(str);
    if (val instanceof Scientific) {
      return val;
    } else {
      throw new Error("Invalid Scientific");
    }
  }
}

/**
 * `stringify` serializes a JSON Value. Note that this differs from
 * `JSON.stringify` in the following points.
 *
 * - Stringification of {@link Scientific} uses its `toString()` method s.t.
 *   such numbers are printed as e.g. `10e5`, or `10`, etc.
 *
 * @privateRemarks
 * Unfortunately, using `JSON.stringify` with the `replacer` argument is broken
 * e.g.
 * ```
 * return JSON.stringify(input, (_key, value) => {
 *   if (value instanceof Scientific) {
 *     return value.toString();
 *   } else {
 *     return value;
 *   }
 * });
 * ```
 * since it'll print something like `999999999999999999999999999` as
 * `"999999999999999999999999999"`.
 */
export function stringify(input: Readonly<Value>): string {
  const strs: string[] = [];

  function printValue(val: Readonly<Value>): void {
    if (isJsonObject(val)) {
      strs.push("{");
      const entries = Object.entries(val);

      if (entries.length !== 0) {
        strs.push(`"${entries[0]![0]}":`);
        printValue(entries[0]![1]);

        for (let i = 1; i < entries.length; ++i) {
          strs.push(`,${JSON.stringify(entries[i]![0])}:`);
          printValue(entries[i]![1]);
        }
      }
      strs.push("}");
    } else if (isJsonArray(val)) {
      strs.push("[");

      if (val.length !== 0) {
        printValue(val[0]!);

        for (let i = 1; i < val.length; ++i) {
          strs.push(`,`);
          printValue(val[i]!);
        }
      }
      strs.push("]");
    } else if (isJsonString(val)) {
      strs.push(JSON.stringify(val));
    } else if (isJsonNumber(val)) {
      strs.push(val.toString());
    } else if (isJsonNull(val)) {
      strs.push("null");
    } else if (isJsonBoolean(val)) {
      if (val) {
        strs.push("true");
      } else {
        strs.push("false");
      }
    }
  }

  printValue(input);

  return strs.join("");
}

/**
 * This matches the corresponding Haskell function in `lbr-prelude`.
 *
 * @example
 * `jsonConstructor(title, fields)` produces a JSON value as follows
 *
 * ```
 * { "name" : <title>
 * , "fields" : <fields>
 * }
 * ```
 * where `<title>` and `<fields>` are the parameters passed into the function.
 *
 * @internal
 */
export function jsonConstructor(title: string, fields: Value[]): Value {
  return { name: title, fields: fields };
}

/**
 * This matches the corresponding Haskell function in `lbr-prelude`.
 *
 * @example
 * `caseJsonConstructor(title, [ [ctor1, fromJsonFields1 ], ..., [ctorN, fromJsonFieldsN ] ], value)`
 * parses the JSON value `value` of the form
 *
 * ```
 * { "name" : <ctori>
 * , "fields" : <fromJsonFieldsi>
 * }
 * ```
 * where
 *
 * - `<ctori>` are the arguments passed in;
 *
 * - `<fromJsonFieldsi>` is the result of `fromJsonFieldsi`; and
 *
 * - `<title> === <ctori>` for some `i`.
 *
 * @internal
 */
export function caseJsonConstructor<A>(
  title: string,
  fromJsonFields: { [index: string]: (fields: Value[]) => A },
  value: Readonly<Value>,
): A {
  if (isJsonObject(value)) {
    const ctori = value["name"];

    if (!(ctori !== undefined && isJsonString(ctori))) {
      throw new JsonError(
        `Expected a string for member name for type ${title}`,
      );
    }

    const fromJsoni = fromJsonFields[ctori];

    if (fromJsoni === undefined) {
      throw new JsonError(`Unexpected name ${ctori}`);
    }

    const fields = value["fields"];

    if (!(fields !== undefined && isJsonArray(fields))) {
      throw new JsonError(`Expected member fields to be an array`);
    }

    return fromJsoni(fields);
  } else {
    throw new JsonError(`Expected JSON object`);
  }
}

/**
 * Corresponds to the Haskell function
 * @internal
 */
export function caseJsonMap<K, V>(
  _title: string,
  dictOrd: Ord<K>,
  parseElem: (arg: [Value, Value]) => [K, V],
  value: Readonly<Value>,
): LbMap.Map<K, V> {
  // TODO(jaredponn): actually use `title` in the error messages.
  const map: LbMap.Map<K, V> = new LbMap.Map();

  if (!isJsonArray(value)) {
    throw new JsonError(`Expected JSON array`);
  }

  for (const kv of value) {
    if (!(isJsonArray(kv) && kv.length == 2)) {
      throw new JsonError(`Expected JSON array`);
    }

    const [k, v] = parseElem(kv as [Value, Value]);

    LbMap.insert(dictOrd, k, v, map);
  }

  if (LbMap.toList(map).length !== value.length) {
    throw new JsonError(`Map should have unique keys`);
  }

  return map;
}

/**
 * Corresponds to the Haskell function
 *
 * @internal
 */
export function caseJsonArray<A>(
  _title: string,
  parseElem: (arg: Value) => A,
  value: Readonly<Value>,
): A[] {
  // TODO(jaredponn): actually use `title` in the error messages.
  if (isJsonArray(value)) {
    return value.map(parseElem);
  } else {
    throw new JsonError("JSON Value is not an array");
  }
}

/**
 * Corresponds to the Haskell function (.:)
 * @internal
 */
export function caseFieldWithValue<A>(
  title: string,
  parseValue: (arg: Value) => A,
  value: Readonly<Value>,
): A {
  if (isJsonObject(value)) {
    const v = value[title];
    if (v === undefined) {
      throw new JsonError(`Expected JSON value to include field ${title}`);
    }

    return parseValue(v);
  } else {
    throw new JsonError("JSON Value is not an object");
  }
}

/**
 * {@link parseJson} parses a JSON value.
 *
 * @remarks
 * This differs from {@link https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse | `JSON.parse()`}
 * as `parseJson` will correctly parse {@link bigint}s
 *
 * @throws {@link SyntaxError}
 * Thrown if the provided string is invalid JSON.
 */
export function parseJson(input: string): Value {
  // Remarks:
  //  - Using the iterator is necessary (as opposed to simply indexing bytes
  //  in the string) since JS strings are UTF16, but 16 bits are not enough
  //  to encode all Unicode code points, so some UTF16 characters require 2
  //  array elements in the underlying string.
  //  Fortunately, using the iterator is enough to handle this mess for
  //  us as it iterates through the string by Unicode code points.
  const iterator: IterableIterator<string> = input[Symbol.iterator]();

  // `theChar`: the current character being read
  // `theCharPos`: the kth unicode codepoint in the original string (only used for error messages)
  let theChar: IteratorYieldResult<string> = iterator
    .next() as IteratorYieldResult<string>;
  let theCharPos = 0;

  // See:
  //  [1] https://www.json.org/json-en.html
  //  [2] https://www.crockford.com/mckeeman.html
  //  [3] https://datatracker.ietf.org/doc/html/rfc8259
  // for JSON grammar.
  //
  // In particular, every function prefixed with a lower case 'p'
  // will be a parser for a production in [2] with [2]'s grammar just above
  // it in comments.

  // Either returns
  //  - the character (as a string); or
  //  - empty string if we're at EOF
  function peek(): string {
    if (theChar.done) {
      return "";
    } else {
      if (theChar.value === undefined) {
        throw new Error("Code point is undefined");
      }
      return theChar.value;
    }
  }

  // Consumes the current character so subsequent calls to to `peek()`
  // returns the next character in the input.
  // Returns `peek()`
  function consume(): string {
    theCharPos = theCharPos + 1;
    theChar = iterator.next() as IteratorYieldResult<string>;

    return peek();
  }

  function bad(unexpected: string): never {
    if (unexpected === "") {
      throw new SyntaxError("Unexpected end of JSON input");
    } else {
      throw new SyntaxError(
        ["Unexpected token", unexpected, "in JSON at position", theCharPos]
          .join(" "),
      );
    }
  }

  // ws
  //      ""
  //      '0020' ws
  //      '000A' ws
  //      '000D' ws
  //      '0009' ws
  //
  function pWs(): void {
    // deno-lint-ignore no-control-regex
    while (peek().match(/^[\u{0020}\u{000A}\u{000D}\u{0009}]$/u)) consume();
  }

  // element
  //      ws value ws
  function pElement(): Value {
    pWs();
    const value: Value = pValue();
    pWs();
    return value;
  }

  // hex
  //     digit
  //     'A' . 'F'
  //     'a' . 'f'
  // digit
  //     '0'
  //     onenine
  //
  // onenine
  //     '1' . '9'
  function pHex(): string {
    const c: string = peek();
    if (c.match(/^[A-Fa-f0-9]$/)) {
      consume();
      return c;
    }
    bad(c);
  }

  // string
  //     '"' characters '"'
  //
  // characters
  //     ""
  //     character characters
  //
  // character
  //     '0020' . '10FFFF' - '"' - '\'
  //     '\' escape
  //
  // escape
  //     '"'
  //     '\'
  //     '/'
  //     'b'
  //     'f'
  //     'n'
  //     'r'
  //     't'
  //     'u' hex hex hex hex
  function pString(): string {
    let c: string = peek();
    const strs: string[] = [];

    if (c === '"') {
      c = consume();

      let cCodePoint: number | undefined;

      while (
        (cCodePoint = c.codePointAt(0)),
          (!(cCodePoint === undefined) &&
            0x0020 <= cCodePoint &&
            cCodePoint <= 0x10FFFF)
      ) {
        if (c === '"') {
          consume();
          return strs.join("");
        } else if (c === "\\") {
          c = consume();
          switch (c) {
            case '"':
              c = consume();
              strs.push('"');
              break;
            case "\\":
              c = consume();
              strs.push("\\");
              break;
            case "/":
              c = consume();
              strs.push("\/");
              break;
            case "b":
              c = consume();
              strs.push("\b");
              break;
            case "f":
              c = consume();
              strs.push("\f");
              break;
            case "n":
              c = consume();
              strs.push("\n");
              break;
            case "r":
              c = consume();
              strs.push("\r");
              break;
            case "t":
              c = consume();
              strs.push("\t");
              break;
            case "u": {
              consume();
              const hexes: string = pHex() + pHex() + pHex() + pHex();
              c = peek();
              strs.push(String.fromCodePoint(parseInt(hexes, 16)));
              break;
            }
            default:
              bad(c);
          }
        } else {
          strs.push(c);
          c = consume();
        }
      }
    }

    bad(c);
  }

  // object
  //     '{' ws '}'
  //     '{' members '}'
  // members
  //     member
  //     member ',' members
  //
  function pObject(): Value {
    let c: string = peek();

    // Remark: in all instances where this is called, we verify that the
    // character is `{` so the following is unnecessary
    // ```
    // if (!(c === '{')) {
    //     bad(c);
    // }
    // ```
    consume();
    pWs();
    c = peek();

    const members: { [index: string]: Value } = {};
    function pMember(): void {
      // member
      //     ws string ws ':' element
      pWs();
      const memberKey: string = pString();
      pWs();

      pChar(":");

      const memberValue = pElement();

      members[memberKey] = memberValue;
    }

    if (c === "}") {
      consume();
      return members;
    } else {
      pMember();

      while (peek() === ",") {
        consume();
        pMember();
      }

      pChar("}");

      return members;
    }
  }

  // array
  //     '[' ws ']'
  //     '[' elements ']'
  //
  // elements
  //     element
  //     element ',' elements
  function pArray(): Value {
    let c: string = peek();
    const array: Value[] = [];

    // Remark: in all instances where this is called, we verify that the
    // character is `[` so the following is unnecessary
    // ```
    // if (!(c === '[')) {
    //     bad(c);
    // }
    // ```

    consume();

    pWs();

    c = peek();
    if (c === "]") {
      consume();
      return array;
    }

    array.push(pElement());

    while (peek() === ",") {
      consume();
      array.push(pElement());
    }

    pChar("]");

    return array;
  }

  // number
  //     integer fraction exponent
  //
  // integer
  //     digit
  //     onenine digits
  //     '-' digit
  //     '-' onenine digits
  // digits
  //     digit
  //     digit digits
  // digit
  //     '0'
  //     onenine
  // onenine
  //     '1' . '9'
  // fraction
  //     ""
  //     '.' digits
  // exponent
  //     ""
  //     'E' sign digits
  //     'e' sign digits
  // sign
  //     ""
  //     '+'
  //     '-'
  function pNumber(): Scientific {
    function pDigits(): string {
      let digits = "";

      while (peek().match(/^[0-9]$/)) {
        digits += peek();
        consume();
      }
      return digits;
    }

    function pDigitOneToNine(): string {
      const c = peek();
      if (c.match(/^[1-9]$/)) {
        consume();
        return c;
      } else {
        bad(c);
      }
    }

    function pIntegerSuffix(): string {
      const c: string = peek();
      if (c === "0") {
        consume();
        return c;
      } else {
        return pDigitOneToNine() + pDigits();
      }
    }

    function pInteger(): string {
      const c = peek();
      if (c === "-") {
        consume();
        return c + pIntegerSuffix();
      } else {
        return pIntegerSuffix();
      }
    }

    function pFraction(): string {
      let c: string = peek();
      if (c === ".") {
        c = consume();
        return pDigits();
      } else {
        return "";
      }
    }

    function pExponent() {
      let c: string = peek();
      if (c === "e" || c === "E") {
        c = consume();
        // save the sign
        if (c === "+" || c === "-") {
          consume();
        } else {
          c = "";
        }
        return c + pDigits();
      } else {
        return "";
      }
    }

    let minus = peek();
    if (minus === "-") {
      consume();
    } else {
      minus = "";
    }

    const coefficient = minus + pInteger();
    let fraction = pFraction();
    const exponent = BigInt(pExponent());

    // Remove trailing 0s from `fraction` since
    // e.g. `0.10000` is the same as `0.1`
    fraction = fraction.substring(0, fraction.search(/0*$/));

    const coefficientBigInt = BigInt(coefficient + fraction);
    const exponentBigInt: bigint = BigInt(exponent) - BigInt(fraction.length);

    return new Scientific(coefficientBigInt, exponentBigInt);
  }

  function pChar(c: string): void {
    if (!(c === peek())) {
      bad(c);
    }
    consume();
  }

  function pChars(str: string): void {
    for (const c of str) {
      if (c !== peek()) {
        bad(c);
      }
      consume();
    }
  }

  // value
  //     object
  //     array
  //     string
  //     number
  //     "true"
  //     "false"
  //     "null"
  function pValue(): Value {
    const c: string = peek();

    if (c === "{") {
      return pObject();
    } else if (c === "[") {
      return pArray();
    } else if (c === '"') {
      return pString();
    } else if (c.match(/^[-0-9]$/)) {
      return pNumber();
    } else if (c === "t") {
      pChars("true");
      return true;
    } else if (c === "f") {
      pChars("false");
      return false;
    } else if (c === "n") {
      pChars("null");
      return null;
    } else {
      bad(c);
    }
  }

  // json
  //      element
  //
  const value: Value = pElement();

  // Verify that we've consumed the entire input
  if (!(peek() === "")) {
    bad(peek());
  }

  return value;
}
