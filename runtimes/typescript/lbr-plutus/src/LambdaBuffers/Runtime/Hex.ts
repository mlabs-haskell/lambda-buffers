/**
 * Type for the error thrown from {@link bytesFromHex}
 */
export class HexError extends Error {
  constructor(msg: string) {
    super(msg);
  }
}

/**
 * Given a hex encoded (case insensitive) string (base 16), return the
 * corresponding bytes.
 *
 * Internally, we copy the algorithm from {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Bytes.hs#L39-L68}
 *
 * @throws
 * {@link HexError} is thrown if there is an odd number of hex digits or there
 * is an invalid hex digit (i.e., a hex digit that is not `[0-9a-fA-F]`)
 */
export function bytesFromHex(hex: string): Uint8Array {
  const iterator = hex[Symbol.iterator]();
  let theChar = iterator.next();

  // `hexIterator` iterates through two codepoints of `hex` at a time,
  // returning the corresponding octet from the 2 codepoints.
  const hexIterator: IterableIterator<number> = {
    [Symbol.iterator]() {
      return this;
    },

    next(): IteratorResult<number> {
      if (theChar.done) {
        return { done: true, value: undefined };
      }
      const c0 = theChar.value!;
      theChar = iterator.next();
      if (theChar.done) {
        //  we have an odd number of hex digits, so should error
        throw new HexError("Unmatched hex digit");
      }

      const c1 = theChar.value!;

      theChar = iterator.next();

      const hexes = c0 + c1;
      if (!hexes.match(/^[0-9a-f]{2}$/i)) {
        throw new HexError(`Invalid hex digit: ${hexes}`);
      }

      const decimal = parseInt(hexes, 16);

      return { value: decimal, done: false };
    },
  };

  return Uint8Array.from(hexIterator);
}

/**
 * Hex encodes the given byte array. This is the inverse of {@link
 * bytesFromHex}.
 */
export function bytesToHex(bytes: Uint8Array): string {
  const result: string[] = [];
  for (let i = 0; i < bytes.length; ++i) {
    result.push(bytes[i]!.toString(16).padStart(2, "0"));
  }

  return result.join("");
}
