export class HexError extends Error {
  constructor(msg: string) {
    super(msg);
  }
}
export function bytesToHex(bytes: Uint8Array): string {
  const result: string[] = [];
  for (let i = 0; i < bytes.length; ++i) {
    result.push(bytes[i]!.toString(16).padStart(2, "0"));
  }

  return result.join("");
}

/**
 * @privateremarks
 * We copy the algorithm here: {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Bytes.hs#L39-L68}
 */
export function bytesFromHex(hex: string): Uint8Array {
  const iterator = hex[Symbol.iterator]();
  let theChar = iterator.next();

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
        throw new HexError("Invalid hex digit");
      }

      const decimal = parseInt(hexes, 16);

      return { value: decimal, done: false };
    },
  };

  return Uint8Array.from(hexIterator);
}
