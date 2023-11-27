import * as LbHex from "../LambdaBuffers/Runtime/Hex.js";

import { describe, it } from "node:test";
import * as assert from "node:assert/strict";
import fc from "fast-check";

/**
 * Random generator for hex strings
 */
export function fcHexString(): fc.Arbitrary<string> {
  return fc.uint8Array().map((t) => {
    return Buffer.from(t).toString("hex");
  });
}

/**
 * Random generator for Uint8Arrays
 */
export function fcBytes(): fc.Arbitrary<Uint8Array> {
  return fc.uint8Array();
}

describe("Hex tests", () => {
  function bytesIsHexIt(bytes: Uint8Array, expectedHex: string) {
    const hexes = LbHex.bytesToHex(bytes);
    it(`Encoding bytes ${bytes} to hex ${expectedHex}`, () => {
      assert.deepStrictEqual(hexes, expectedHex);
    });
  }

  function hexIsBytesIt(hex: string, expectedBytes: Uint8Array) {
    const bytes = LbHex.bytesFromHex(hex);
    it(`Decoding hex ${hex} to bytes ${expectedBytes}`, () => {
      assert.deepStrictEqual(bytes, expectedBytes);
    });
  }

  function hexRoundTripIt(hex: string) {
    const hexRoundTrip = LbHex.bytesToHex(LbHex.bytesFromHex(hex));
    it(`Roundtrip for hex ${hex}`, () => {
      assert.deepStrictEqual(hexRoundTrip, hex);
    });
  }

  function bytesRoundTripIt(bytes: Uint8Array) {
    const bytesRoundTrip = LbHex.bytesFromHex(LbHex.bytesToHex(bytes));
    it(`Roundtrip for bytes ${bytes}`, () => {
      assert.deepStrictEqual(bytesRoundTrip, bytes);
    });
  }

  bytesIsHexIt(Uint8Array.from([0xff, 0x11]), "ff11");
  bytesIsHexIt(Uint8Array.from([0xff]), "ff");
  bytesIsHexIt(Uint8Array.from([]), "");
  bytesIsHexIt(Uint8Array.from([0xff, 0x10, 0x01]), "ff1001");
  bytesIsHexIt(Uint8Array.from([0x00, 0x00, 0x00]), "000000");
  bytesIsHexIt(Uint8Array.from([0x01, 0x00, 0x01]), "010001");

  hexIsBytesIt("00", Uint8Array.from([0x00]));
  hexIsBytesIt("", Uint8Array.from([]));
  hexIsBytesIt("ff", Uint8Array.from([0xff]));
  hexIsBytesIt("01", Uint8Array.from([0x01]));
  hexIsBytesIt("02", Uint8Array.from([0x02]));
  hexIsBytesIt("03", Uint8Array.from([0x03]));
  hexIsBytesIt("04", Uint8Array.from([0x04]));
  hexIsBytesIt("05", Uint8Array.from([0x05]));
  hexIsBytesIt("06", Uint8Array.from([0x06]));
  hexIsBytesIt("07", Uint8Array.from([0x07]));
  hexIsBytesIt("08", Uint8Array.from([0x08]));
  hexIsBytesIt("09", Uint8Array.from([0x09]));
  hexIsBytesIt("0a", Uint8Array.from([0x0a]));
  hexIsBytesIt("0b", Uint8Array.from([0x0b]));
  hexIsBytesIt("0c", Uint8Array.from([0x0c]));
  hexIsBytesIt("0d", Uint8Array.from([0x0d]));
  hexIsBytesIt("0f", Uint8Array.from([0x0f]));
  hexIsBytesIt("01ff", Uint8Array.from([0x01, 0xff]));
  hexIsBytesIt("ffaaff00", Uint8Array.from([0xff, 0xaa, 0xff, 0x00]));
  hexIsBytesIt("FFAAFF00", Uint8Array.from([0xff, 0xaa, 0xff, 0x00]));
  hexIsBytesIt("ffAAfF00", Uint8Array.from([0xff, 0xaa, 0xff, 0x00]));
  hexIsBytesIt("ffAAfF00", Uint8Array.from([0xff, 0xaa, 0xff, 0x00]));

  hexRoundTripIt("ffaabbaacc011245");
  hexRoundTripIt("000000ffaabbaacc011245");
  hexRoundTripIt("000000ffaabbaacc011245000000");
  hexRoundTripIt("");
  hexRoundTripIt("abcdef");
  hexRoundTripIt("01234567890a");

  bytesRoundTripIt(Uint8Array.from([]));
  bytesRoundTripIt(Uint8Array.from([0x00]));
  bytesRoundTripIt(Uint8Array.from([0x01]));
  bytesRoundTripIt(Uint8Array.from([0x01, 0xff]));
  bytesRoundTripIt(Uint8Array.from([0x01, 0xf0]));
  bytesRoundTripIt(Uint8Array.from([0x01, 0xf0]));
  bytesRoundTripIt(Uint8Array.from([0x00, 0xf0]));
  bytesRoundTripIt(Uint8Array.from([0x00, 0xf0, 0x00]));
  bytesRoundTripIt(Uint8Array.from([0x00, 0xf0, 0x00, 0x00]));
  bytesRoundTripIt(Uint8Array.from([0x00, 0x00, 0x00, 0x00]));
  bytesRoundTripIt(Uint8Array.from([0xf0, 0x00, 0x00, 0xf0]));

  it(`bytesToHex/bytesFromHex property based roundtrips`, () => {
    fc.assert(
      fc.property(
        fcBytes(),
        (bytes) => {
          assert.deepStrictEqual(
            LbHex.bytesFromHex(LbHex.bytesToHex(bytes)),
            bytes,
          );
        },
      ),
    );
  });

  it(`bytesFromHex/bytesToHex property based roundtrips`, () => {
    fc.assert(
      fc.property(
        fcHexString(),
        (hexstr) => {
          assert.deepStrictEqual(
            LbHex.bytesToHex(LbHex.bytesFromHex(hexstr)),
            hexstr,
          );
        },
      ),
    );
  });
});
