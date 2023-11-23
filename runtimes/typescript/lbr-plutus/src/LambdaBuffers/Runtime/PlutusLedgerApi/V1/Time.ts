import type { Interval } from "./Interval.js";
import * as LbInterval from "./Interval.js";
import type { Integer } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../../Prelude/Instances.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Time.hs

/**
 * {@link POSIXTime} wraps {@link Integer} and measures the number of
 * milliseconds since 1970-01-01T00:00:00Z.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Time.hs#L38-L44}
 */
export type POSIXTime = Integer;

export const eqPOSIXTime = LbPrelude.eqInteger;
export const jsonPOSIXTime = LbPrelude.jsonInteger;
export const toDataPOSIXTime = LbPreludeInstances.toDataInteger;
export const fromDataPOSIXTime = LbPreludeInstances.fromDataInteger;

/**
 * {@link POSIXTimeRange} is an {@link Interval} of {@link POSIXTime}.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Time.hs#L51-L52}
 */
export type POSIXTimeRange = Interval<POSIXTime>;

export const eqPOSIXTimeRange = LbInterval.eqInterval(eqPOSIXTime);
export const jsonPOSIXTimeRange = LbInterval.jsonInterval(jsonPOSIXTime);
export const toDataPOSIXTimeRange = LbInterval.toDataInterval(toDataPOSIXTime);
export const fromDataPOSIXTimeRange = LbInterval.fromDataInterval(
  fromDataPOSIXTime,
);
