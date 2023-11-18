import type { Interval } from "./Interval.js";
import * as LbInterval from "./Interval.js";
import type { Integer } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../../Prelude/Instances.js";

/**
 * https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Time.hs
 */

// -- PlutusLedgerApi.V1.Time
// opaque POSIXTime
//
// instance PlutusData POSIXTime
// instance Eq POSIXTime
// instance Json POSIXTime

export type POSIXTime = Integer;

export const eqPOSIXTime = LbPrelude.eqInteger;
export const jsonPOSIXTime = LbPrelude.jsonInteger;
export const toDataPOSIXTime = LbPreludeInstances.toDataInteger;
export const fromDataPOSIXTime = LbPreludeInstances.fromDataInteger;

// opaque POSIXTimeRange
//
// instance PlutusData POSIXTimeRange
// instance Eq POSIXTimeRange
// instance Json POSIXTimeRange

export type POSIXTimeRange = Interval<POSIXTime>;
export const eqPOSIXTimeRange = LbInterval.eqInterval(eqPOSIXTime);
export const jsonPOSIXTimeRange = LbInterval.jsonInterval(jsonPOSIXTime);
export const toDataPOSIXTimeRange = LbInterval.toDataInterval(toDataPOSIXTime);
export const fromDataPOSIXTimeRange = LbInterval.fromDataInterval(
  fromDataPOSIXTime,
);
