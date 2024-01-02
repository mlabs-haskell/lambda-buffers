import * as LbrPrelude from "lbr-prelude";
import * as Prelude from "prelude";
import * as PlutusLedgerApiPlutusData from "plutus-ledger-api/PlutusData.js";
import * as PlutusLedgerApiPreludeInstances from "plutus-ledger-api/Prelude/Instances.js";

// deno-lint-ignore no-empty-interface
export interface IsPlutusDataInstances {}

export const IsPlutusData: IsPlutusDataInstances = {} as IsPlutusDataInstances;

// Bool
export interface IsPlutusDataInstances {
  [LbrPrelude.Bool]: PlutusLedgerApiPlutusData.IsPlutusData<Prelude.Bool>;
}

IsPlutusData[LbrPrelude.Bool] =
  PlutusLedgerApiPreludeInstances.isPlutusDataBool;

// Integer
export interface IsPlutusDataInstances {
  [LbrPrelude.Integer]: PlutusLedgerApiPlutusData.IsPlutusData<Prelude.Integer>;
}

IsPlutusData[LbrPrelude.Integer] =
  PlutusLedgerApiPreludeInstances.isPlutusDataInteger;

// Maybe
export interface IsPlutusDataInstances {
  [LbrPrelude.Maybe]: <A>(
    dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
  ) => PlutusLedgerApiPlutusData.IsPlutusData<Prelude.Maybe<A>>;
}

IsPlutusData[LbrPrelude.Maybe] =
  PlutusLedgerApiPreludeInstances.isPlutusDataMaybe;

// Either
export interface IsPlutusDataInstances {
  [LbrPrelude.Either]: <A, B>(
    dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
    dictB: PlutusLedgerApiPlutusData.IsPlutusData<B>,
  ) => PlutusLedgerApiPlutusData.IsPlutusData<Prelude.Either<A, B>>;
}

IsPlutusData[LbrPrelude.Either] =
  PlutusLedgerApiPreludeInstances.isPlutusDataEither;

// List
export interface IsPlutusDataInstances {
  [LbrPrelude.List]: <A>(
    dictA: PlutusLedgerApiPlutusData.IsPlutusData<A>,
  ) => PlutusLedgerApiPlutusData.IsPlutusData<Prelude.List<A>>;
}
