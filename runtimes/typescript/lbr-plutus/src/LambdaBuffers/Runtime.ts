import * as PlutusLedgerApiPlutusData from "plutus-ledger-api/PlutusData.js";
import * as Prelude from "prelude";

export function bindParse<A, B>(a: A, f: (arg: A) => B): B {
  return f(a);
}

export function failParse<A>(err: string): A {
  throw new PlutusLedgerApiPlutusData.IsPlutusDataError(err);
}

export function succeedParse<A>(a: A): A {
  return a;
}

export function integerData(
  a: Prelude.Integer,
): PlutusLedgerApiPlutusData.PlutusData {
  return { name: "Integer", fields: a };
}

export function constrData(
  tag: Prelude.Integer,
  fields: Prelude.List<PlutusLedgerApiPlutusData.PlutusData>,
): PlutusLedgerApiPlutusData.PlutusData {
  return { name: "Constr", fields: [tag, fields] };
}

export function listData(
  fields: Prelude.List<PlutusLedgerApiPlutusData.PlutusData>,
): PlutusLedgerApiPlutusData.PlutusData {
  return { name: "List", fields: fields };
}

export function casePlutusData<A>(
  ctorCase: (
    tag: Prelude.Integer,
    fields: Prelude.List<PlutusLedgerApiPlutusData.PlutusData>,
  ) => A,
  listCase: (list: Prelude.List<PlutusLedgerApiPlutusData.PlutusData>) => A,
  intCase: (int: Prelude.Integer) => A,
  otherCase: (pd: PlutusLedgerApiPlutusData.PlutusData) => A,
  pd: PlutusLedgerApiPlutusData.PlutusData,
): A {
  switch (pd.name) {
    case "Constr":
      return ctorCase(pd.fields[0], pd.fields[1]);
    case "Integer":
      return intCase(pd.fields);
    case "List":
      return listCase(pd.fields);
    default:
      return otherCase(pd);
  }
}
