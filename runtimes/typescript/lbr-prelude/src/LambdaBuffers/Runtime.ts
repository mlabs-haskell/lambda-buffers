import * as Prelude from "prelude";
export { jsonArray, jsonConstructor, jsonField, jsonObject } from "prelude";

export function and(l: boolean, r: boolean) {
  return l && r;
}

export function succeedParse<A>(a: A): A {
  return a;
}

export function failParse<A>(err: string): A {
  throw new Prelude.JsonError(err);
}

export function bindParse<A, B>(a: A, f: (arg: A) => B): B {
  return f(a);
}

export function caseJsonConstructor<A>(
  title: string,
  fromJsonFields: [[string, (fields: Prelude.Value[]) => A]],
): (value: Readonly<Prelude.Value>) => A {
  return (value) => {
    return Prelude.caseJsonConstructor(
      title,
      Object.fromEntries(fromJsonFields),
      value,
    );
  };
}

export function caseJsonArray<A>(
  title: string,
  parseElem: (arg: Prelude.Value) => A,
): (value: Readonly<Prelude.Value>) => A[] {
  return (value) => {
    return Prelude.caseJsonArray(title, parseElem, value);
  };
}

export function caseJsonObject<A>(
  f: (obj: { [index: string]: Prelude.Value }) => A,
): (value: Prelude.Value) => A {
  return (value) => {
    return Prelude.caseJsonObject(f, value);
  };
}
