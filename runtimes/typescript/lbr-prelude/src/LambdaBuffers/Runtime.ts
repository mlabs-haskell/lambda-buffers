import * as Prelude from "prelude";
export {
  caseJsonArray,
  caseJsonConstructor,
  caseJsonObject,
  jsonArray,
  jsonConstructor,
  jsonField,
  jsonObject,
} from "prelude";

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
