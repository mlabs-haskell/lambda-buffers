export * from "./Runtime/Prelude.js";
export * from "./Runtime/Maybe.js";
export * from "./Runtime/Either.js";
export * from "./Runtime/Json.js";
export * from "./Runtime/Ord.js";
export * from "./Runtime/Eq.js";

export function and(l: boolean, r: boolean) {
  return l && r;
}
