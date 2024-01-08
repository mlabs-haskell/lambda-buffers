import type { Eq } from "./Eq.js";

type Ordering =
  | "LT"
  | "GT"
  | "EQ";

export interface Ord<A> extends Eq<A> {
  readonly compare: (l: Readonly<A>, r: Readonly<A>) => Ordering;
}
