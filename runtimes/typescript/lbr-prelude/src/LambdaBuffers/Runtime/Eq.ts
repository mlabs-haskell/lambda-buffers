/**
 * `Eq` is a typeclass for "deep" i.e., all substructures recursively are equal by
 * value.
 */
export type Eq<A> = {
  eq: (l: A, r: A) => boolean;
  neq: (l: A, r: A) => boolean;
};
