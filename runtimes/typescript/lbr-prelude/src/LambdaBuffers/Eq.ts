import * as Symbols from "./Symbols.js";
import * as Prelude from "prelude";

// deno-lint-ignore no-empty-interface
export interface EqInstances {}

// Reexport Eq from Prelude
export type Eq<A> = Prelude.Eq<A>;

// Runtime dictionary for _all_ Eq instances
export const Eq: EqInstances = {} as EqInstances;

export interface EqInstances {
  [Symbols.Bool]: Prelude.Eq<Prelude.Bool>;
}

Eq[Symbols.Bool] = Prelude.eqBool;

export interface EqInstances {
  [Symbols.Integer]: Prelude.Eq<Prelude.Integer>;
}

Eq[Symbols.Integer] = Prelude.eqInteger;

export interface EqInstances {
  [Symbols.Bytes]: Prelude.Eq<Prelude.Bytes>;
}

Eq[Symbols.Bytes] = Prelude.eqBytes;

export interface EqInstances {
  [Symbols.Char]: Prelude.Eq<Prelude.Char>;
}

Eq[Symbols.Char] = Prelude.eqChar;

export interface EqInstances {
  [Symbols.Text]: Prelude.Eq<Prelude.Text>;
}

Eq[Symbols.Text] = Prelude.eqText;

export interface EqInstances {
  [Symbols.Maybe]: <A>(
    dictA: Prelude.Eq<A>,
  ) => Prelude.Eq<Prelude.Maybe<A>>;
}

Eq[Symbols.Maybe] = Prelude.eqMaybe;

export interface EqInstances {
  [Symbols.Either]: <L, R>(
    dictL: Prelude.Eq<L>,
    dictR: Prelude.Eq<R>,
  ) => Prelude.Eq<Prelude.Either<L, R>>;
}

Eq[Symbols.Either] = Prelude.eqEither;

export interface EqInstances {
  [Symbols.Map]: <K, V>(
    dictK: Prelude.Eq<K>,
    dictV: Prelude.Eq<V>,
  ) => Prelude.Eq<Prelude.Map<K, V>>;
}

Eq[Symbols.Map] = Prelude.eqMap;

export interface EqInstances {
  [Symbols.Set]: <K>(dictK: Prelude.Eq<K>) => Prelude.Eq<Prelude.Set<K>>;
}

Eq[Symbols.Set] = Prelude.eqSet;

export interface EqInstances {
  [Symbols.List]: <A>(dict: Prelude.Eq<A>) => Prelude.Eq<Prelude.List<A>>;
}

Eq[Symbols.List] = Prelude.eqList;
