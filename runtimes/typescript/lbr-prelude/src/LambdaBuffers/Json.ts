import * as Symbols from "./Symbols.js";
import * as Prelude from "prelude";

// deno-lint-ignore no-empty-interface
export interface JsonInstances {}
export const Json: JsonInstances = {} as JsonInstances;

export interface JsonInstances {
  [Symbols.Bool]: Prelude.Json<Prelude.Bool>;
}

Json[Symbols.Bool] = Prelude.jsonBool;

export interface JsonInstances {
  [Symbols.Integer]: Prelude.Json<Prelude.Integer>;
}

Json[Symbols.Integer] = Prelude.jsonInteger;

export interface JsonInstances {
  [Symbols.Bytes]: Prelude.Json<Prelude.Bytes>;
}

Json[Symbols.Bytes] = Prelude.jsonBytes;

export interface JsonInstances {
  [Symbols.Char]: Prelude.Json<Prelude.Char>;
}

Json[Symbols.Char] = Prelude.jsonChar;

export interface JsonInstances {
  [Symbols.Text]: Prelude.Json<Prelude.Text>;
}

Json[Symbols.Text] = Prelude.jsonText;

export interface JsonInstances {
  [Symbols.Maybe]: <A>(
    dictA: Prelude.Json<A>,
  ) => Prelude.Json<Prelude.Maybe<A>>;
}

Json[Symbols.Maybe] = Prelude.jsonMaybe;

export interface JsonInstances {
  [Symbols.Either]: <K, V>(
    dictK: Prelude.Json<K>,
    dictV: Prelude.Json<V>,
  ) => Prelude.Json<Prelude.Either<K, V>>;
}

Json[Symbols.Either] = Prelude.jsonEither;

export interface JsonInstances {
  [Symbols.Map]: <K, V>(
    dictOrd: Prelude.Ord<K>,
    dictK: Prelude.Json<K>,
    dictV: Prelude.Json<V>,
  ) => Prelude.Json<Prelude.Map<K, V>>;
}

Json[Symbols.Map] = Prelude.jsonMap;

export interface JsonInstances {
  [Symbols.Set]: <K>(
    dictOrd: Prelude.Ord<K>,
    dictK: Prelude.Json<K>,
  ) => Prelude.Json<Prelude.Set<K>>;
}

Json[Symbols.Set] = Prelude.jsonSet;

export interface JsonInstances {
  [Symbols.List]: <A>(dict: Prelude.Json<A>) => Prelude.Json<Prelude.List<A>>;
}

Json[Symbols.List] = Prelude.jsonList;
