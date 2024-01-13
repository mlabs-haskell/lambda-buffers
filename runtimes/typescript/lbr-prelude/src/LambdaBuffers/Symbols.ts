import type * as Prelude from "prelude";

// Hacking around https://github.com/microsoft/TypeScript/issues/42633
export type Bool = Prelude.Bool;
export type Bytes = Prelude.Bytes;
export type Char = Prelude.Char;
export type Either<L, R> = Prelude.Either<L, R>;
export type Integer = Prelude.Integer;
export type List<A> = Prelude.List<A>;
export type Map<K, V> = Prelude.Map<K, V>;
export type Maybe<A> = Prelude.Maybe<A>;
export type Set<A> = Prelude.Set<A>;
export type Text = Prelude.Text;

export const Bool: unique symbol = Symbol("Bool");
export const Integer: unique symbol = Symbol("Integer");
export const Bytes: unique symbol = Symbol("Bytes");
export const Char: unique symbol = Symbol("Char");
export const Text: unique symbol = Symbol("Text");
export const Maybe: unique symbol = Symbol("Maybe");
export const Either: unique symbol = Symbol("Either");
export const Map: unique symbol = Symbol("Map");
export const Set: unique symbol = Symbol("Set");
export const List: unique symbol = Symbol("List");
