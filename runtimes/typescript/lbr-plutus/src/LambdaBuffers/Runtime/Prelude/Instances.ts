import { FromDataError } from "../PlutusData.js";
import type { FromData, ToData } from "../PlutusData.js";

import type { Bool, Either, Integer, List, Maybe, Pair } from "lbr-prelude";

/**
 * {@link ToData} instance for {@link Bool}
 */
export const toDataBool: ToData<Bool> = {
  toData: (arg) => {
    if (arg) {
      return { name: "Constr", fields: [1n, []] };
    } else {
      return { name: "Constr", fields: [0n, []] };
    }
  },
};

/**
 * {@link FromData} instance for {@link Bool}
 */
export const fromDataBool: FromData<Bool> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr": {
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 0) {
          return false;
        } else if (
          plutusData.fields[0] === 1n && plutusData.fields[1].length === 0
        ) {
          return true;
        } else {
          throw new FromDataError("Malformed Constr but got " + plutusData);
        }
      }
      default:
        throw new FromDataError("Expected Constr but got " + plutusData);
    }
  },
};

/**
 * {@link ToData} instance for {@link Integer}
 */
export const toDataInteger: ToData<Integer> = {
  toData: (arg) => {
    return { name: "Integer", fields: arg };
  },
};

/**
 * {@link FromData} instance for {@link Integer}
 */
export const fromDataInteger: FromData<Integer> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Integer":
        return plutusData.fields;
      default:
        throw new FromDataError("Expected Integer but got " + plutusData);
    }
  },
};

/**
 * {@link ToData} instance for {@link Maybe}
 */
export function toDataMaybe<A>(dict: ToData<A>): ToData<Maybe<A>> {
  return {
    toData: (arg) => {
      if (arg === undefined) {
        return { name: "Constr", fields: [1n, []] };
      } else {
        return { name: "Constr", fields: [0n, [dict.toData(arg)]] };
      }
    },
  };
}

/**
 * {@link FromData} instance for {@link Maybe}
 */
export function fromDataMaybe<A>(dict: FromData<A>): FromData<Maybe<A>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr":
          if (plutusData.fields[0] == 1n) {
            return undefined;
          } else if (plutusData.fields[0] == 0n) {
            if (plutusData.fields[1].length !== 1) {
              throw new FromDataError("Malformed Constr" + plutusData);
            }
            return dict.fromData(plutusData.fields[1][0]!);
          } else {
            throw new FromDataError("Malformed Constr" + plutusData);
          }
        default:
          throw new FromDataError("Expected Constr but got " + plutusData);
      }
    },
  };
}

/**
 * {@link ToData} instance for {@link List}
 */
export function toDataList<A>(dict: ToData<A>): ToData<List<A>> {
  return {
    toData: (arg) => {
      return { name: "List", fields: arg.map(dict.toData) };
    },
  };
}

/**
 * {@link FromData} instance for {@link List}
 */
export function fromDataList<A>(dict: FromData<A>): FromData<List<A>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "List":
          return plutusData.fields.map(dict.fromData);
        default:
          throw new FromDataError("Expected List but got " + plutusData);
      }
    },
  };
}

/**
 * {@link ToData} instance for {@link Either}
 */
export function toDataEither<A, B>(
  dictA: ToData<A>,
  dictB: ToData<B>,
): ToData<Either<A, B>> {
  return {
    toData: (arg) => {
      switch (arg.name) {
        case "Left":
          return { name: "Constr", fields: [0n, [dictA.toData(arg.fields)]] };
        case "Right":
          return { name: "Constr", fields: [1n, [dictB.toData(arg.fields)]] };
      }
    },
  };
}

/**
 * {@link FromData} instance for {@link Either}
 */
export function fromDataEither<A, B>(
  dictA: FromData<A>,
  dictB: FromData<B>,
): FromData<Either<A, B>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr":
          if (
            plutusData.fields[0] === 0n && plutusData.fields[1].length === 1
          ) {
            return {
              name: "Left",
              fields: dictA.fromData(plutusData.fields[1][0]!),
            };
          } else if (
            plutusData.fields[0] === 1n && plutusData.fields[1].length === 1
          ) {
            return {
              name: "Right",
              fields: dictB.fromData(plutusData.fields[1][0]!),
            };
          } else {
            throw new FromDataError("Malformed Constr but got " + plutusData);
          }
        default:
          throw new FromDataError("Expected Constr but got " + plutusData);
      }
    },
  };
}

/**
 * A {@link ToData} instance for {@link Pair} which encodes elements `A` and
 * `B` as `Constr 0 [A,B]` where we note the `0` "tag".
 *
 * @remarks many encodings of opaque types are encoded with the `0` tag
 */
export function toDataPairWithTag<A, B>(
  dictA: ToData<A>,
  dictB: ToData<B>,
): ToData<Pair<A, B>> {
  return {
    toData: (arg) => {
      return {
        name: "Constr",
        fields: [0n, [dictA.toData(arg[0]), dictB.toData(arg[1])]],
      };
    },
  };
}

/**
 * A {@link FromData} instance for {@link Pair} which is the inverse of {@link toDataPairWithoutTag}
 */
export function fromDataPairWithTag<A, B>(
  dictA: FromData<A>,
  dictB: FromData<B>,
): FromData<Pair<A, B>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "Constr":
          if (
            plutusData.fields[0] === 0n && plutusData.fields[1].length === 2
          ) {
            return [
              dictA.fromData(plutusData.fields[1][0]!),
              dictB.fromData(plutusData.fields[1][1]!),
            ];
          } else {
            throw new FromDataError("Malformed Constr but got " + plutusData);
          }
        default:
          throw new FromDataError("Expected Constr but got " + plutusData);
      }
    },
  };
}

/**
 * A {@link ToData} instance for {@link Pair} which encodes elements `A` and
 * `B` as just `[A,B]`.
 */
export function toDataPairWithoutTag<A, B>(
  dictA: ToData<A>,
  dictB: ToData<B>,
): ToData<Pair<A, B>> {
  return {
    toData: (arg) => {
      return {
        name: "List",
        fields: [dictA.toData(arg[0]), dictB.toData(arg[1])],
      };
    },
  };
}

/**
 * A {@link FromData} instance for {@link Pair} which is the inverse of {@link toDataPairWithoutTag}
 */
export function fromDataPairWithoutTag<A, B>(
  dictA: FromData<A>,
  dictB: FromData<B>,
): FromData<Pair<A, B>> {
  return {
    fromData: (plutusData) => {
      switch (plutusData.name) {
        case "List":
          if (plutusData.fields.length === 2) {
            return [
              dictA.fromData(plutusData.fields[0]!),
              dictB.fromData(plutusData.fields[1]!),
            ];
          }
          break;
        default:
          break;
      }
      throw new FromDataError(`Expected List of size 2 but got ${plutusData}`);
    },
  };
}
