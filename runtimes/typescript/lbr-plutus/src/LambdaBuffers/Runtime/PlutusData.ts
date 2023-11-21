import type { Bytes, Eq, Integer, Json, List } from "lbr-prelude";
import { JsonError, Scientific } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import * as LbHex from "./Hex.js";

export class FromDataError extends Error {
  constructor(message: string) {
    super(message);
  }
}

/**
 * `ToData<A>` is a type class to translate to `PlutusData`
 */
export interface ToData<A> {
  readonly toData: (arg: Readonly<A>) => PlutusData;
}

/**
 * `FromData<A>` is a type class to translate from `PlutusData`
 */
export interface FromData<A> {
  readonly fromData: (arg: Readonly<PlutusData>) => A;
}

/**
 * `PlutusData`
 */
export type PlutusData =
  | { name: "Constr"; fields: [Integer, List<PlutusData>] }
  | { name: "Map"; fields: List<[PlutusData, PlutusData]> }
  | { name: "List"; fields: List<PlutusData> }
  | { name: "Bytes"; fields: Bytes }
  | { name: "Integer"; fields: Integer };

/**
 * `Eq` instance for `PlutusData`
 */
export const eqPlutusData: Eq<PlutusData> = {
  eq: (l, r) => {
    if (
      l.name === "Constr" && r.name === "Constr" && l.fields[0] === r.fields[0]
    ) {
      return LbPrelude.eqList(eqPlutusData).eq(l.fields[1], r.fields[1]);
    } else if (l.name === "Map" && r.name === "Map") {
      return LbPrelude.eqList(LbPrelude.eqList(eqPlutusData)).eq(
        l.fields,
        r.fields,
      );
    } else if (l.name === "List" && r.name === "List") {
      return LbPrelude.eqList(eqPlutusData).eq(l.fields, r.fields);
    } else if (l.name === "Bytes" && r.name === "Bytes") {
      return LbPrelude.eqBytes.eq(l.fields, r.fields);
    } else if (l.name === "Integer" && r.name === "Integer") {
      return LbPrelude.eqInteger.eq(l.fields, r.fields);
    } else {
      return false;
    }
  },
  neq: (l, r) => {
    if (
      l.name === "Constr" && r.name === "Constr" && l.fields[0] === r.fields[0]
    ) {
      return LbPrelude.eqList(eqPlutusData).neq(l.fields[1], r.fields[1]);
    } else if (l.name === "Map" && r.name === "Map") {
      return LbPrelude.eqList(LbPrelude.eqList(eqPlutusData)).neq(
        l.fields,
        r.fields,
      );
    } else if (l.name === "List" && r.name === "List") {
      return LbPrelude.eqList(eqPlutusData).neq(l.fields, r.fields);
    } else if (l.name === "Bytes" && r.name === "Bytes") {
      return LbPrelude.eqBytes.neq(l.fields, r.fields);
    } else if (l.name === "Integer" && r.name === "Integer") {
      return LbPrelude.eqInteger.neq(l.fields, r.fields);
    } else {
      return true;
    }
  },
};

export const toDataPlutusData: ToData<PlutusData> = {
  toData: (arg) => arg,
};

export const fromDataPlutusData: FromData<PlutusData> = {
  fromData: (arg) => arg,
};
export const jsonPlutusData: Json<PlutusData> = {
  toJson: (plutusData) => {
    switch (plutusData.name) {
      case "Constr": {
        const fields = LbPrelude.jsonList(jsonPlutusData).toJson(
          plutusData.fields[1],
        );
        return LbPrelude.jsonConstructor(plutusData.name, [{
          index: new Scientific(plutusData.fields[0], 0n),
          fields: fields,
        }]);
      }
      case "Map": {
        const fields = LbPrelude.jsonList(LbPrelude.jsonList(jsonPlutusData))
          .toJson(plutusData.fields);
        return LbPrelude.jsonConstructor(plutusData.name, [fields]);
      }
      case "List": {
        const fields = LbPrelude.jsonList(jsonPlutusData).toJson(
          plutusData.fields,
        );
        return LbPrelude.jsonConstructor(plutusData.name, [fields]);
      }
      case "Bytes": {
        return LbPrelude.jsonConstructor(plutusData.name, [
          LbHex.bytesToHex(plutusData.fields),
        ]);
      }
      case "Integer": {
        return LbPrelude.jsonConstructor(plutusData.name, [
          LbPrelude.jsonInteger.toJson(plutusData.fields),
        ]);
      }
    }
  },
  fromJson: (value) => {
    return LbPrelude.caseJsonConstructor<PlutusData>("PlutusData.PlutusData", {
      "Constr": (ctorFields) => {
        if (ctorFields.length === 1) {
          const indexAndFields = ctorFields[0]!;

          if (!LbPrelude.isJsonObject(indexAndFields)) {
            throw new JsonError(
              "Expected JSON object but got" +
                LbPrelude.stringify(value),
            );
          }

          const indexValue = indexAndFields["index"];
          if (indexValue === undefined) {
            throw new JsonError(
              "Expected index field but got" +
                LbPrelude.stringify(value),
            );
          }

          const index = LbPrelude.jsonInteger.fromJson(indexValue);

          const fieldsValue = indexAndFields["fields"];

          if (fieldsValue === undefined) {
            throw new JsonError(
              "Expected fields field but got" +
                LbPrelude.stringify(value),
            );
          }

          const fields = LbPrelude.jsonList(jsonPlutusData).fromJson(
            fieldsValue,
          );

          return {
            name: "Constr",
            fields: [index, fields],
          };
        } else {
          throw new JsonError(
            "Expected JSON Array with 1 field but got" +
              LbPrelude.stringify(value),
          );
        }
      },
      "Map": (ctorFields) => {
        if (ctorFields.length === 1) {
          const elemsValue = ctorFields[0]!;
          return {
            name: "Map",
            fields: LbPrelude.caseJsonArray("Map", (kv) => {
              if (!(LbPrelude.isJsonArray(kv) && kv.length == 2)) {
                throw new JsonError(
                  "Expected JSON Array with 2 elements but got" +
                    LbPrelude.stringify(kv),
                );
              }
              return LbPrelude.caseJsonArray(
                "KeyValue",
                jsonPlutusData.fromJson,
                kv,
              ) as [PlutusData, PlutusData];
            }, elemsValue),
          };
        } else {
          throw new JsonError(
            "Expected JSON Array with 1 element but got" +
              LbPrelude.stringify(value),
          );
        }
      },
      "List": (ctorFields) => {
        if (ctorFields.length === 1) {
          const listValue = ctorFields[0]!;
          return {
            name: "List",
            fields: LbPrelude.caseJsonArray(
              "List",
              jsonPlutusData.fromJson,
              listValue,
            ),
          };
        } else {
          throw new JsonError(
            "Expected JSON Array with 1 element but got" +
              LbPrelude.stringify(value),
          );
        }
      },
      "Bytes": (ctorFields) => {
        if (ctorFields.length === 1) {
          const bytesValue = ctorFields[0]!;
          if (LbPrelude.isJsonString(bytesValue)) {
            return { name: "Bytes", fields: LbHex.bytesFromHex(bytesValue) };
          } else {
            throw new JsonError("JSON Value is not a string");
          }
        } else {
          throw new JsonError(
            "Expected JSON Array with 1 element but got" +
              LbPrelude.stringify(value),
          );
        }
      },
      "Integer": (ctorFields) => {
        if (ctorFields.length === 1) {
          const integerValue = ctorFields[0]!;
          return {
            name: "Integer",
            fields: LbPrelude.jsonInteger.fromJson(integerValue),
          };
        } else {
          throw new JsonError(
            "Expected JSON Array with 1 element but got" +
              LbPrelude.stringify(value),
          );
        }
      },
    }, value);
  },
};
