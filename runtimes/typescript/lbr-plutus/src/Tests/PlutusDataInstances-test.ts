import * as LbPlutusData from "../LambdaBuffers/Runtime/PlutusLedgerApi/PlutusData.js";
import * as LbAssocMap from "../LambdaBuffers/Runtime/PlutusLedgerApi/AssocMap.js";

import { describe, it } from "node:test";

import * as TestUtils from "./TestUtils.js";
import fc from "fast-check";

export function fcPlutusData(): fc.Arbitrary<LbPlutusData.PlutusData> {
  const { plutusData } = fc.letrec((tie) => ({
    plutusData: fc.oneof(
      { depthSize: "small", withCrossShrink: true },
      tie("Bytes"),
      tie("Integer"),
      tie("Constr"),
      tie("Map"),
      tie("List"),
    ),
    Constr: fc.record({
      name: fc.constant("Constr"),
      fields: fc.tuple(fc.bigInt(), fc.array(tie("plutusData"))),
    }),
    Map: fc.record({
      name: fc.constant("Map"),
      fields: fc.array(fc.tuple(tie("plutusData"), tie("plutusData")))
        .map((arr) => {
          return LbAssocMap.fromListSafe(
            LbPlutusData.eqPlutusData,
            arr as [LbPlutusData.PlutusData, LbPlutusData.PlutusData][],
          );
        }),
    }),
    List: fc.record({
      name: fc.constant("List"),
      fields: fc.array(tie("plutusData")),
    }),
    Bytes: fc.record({
      name: fc.constant("Bytes"),
      fields: fc.uint8Array(),
    }),
    Integer: fc.record({
      name: fc.constant("Integer"),
      fields: fc.bigInt(),
    }),
  }));

  return plutusData as fc.Arbitrary<LbPlutusData.PlutusData>;
}

describe("PlutusData tests", () => {
  /*
    // Some statistics on the generated data
    fc.statistics( fcPlutusData(),
                    (plutusData) => {
                        function vectorAdd(l: [number, number, number, number, number], r: [number, number, number, number, number]) : [number, number, number, number, number]{
                            return l.map( (v,i) => { return v + r[i]!}  ) as [number, number, number, number, number]
                        }
                        const zero : [number, number, number, number, number] = [0,0,0,0,0]

                        // [ number of Constr
                        // , number of Map
                        // , number of List
                        // , number of Bytes
                        // , number of Integer
                        // ]
                        function measure(pData : LbPlutusData.PlutusData) : [number, number, number, number, number]{
                            switch(pData.name) {
                                case 'Constr':
                                    return vectorAdd( [1,0,0,0,0],pData.fields[1].map(measure).reduce(vectorAdd, zero))
                                case 'Map':
                                    return vectorAdd( [0,1,0,0,0] ,
                                                    pData.fields.map(
                                                                (kv : [LbPlutusData.PlutusData, LbPlutusData.PlutusData]) =>
                                                                    { return kv.map(measure).reduce(vectorAdd, zero) } )
                                                          .reduce(vectorAdd, zero)
                                                            )
                                case 'List':
                                    return vectorAdd( [0,0,1,0,0], pData.fields.map(measure).reduce(vectorAdd,zero))
                                case 'Bytes':
                                    return [0,0,0,1,0]
                                case 'Integer':
                                    return [0,0,0,0,1]
                            }
                        }
                        const [constrs, maps, lists, bytes, integers] = measure(plutusData)

                        return `Constr: ${(constrs + '').padStart(3,'0') }`
                             + ` Map: ${(maps + '').padStart(3,'0') }`
                             + ` List: ${(lists + '').padStart(3,'0') }`
                             + ` Bytes: ${(bytes + '').padStart(3,'0') }`
                             + ` Integers: ${(integers + '').padStart(3,'0') }`
                    }
                        ,
                        { numRuns: 100 },
                 );
    */

  describe("Eq PlutusData tests", () => {
    const dict = LbPlutusData.eqPlutusData;
    TestUtils.eqIt(dict, { name: "Constr", fields: [0n, []] }, {
      name: "Constr",
      fields: [0n, []],
    }, true);
    TestUtils.neqIt(dict, { name: "Constr", fields: [0n, []] }, {
      name: "Constr",
      fields: [0n, []],
    }, false);

    TestUtils.eqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      true,
    );
    TestUtils.neqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      false,
    );

    TestUtils.eqIt(dict, { name: "Integer", fields: 1n }, {
      name: "Integer",
      fields: 1n,
    }, true);
    TestUtils.neqIt(dict, { name: "Integer", fields: 1n }, {
      name: "Integer",
      fields: 1n,
    }, false);

    TestUtils.eqIt(dict, { name: "Bytes", fields: Uint8Array.from([0xff]) }, {
      name: "Bytes",
      fields: Uint8Array.from([0xff]),
    }, true);
    TestUtils.neqIt(dict, { name: "Bytes", fields: Uint8Array.from([0xff]) }, {
      name: "Bytes",
      fields: Uint8Array.from([0xff]),
    }, false);

    TestUtils.eqIt(dict, { name: "Bytes", fields: Uint8Array.from([0xff]) }, {
      name: "Integer",
      fields: 1n,
    }, false);
    TestUtils.neqIt(dict, { name: "Bytes", fields: Uint8Array.from([0xff]) }, {
      name: "Integer",
      fields: 1n,
    }, true);

    TestUtils.eqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [1n, [{ name: "Integer", fields: 1n }]] },
      false,
    );
    TestUtils.neqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      { name: "Constr", fields: [1n, [{ name: "Integer", fields: 1n }]] },
      true,
    );

    TestUtils.eqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 0n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      false,
    );
    TestUtils.neqIt(
      dict,
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 0n }]] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      true,
    );

    TestUtils.eqIt(
      dict,
      { name: "List", fields: [{ name: "Integer", fields: 0n }] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      false,
    );
    TestUtils.neqIt(
      dict,
      { name: "List", fields: [{ name: "Integer", fields: 0n }] },
      { name: "Constr", fields: [0n, [{ name: "Integer", fields: 1n }]] },
      true,
    );

    TestUtils.eqIt(dict, { name: "Map", fields: [] }, {
      name: "Map",
      fields: [],
    }, true);
    TestUtils.neqIt(dict, { name: "Map", fields: [] }, {
      name: "Map",
      fields: [],
    }, false);

    TestUtils.eqIt(dict, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, true);
    TestUtils.neqIt(dict, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, false);

    TestUtils.eqIt(dict, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "Integer", fields: -1n }, {
        name: "List",
        fields: [],
      }]],
    }, false);
    TestUtils.neqIt(dict, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    }, {
      name: "Map",
      fields: [[{ name: "Integer", fields: -1n }, {
        name: "List",
        fields: [],
      }]],
    }, true);

    it(`eq is not neq property based tests`, () => {
      fc.assert(
        fc.property(
          fcPlutusData(),
          fcPlutusData(),
          (l, r) => {
            TestUtils.negationTest(dict, l, r);
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("Json PubKeyHash tests", () => {
    TestUtils.toJsonFromJsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Map",
      fields: [[{ name: "Integer", fields: 0n }, { name: "List", fields: [] }]],
    });

    TestUtils.toJsonFromJsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "List",
      fields: [{ name: "Integer", fields: 0n }],
    });

    TestUtils.toJsonFromJsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Integer",
      fields: 1n,
    });

    TestUtils.toJsonFromJsonRoundTripIt(LbPlutusData.jsonPlutusData, {
      name: "Bytes",
      fields: Uint8Array.from([0xff]),
    });

    it(`toJson/fromJson property based tests`, () => {
      fc.assert(
        fc.property(
          fcPlutusData(),
          (data) => {
            TestUtils.toJsonFromJsonRoundTrip(
              LbPlutusData.jsonPlutusData,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });

  describe("IsPlutusData PlutusData tests", () => {
    it(`IsPlutusData property based tests`, () => {
      fc.assert(
        fc.property(
          fcPlutusData(),
          (data) => {
            TestUtils.isPlutusDataRoundTrip(
              LbPlutusData.isPlutusDataPlutusData,
              data,
            );
          },
        ),
        { examples: [] },
      );
    });
  });
});
