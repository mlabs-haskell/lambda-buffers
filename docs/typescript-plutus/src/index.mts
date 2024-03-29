// Simple plutus sample project.
// To run, type
// ```
// npm run build && node ./dist/src/index.mjs
// ```
import * as LbrPlutusV1 from "lbr-plutus/V1.js";
import * as LbrPlutusPd from "lbr-plutus/PlutusData.js";
import * as LbrPrelude from "lbr-prelude";
import * as Prelude from "prelude";
import * as MySchema from "lbf-plutus-sample-project/LambdaBuffers/MySchema.mjs";

const pubKeyHash: LbrPlutusV1.PubKeyHash = new Uint8Array([
  0,
  0,
]) as LbrPlutusV1.PubKeyHash;

const cred: LbrPlutusV1.Credential = {
  name: "PubKeyCredential",
  fields: pubKeyHash,
};

console.log(`Some samples of PubKeyHash and Credential`);
console.log(pubKeyHash);
console.log(cred);

// Printing the leafy type
console.log(`The Leaf constructor`);
const leaf: MySchema.Branchy<LbrPlutusV1.Credential> = {
  name: "Leaf",
  fields: cred,
};
const leafpd: LbrPlutusV1.PlutusData = LbrPlutusPd.IsPlutusData
  [MySchema.Branchy](
    LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential],
  ).toData(leaf);

console.log(leafpd);
console.log(
  `Prelude.Json: ${
    Prelude.stringify(
      LbrPrelude.Json[MySchema.Branchy](LbrPrelude.Json[LbrPlutusV1.Credential])
        .toJson(leaf),
    )
  }`,
);
console.log(
  `Prelude.Json: ${
    Prelude.stringify(LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(leafpd))
  }`,
);

console.log("fromData:");
console.log(
  LbrPlutusPd.IsPlutusData[MySchema.Branchy](
    LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential],
  ).fromData(leafpd),
);

// Printing the branchy
console.log(`The Node constructor`);
const branchy: MySchema.Branchy<LbrPlutusV1.Credential> = {
  name: "Node",
  fields: [leafpd, { name: "Finite", fields: leaf }],
};
const branchypd: LbrPlutusV1.PlutusData = LbrPlutusPd.IsPlutusData
  [MySchema.Branchy](LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential]).toData(
    branchy,
  );

console.log("toData:");
console.log(branchypd);

console.log(
  `Prelude.Json: ${
    Prelude.stringify(
      LbrPrelude.Json[MySchema.Branchy](LbrPrelude.Json[LbrPlutusV1.Credential])
        .toJson(branchy),
    )
  }`,
);
console.log(
  `Prelude.Json: ${
    Prelude.stringify(LbrPrelude.Json[LbrPlutusV1.PlutusData].toJson(branchypd))
  }`,
);

console.log("fromData:");
console.log(
  LbrPlutusPd.IsPlutusData[MySchema.Branchy](
    LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential],
  ).fromData(branchypd),
);

// Printing the Eq
console.log(
  `branchy is branchy: ${
    LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPlutusV1.Credential]).eq(
      branchy,
      branchy,
    )
  }`,
);
console.log(
  `branchy is not branchy: ${
    LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPlutusV1.Credential]).neq(
      branchy,
      branchy,
    )
  }`,
);
console.log(
  `branchy is leaf: ${
    LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPlutusV1.Credential]).eq(
      branchy,
      leaf,
    )
  }`,
);
console.log(
  `branchy is not leaf: ${
    LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPlutusV1.Credential]).neq(
      branchy,
      leaf,
    )
  }`,
);
