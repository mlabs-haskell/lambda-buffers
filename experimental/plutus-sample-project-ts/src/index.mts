import * as LbrPlutusV1 from  "lbr-plutus/V1.js"
import * as LbrPlutusPd from  "lbr-plutus/PlutusData.js"
import * as MySchema from  "myschema-lb/LambdaBuffers/MySchema.mjs"

(BigInt.prototype as any).toJSON = function () {
  return this.toString();
};


const pubKeyHash : LbrPlutusV1.PubKeyHash = new Uint8Array([0,0]) as LbrPlutusV1.PubKeyHash;

const cred : LbrPlutusV1.Credential =
        { name: "PubKeyCredential"
        , fields: pubKeyHash }

console.log(pubKeyHash)
console.log(cred)

let leaf : MySchema.Branchy<LbrPlutusV1.Credential> = { name: 'Leaf', fields : cred }
let leafpd : LbrPlutusV1.PlutusData = LbrPlutusPd.IsPlutusData[MySchema.Branchy](LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential]).toData(leaf)

console.log(leafpd);
console.log(JSON.stringify(leafpd));

console.log("fromData:");
console.log(LbrPlutusPd.IsPlutusData[MySchema.Branchy](LbrPlutusPd.IsPlutusData[LbrPlutusV1.Credential]).fromData(leafpd));

