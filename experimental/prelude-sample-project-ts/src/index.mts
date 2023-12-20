import * as Prelude from  "prelude"
import * as PreludeJson from  "prelude/Json.js"
import * as LbrPrelude from  "lbr-prelude"
import * as MySchema from  "myschema-lb"

let a : MySchema.Branchy<Prelude.Bool> = { name: 'Node', fields :[420n, [ {name: 'Leaf', fields: true }, {name: 'Leaf', fields: false } ] ] }
let b : MySchema.Branchy<Prelude.Bool> = { name: 'Leaf', fields : false}

a

console.log("a == a")
console.log(LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPrelude.Bool]).eq(a,a) )
console.log("a == b")
console.log(LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPrelude.Bool]).eq(a,b) )

console.log("a != a")
console.log(LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPrelude.Bool]).neq(a,a) )
console.log("a != b")
console.log(LbrPrelude.Eq[MySchema.Branchy](LbrPrelude.Eq[LbrPrelude.Bool]).neq(a,b) )

console.log("toJson(a)")
console.log(
    PreludeJson.stringify(
        LbrPrelude.Json[MySchema.Branchy](LbrPrelude.Json[LbrPrelude.Bool])
            .toJson(a) 
        )
)

console.log("fromJson(toJson(a))")
console.log(
    JSON.stringify(
        LbrPrelude.Json[MySchema.Branchy](LbrPrelude.Json[LbrPrelude.Bool]).fromJson(
            LbrPrelude.Json[MySchema.Branchy](LbrPrelude.Json[LbrPrelude.Bool])
                .toJson(a) 
            )
        , (_key,value) => { if (typeof value === 'bigint') return (value.toString()); else return value }
    )
)
