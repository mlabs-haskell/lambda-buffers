import * as MySchema from  "myschema-lb"
import * as Prelude from  "lbr-prelude"

let a : MySchema.MyMaybe<Prelude.Bool> = { name: 'MyNothing'}
let b : MySchema.MyMaybe<Prelude.Bool> = { name: 'MyJust', fields : [false, 0n, false]}

console.log(MySchema.eqMyMaybe(Prelude.eqBool).eq(a,b))
