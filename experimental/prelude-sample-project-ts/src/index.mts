import * as MySchema from  "myschema-lb"
import * as Prelude from  "lbr-prelude"

let a : MySchema.Branchy<Prelude.Bool> = { name: 'Node', fields :[69n, [ {name: 'Leaf', fields: true } ] ] }
//let b : MySchema.Branchy<Prelude.Bool> = { name: 'Leaf', fields : false}

// console.log(MySchema.eqBranchy(Prelude.eqBool).eq(a,b))
console.log(MySchema.eqBranchy(Prelude.eqBool).eq(a,a))
