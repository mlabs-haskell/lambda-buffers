use lbf_prelude::prelude::{Bool, Bytes, Char, Either, Integer, List, Map, Maybe, Set, Text};
use lbf_prelude_golden_api::days::{Day, FreeDay, WorkDay};
use lbf_prelude_golden_api::foo::bar::{FooComplicated, FooProd, FooRec, FooSum, F, G};
use lbf_prelude_golden_api::foo::{FInt, GInt, A, B, C, D};
use num_bigint::BigInt;
use std::collections::{BTreeMap, BTreeSet};

pub fn bi(num: i32) -> Integer {
    BigInt::from(num)
}

pub fn some_bytes() -> Bytes {
    vec![115, 111, 109, 101, 32, 98, 121, 116, 101, 115]
}

pub fn foo_sum_goldens<A: Clone, B: Clone, C>(x: A, y: B, z: C) -> List<FooSum<A, B, C>> {
    vec![
        FooSum::Foo(x.clone(), y.clone(), z),
        FooSum::Bar(x, y.clone()),
        FooSum::Baz(y),
        FooSum::Qax,
        FooSum::Faz(bi(0)),
    ]
}

pub fn a_goldens() -> List<A> {
    foo_sum_goldens(bi(1337), false, some_bytes())
        .into_iter()
        .map(A)
        .collect()
}

pub fn foo_prod_goldens<A, B, C>(x: A, y: B, z: C) -> List<FooProd<A, B, C>> {
    vec![FooProd(x, y, z, bi(1337))]
}

pub fn b_goldens() -> List<B> {
    foo_prod_goldens(bi(1337), false, some_bytes())
        .into_iter()
        .map(B)
        .collect()
}

pub fn foo_rec_goldens<A, B, C>(x: A, y: B, z: C) -> List<FooRec<A, B, C>> {
    vec![FooRec {
        foo_a: x,
        foo_b: y,
        foo_c: z,
        foo_int: bi(1337),
    }]
}

pub fn c_goldens() -> List<C> {
    foo_rec_goldens(bi(1337), false, some_bytes())
        .into_iter()
        .map(C)
        .collect()
}

pub fn d_goldens() -> List<D> {
    let foo_sum = foo_sum_goldens(bi(1337), false, some_bytes());
    let foo_prod = foo_prod_goldens(bi(1337), false, some_bytes());
    let foo_rec = foo_rec_goldens(bi(1337), false, some_bytes());

    foo_sum
        .iter()
        .map(|sum| {
            foo_prod
                .iter()
                .map(|prod| {
                    foo_rec.iter().map(|rec| {
                        D(FooComplicated {
                            sum: sum.clone(),
                            prod: prod.clone(),
                            rec: rec.clone(),
                        })
                    })
                })
                .flatten()
        })
        .flatten()
        .collect()
}

pub fn f_int_goldens() -> List<FInt> {
    vec![FInt(F::Nil), FInt(F::Rec(Box::new(G::Nil)))]
}

pub fn g_int_goldens() -> List<GInt> {
    vec![GInt(G::Nil), GInt(G::Rec(Box::new(F::Nil)))]
}

pub fn day_goldens() -> List<Day> {
    vec![
        Day::Monday,
        Day::Tuesday,
        Day::Wednesday,
        Day::Thursday,
        Day::Friday,
        Day::Saturday,
        Day::Sunday,
    ]
}

pub fn workday_goldens() -> List<WorkDay> {
    vec![
        Day::Monday,
        Day::Tuesday,
        Day::Wednesday,
        Day::Thursday,
        Day::Friday,
    ]
    .into_iter()
    .map(WorkDay)
    .collect()
}

pub fn freeday_goldens() -> List<FreeDay> {
    vec![FreeDay { day: Day::Saturday }, FreeDay { day: Day::Sunday }]
}

pub fn bool_goldens() -> List<Bool> {
    vec![true, false]
}

pub fn integer_goldens() -> List<Integer> {
    vec![
        bi(0),
        bi(1),
        bi(-1),
        bi(2).pow(32),
        bi(-1) * (bi(2).pow(32)),
        bi(2).pow(64),
        bi(-1) * (bi(2).pow(64)),
        bi(2).pow(128),
        bi(-1) * (bi(2).pow(128)),
    ]
}

pub fn bytes_goldens() -> List<Bytes> {
    vec![Vec::new(), vec![0], some_bytes()]
}

pub fn char_goldens() -> List<Char> {
    vec![0x0, 0xA, 0x1F643]
        .into_iter()
        .map(|int| char::from_u32(int).unwrap())
        .collect()
}

pub fn text_goldens() -> List<Text> {
    vec!["", "\n", "dražen popović"]
        .into_iter()
        .map(String::from)
        .collect()
}

pub fn maybe_goldens() -> List<Maybe<Bool>> {
    vec![None, Some(true), Some(false)]
}

pub fn either_goldens() -> List<Either<Bool, Text>> {
    vec![Err(true), Err(false), Ok(String::from("this is right"))]
}

pub fn list_goldens() -> List<List<Bool>> {
    vec![
        Vec::new(),
        vec![true],
        vec![false],
        vec![true, true, false, false],
    ]
}

pub fn set_goldens() -> List<Set<Bool>> {
    vec![
        BTreeSet::new(),
        BTreeSet::from([true]),
        BTreeSet::from([true, false]),
    ]
}

pub fn map_goldens() -> List<Map<Bool, Bool>> {
    vec![
        BTreeMap::new(),
        BTreeMap::from([(true, true)]),
        BTreeMap::from([(true, true), (false, false)]),
    ]
}
