use lbf_plutus_golden_api::days::{Day, FreeDay, WorkDay};
use lbf_plutus_golden_api::foo::bar::{FooComplicated, FooProd, FooRec, FooSum, F, G};
use lbf_plutus_golden_api::foo::{FInt, GInt, A, B, C, D};
use lbf_prelude::prelude::{Bool, Either, Integer, List, Map};
use num_bigint::BigInt;
use plutus_ledger_api::plutus_data::PlutusData;
use plutus_ledger_api::v1::address::{
    Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential, TransactionIndex,
};
use plutus_ledger_api::v1::crypto::{Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash};
use plutus_ledger_api::v1::datum::{Datum, DatumHash};
use plutus_ledger_api::v1::interval::{Extended, LowerBound, PlutusInterval, UpperBound};
use plutus_ledger_api::v1::redeemer::{Redeemer, RedeemerHash};
use plutus_ledger_api::v1::script::{MintingPolicyHash, ScriptHash, ValidatorHash};
use plutus_ledger_api::v1::transaction::{
    POSIXTime, ScriptContext, TransactionHash, TransactionInfo, TransactionInput,
    TransactionOutput, TxInInfo,
};
use plutus_ledger_api::v1::value::{AssetClass, CurrencySymbol, TokenName, Value};
use plutus_ledger_api::v2::assoc_map::AssocMap;
use plutus_ledger_api::v2::datum::OutputDatum;
use plutus_ledger_api::v2::transaction::{
    DCert, ScriptContext as ScriptContextV2, ScriptPurpose, TransactionInfo as TransactionInfoV2,
    TransactionOutput as TransactionOutputV2, TxInInfo as TxInInfoV2,
};
use std::collections::BTreeMap;

pub fn bi(num: i32) -> Integer {
    BigInt::from(num)
}

pub fn plutus_data_goldens() -> List<PlutusData> {
    vec![
        PlutusData::Constr(bi(0), vec![]),
        PlutusData::Constr(
            bi(1),
            vec![
                PlutusData::Integer(bi(1)),
                PlutusData::Bytes("some bytes".as_bytes().to_vec()),
            ],
        ),
        PlutusData::List(Vec::new()),
        PlutusData::List(vec![PlutusData::Integer(bi(1)), PlutusData::Integer(bi(2))]),
        PlutusData::List(vec![
            PlutusData::Integer(bi(1)),
            PlutusData::Bytes("some bytes".as_bytes().to_vec()),
        ]),
        PlutusData::Map(Vec::new()),
        PlutusData::Map(vec![
            (
                PlutusData::Integer(bi(1)),
                PlutusData::Bytes("some bytes".as_bytes().to_vec()),
            ),
            (
                PlutusData::Integer(bi(2)),
                PlutusData::Bytes("some more bytes".as_bytes().to_vec()),
            ),
        ]),
        PlutusData::Integer(bi(0)),
        PlutusData::Integer(bi(1)),
        PlutusData::Integer(bi(-1)),
        PlutusData::Bytes("".as_bytes().to_vec()),
        PlutusData::Bytes("\0".as_bytes().to_vec()),
        PlutusData::Bytes("some bytes".as_bytes().to_vec()),
    ]
}

pub fn blake2b_256_hash() -> LedgerBytes {
    LedgerBytes((1..33).collect())
}

pub fn blake2b_224_hash() -> LedgerBytes {
    LedgerBytes((1..29).collect())
}

pub fn address_goldens() -> List<Address> {
    [
        credential_goldens()
            .into_iter()
            .map(|credential| Address {
                credential,
                staking_credential: None,
            })
            .collect::<Vec<_>>(),
        credential_goldens()
            .iter()
            .flat_map(|credential| {
                staking_credential_goldens()
                    .into_iter()
                    .map(|staking_credential| Address {
                        credential: credential.clone(),
                        staking_credential: Some(staking_credential.clone()),
                    })
            })
            .collect::<Vec<_>>(),
    ]
    .concat()
}

pub fn credential_goldens() -> List<Credential> {
    [
        pubkeyhash_goldens()
            .into_iter()
            .map(Credential::PubKey)
            .collect::<Vec<_>>(),
        validator_hash_goldens()
            .into_iter()
            .map(Credential::Script)
            .collect::<Vec<_>>(),
    ]
    .concat()
}

pub fn staking_credential_goldens() -> List<StakingCredential> {
    [
        credential_goldens()
            .into_iter()
            .map(StakingCredential::Hash)
            .collect::<Vec<_>>(),
        vec![StakingCredential::Pointer(ChainPointer {
            slot_number: Slot(bi(0)),
            transaction_index: TransactionIndex(bi(1)),
            certificate_index: CertificateIndex(bi(2)),
        })],
    ]
    .concat()
}

pub fn pubkeyhash_goldens() -> List<Ed25519PubKeyHash> {
    vec![Ed25519PubKeyHash(blake2b_224_hash())]
}

pub fn script_hash_goldens() -> List<ScriptHash> {
    vec![ScriptHash(blake2b_224_hash())]
}

pub fn validator_hash_goldens() -> List<ValidatorHash> {
    script_hash_goldens()
        .into_iter()
        .map(ValidatorHash)
        .collect()
}

pub fn minting_policy_hash_goldens() -> List<MintingPolicyHash> {
    script_hash_goldens()
        .into_iter()
        .map(MintingPolicyHash)
        .collect()
}

pub fn bytes_goldens() -> List<LedgerBytes> {
    vec![
        LedgerBytes(Vec::new()),
        LedgerBytes(vec![0]),
        LedgerBytes("some bytes".as_bytes().to_vec()),
    ]
}

pub fn interval_goldens() -> List<PlutusInterval<POSIXTime>> {
    lower_bound_goldens()
        .iter()
        .flat_map(|from| {
            upper_bound_goldens().into_iter().map(|to| PlutusInterval {
                from: from.clone(),
                to,
            })
        })
        .collect()
}

pub fn lower_bound_goldens() -> List<LowerBound<POSIXTime>> {
    extended_goldens()
        .iter()
        .flat_map(|bound| {
            closure_goldens().into_iter().map(|closed| LowerBound {
                bound: bound.clone(),
                closed,
            })
        })
        .collect()
}

pub fn upper_bound_goldens() -> List<UpperBound<POSIXTime>> {
    extended_goldens()
        .iter()
        .flat_map(|bound| {
            closure_goldens().into_iter().map(|closed| UpperBound {
                bound: bound.clone(),
                closed,
            })
        })
        .collect()
}

pub fn extended_goldens() -> List<Extended<POSIXTime>> {
    vec![
        Extended::NegInf,
        Extended::PosInf,
        Extended::Finite(POSIXTime(bi(0))),
    ]
}

pub fn closure_goldens() -> List<Bool> {
    vec![true, false]
}

pub fn posix_time_goldens() -> List<POSIXTime> {
    [bi(0), bi(1), bi(2)].into_iter().map(POSIXTime).collect()
}

pub fn posix_time_range_goldens() -> List<PlutusInterval<POSIXTime>> {
    interval_goldens()
}

pub fn currency_symbol_goldens() -> List<CurrencySymbol> {
    minting_policy_hash_goldens()
        .into_iter()
        .map(CurrencySymbol::NativeToken)
        .collect()
}

pub fn ada_currency_symbol_golden() -> CurrencySymbol {
    CurrencySymbol::Ada
}

pub fn token_name_goldens() -> List<TokenName> {
    vec![
        TokenName(LedgerBytes(Vec::new())),
        TokenName(LedgerBytes((1..17).collect())),
        TokenName(LedgerBytes((1..33).collect())),
    ]
}

pub fn asset_class_goldens() -> List<AssetClass> {
    currency_symbol_goldens()
        .iter()
        .flat_map(|currency_symbol| {
            token_name_goldens()
                .into_iter()
                .map(|token_name| AssetClass {
                    currency_symbol: currency_symbol.clone(),
                    token_name,
                })
        })
        .chain([AssetClass {
            currency_symbol: CurrencySymbol::Ada,
            token_name: TokenName::ada(),
        }])
        .collect()
}

pub fn value_goldens() -> List<Value> {
    map_goldens().into_iter().map(Value).collect()
}

pub fn map_goldens() -> List<Map<CurrencySymbol, Map<TokenName, Integer>>> {
    vec![
        BTreeMap::new(),
        BTreeMap::from([(
            CurrencySymbol::Ada,
            BTreeMap::from([(TokenName::ada(), bi(1337))]),
        )]),
        BTreeMap::from([
            (
                CurrencySymbol::Ada,
                BTreeMap::from([(TokenName::ada(), bi(1337))]),
            ),
            (
                CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(blake2b_224_hash()))),
                BTreeMap::from([
                    (TokenName(LedgerBytes(Vec::new())), bi(1337)),
                    (TokenName(LedgerBytes((1..17).collect())), bi(16)),
                    (TokenName(LedgerBytes((1..33).collect())), bi(32)),
                ]),
            ),
        ]),
    ]
}

pub fn redeemer_goldens() -> List<Redeemer> {
    vec![Redeemer(PlutusData::Integer(bi(1337)))]
}

pub fn datum_goldens() -> List<Datum> {
    vec![Datum(PlutusData::Integer(bi(1337)))]
}

pub fn redeemer_hash_goldens() -> List<RedeemerHash> {
    vec![RedeemerHash(blake2b_256_hash())]
}

pub fn datum_hash_goldens() -> List<DatumHash> {
    vec![DatumHash(blake2b_256_hash())]
}

pub fn tx_id_goldens() -> List<TransactionHash> {
    vec![TransactionHash(blake2b_256_hash())]
}

pub fn tx_out_ref_goldens() -> List<TransactionInput> {
    tx_id_goldens()
        .into_iter()
        .map(|transaction_id| TransactionInput {
            transaction_id,
            index: bi(0),
        })
        .collect()
}

pub fn tx_in_info_goldens_v1() -> List<TxInInfo> {
    tx_out_ref_goldens()
        .iter()
        .flat_map(|reference| {
            tx_out_goldens_v1().into_iter().map(|output| TxInInfo {
                reference: reference.clone(),
                output,
            })
        })
        .collect()
}

pub fn tx_out_goldens_v1() -> List<TransactionOutput> {
    address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    [None]
                        .into_iter()
                        .chain(datum_hash_goldens().into_iter().map(Some))
                        .map(|datum_hash| TransactionOutput {
                            address: address.clone(),
                            datum_hash,
                            value: value.clone(),
                        })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn dcert_goldens() -> List<DCert> {
    [
        vec![DCert::Mir],
        vec![DCert::Genesis],
        pubkeyhash_goldens()
            .into_iter()
            .map(|pkh| DCert::PoolRetire(PaymentPubKeyHash(pkh), BigInt::from(1337)))
            .collect(),
        staking_credential_goldens()
            .into_iter()
            .map(|staking_cred| DCert::DelegRegKey(staking_cred))
            .collect(),
        pubkeyhash_goldens()
            .iter()
            .flat_map(|pkh1| {
                pubkeyhash_goldens()
                    .into_iter()
                    .map(|pkh2| {
                        DCert::PoolRegister(
                            PaymentPubKeyHash(pkh1.clone()),
                            PaymentPubKeyHash(pkh2),
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .collect(),
        staking_credential_goldens()
            .into_iter()
            .map(|staking_cred| DCert::DelegDeRegKey(staking_cred))
            .collect(),
        staking_credential_goldens()
            .iter()
            .flat_map(|staking_cred| {
                pubkeyhash_goldens()
                    .into_iter()
                    .map(|pkh| DCert::DelegDelegate(staking_cred.clone(), PaymentPubKeyHash(pkh)))
                    .collect::<Vec<_>>()
            })
            .collect(),
    ]
    .concat()
}

pub fn script_purpose_goldens() -> List<ScriptPurpose> {
    [
        currency_symbol_goldens()
            .into_iter()
            .map(|cur_sym| ScriptPurpose::Minting(cur_sym))
            .collect::<Vec<_>>(),
        tx_out_ref_goldens()
            .into_iter()
            .map(|tx_out_ref| ScriptPurpose::Spending(tx_out_ref))
            .collect::<Vec<_>>(),
        staking_credential_goldens()
            .into_iter()
            .map(|staking_cred| ScriptPurpose::Rewarding(staking_cred))
            .collect::<Vec<_>>(),
        dcert_goldens()
            .into_iter()
            .map(|dcert| ScriptPurpose::Certifying(dcert))
            .collect::<Vec<_>>(),
    ]
    .concat()
}

pub fn tx_info_goldens_v1() -> List<TransactionInfo> {
    value_goldens()
        .iter()
        .flat_map(|fee| {
            value_goldens()
                .iter()
                .flat_map(|mint| {
                    posix_time_range_goldens()
                        .iter()
                        .flat_map(|valid_range| {
                            tx_id_goldens()
                                .into_iter()
                                .map(|tx_id| TransactionInfo {
                                    inputs: tx_in_info_goldens_v1(),
                                    outputs: tx_out_goldens_v1(),
                                    fee: fee.clone(),
                                    mint: mint.clone(),
                                    d_cert: dcert_goldens(),
                                    wdrl: staking_credential_goldens()
                                        .into_iter()
                                        .map(|staking_cred| (staking_cred, BigInt::from(1234)))
                                        .collect(),
                                    valid_range: valid_range.clone(),
                                    signatories: pubkeyhash_goldens()
                                        .into_iter()
                                        .map(|pkh| PaymentPubKeyHash(pkh))
                                        .collect(),
                                    datums: datum_hash_goldens()
                                        .into_iter()
                                        .zip(datum_goldens().into_iter())
                                        .collect(),
                                    id: tx_id,
                                })
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn script_context_goldens_v1() -> List<ScriptContext> {
    tx_info_goldens_v1()
        .iter()
        .flat_map(|tx_info| {
            script_purpose_goldens()
                .into_iter()
                .map(|purpose| ScriptContext {
                    purpose,
                    tx_info: tx_info.clone(),
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn tx_in_info_goldens_v2() -> List<TxInInfoV2> {
    tx_out_ref_goldens()
        .iter()
        .flat_map(|reference| {
            tx_out_goldens_v2().into_iter().map(|output| TxInInfoV2 {
                reference: reference.clone(),
                output,
            })
        })
        .collect()
}

pub fn tx_out_goldens_v2() -> List<TransactionOutputV2> {
    address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    out_datum_goldens()
                        .iter()
                        .take(2)
                        .flat_map(|datum| {
                            [None]
                                .into_iter()
                                .chain(script_hash_goldens().into_iter().map(Some))
                                .map(|reference_script| TransactionOutputV2 {
                                    address: address.clone(),
                                    datum: datum.clone(),
                                    value: value.clone(),
                                    reference_script,
                                })
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn out_datum_goldens() -> List<OutputDatum> {
    [
        vec![OutputDatum::None],
        datum_hash_goldens()
            .into_iter()
            .map(OutputDatum::DatumHash)
            .collect(),
        datum_goldens()
            .into_iter()
            .map(OutputDatum::InlineDatum)
            .collect(),
    ]
    .concat()
}

pub fn tx_info_goldens_v2() -> List<TransactionInfoV2> {
    value_goldens()
        .iter()
        .flat_map(|fee| {
            value_goldens()
                .iter()
                .flat_map(|mint| {
                    posix_time_range_goldens()
                        .iter()
                        .flat_map(|valid_range| {
                            tx_id_goldens()
                                .into_iter()
                                .map(|tx_id| TransactionInfoV2 {
                                    inputs: tx_in_info_goldens_v2(),
                                    reference_inputs: tx_in_info_goldens_v2(),
                                    outputs: tx_out_goldens_v2(),
                                    fee: fee.clone(),
                                    mint: mint.clone(),
                                    d_cert: dcert_goldens(),
                                    wdrl: AssocMap::from(
                                        staking_credential_goldens()
                                            .into_iter()
                                            .map(|staking_cred| (staking_cred, BigInt::from(1234)))
                                            .collect::<Vec<_>>(),
                                    ),
                                    valid_range: valid_range.clone(),
                                    signatories: pubkeyhash_goldens()
                                        .into_iter()
                                        .map(|pkh| PaymentPubKeyHash(pkh))
                                        .collect(),
                                    redeemers: AssocMap::from(
                                        script_purpose_goldens()
                                            .into_iter()
                                            .zip(redeemer_goldens().into_iter())
                                            .collect::<Vec<_>>(),
                                    ),
                                    datums: AssocMap::from(
                                        datum_hash_goldens()
                                            .into_iter()
                                            .zip(datum_goldens().into_iter())
                                            .collect::<Vec<_>>(),
                                    ),
                                    id: tx_id,
                                })
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn script_context_goldens_v2() -> List<ScriptContextV2> {
    tx_info_goldens_v2()
        .iter()
        .flat_map(|tx_info| {
            script_purpose_goldens()
                .into_iter()
                .map(|purpose| ScriptContextV2 {
                    purpose,
                    tx_info: tx_info.clone(),
                })
                .collect::<Vec<_>>()
        })
        .collect()
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

pub fn foo_prod_goldens<A, B, C>(x: A, y: B, z: C) -> List<FooProd<A, B, C>> {
    vec![FooProd(x, y, z, bi(1337))]
}

pub fn foo_rec_goldens<A, B, C>(x: A, y: B, z: C) -> List<FooRec<A, B, C>> {
    vec![FooRec {
        foo_a: x,
        foo_b: y,
        foo_c: z,
        foo_int: bi(1337),
    }]
}

pub fn a_goldens() -> List<A> {
    address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens()
                        .into_iter()
                        .flat_map(|datum| {
                            foo_sum_goldens(address.clone(), value.clone(), datum)
                                .into_iter()
                                .map(A)
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn b_goldens() -> List<B> {
    address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens().into_iter().flat_map(|datum| {
                        foo_prod_goldens(address.clone(), value.clone(), datum)
                            .into_iter()
                            .map(B)
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn c_goldens() -> List<C> {
    address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens().into_iter().flat_map(|datum| {
                        foo_rec_goldens(address.clone(), value.clone(), datum)
                            .into_iter()
                            .map(C)
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn d_goldens() -> List<D> {
    let foo_sum = address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens()
                        .into_iter()
                        .flat_map(|datum| foo_sum_goldens(address.clone(), value.clone(), datum))
                })
                .collect::<Vec<_>>()
        })
        .take(2)
        .collect::<Vec<_>>();
    let foo_prod = address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens()
                        .into_iter()
                        .flat_map(|datum| foo_prod_goldens(address.clone(), value.clone(), datum))
                })
                .collect::<Vec<_>>()
        })
        .take(2)
        .collect::<Vec<_>>();
    let foo_rec = address_goldens()
        .iter()
        .flat_map(|address| {
            value_goldens()
                .iter()
                .flat_map(|value| {
                    datum_goldens()
                        .into_iter()
                        .flat_map(|datum| foo_rec_goldens(address.clone(), value.clone(), datum))
                })
                .collect::<Vec<_>>()
        })
        .take(2)
        .collect::<Vec<_>>();

    foo_sum
        .iter()
        .flat_map(|sum| {
            foo_prod.iter().flat_map(|prod| {
                foo_rec.iter().map(|rec| {
                    D(FooComplicated {
                        sum: sum.clone(),
                        prod: prod.clone(),
                        rec: rec.clone(),
                    })
                })
            })
        })
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
    vec![false, true]
}

pub fn maybe_goldens() -> List<Option<Bool>> {
    vec![None, Some(true), Some(false)]
}

pub fn either_goldens() -> List<Either<Bool, Bool>> {
    vec![Err(true), Err(false), Ok(true)]
}

pub fn list_goldens() -> List<List<Bool>> {
    vec![
        Vec::new(),
        vec![true],
        vec![false],
        vec![true, true, false, false],
    ]
}
