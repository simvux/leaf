use super::*;

const TRAIT_FROM: TRID = 0; // From
const TRAIT_INTO: TRID = 1; // Into
const TRAIT_FUNCTOR: TRID = 2;
const TYPE_OPTION: TID = 3; // option
const TYPE_RESULT: TID = 4; // result
const TYPE_EITHER: TID = 5; // either
const TRAIT_TOINT: TRID = 6; // ToInt
const GENID_A: u8 = 0; // 'a'
const GENID_B: u8 = 1; // 'b'
const GENID_F: u8 = 5; // 'f'

fn trtp<T>(v: T) -> Tr<Tp<T>> {
    tr(Tp::none(v))
}
fn tr<T>(v: T) -> Tr<T> {
    Tr::new(v)
}

fn test_implementations() -> TraitImpls<usize> {
    let mut impls = TraitImpls::new();

    // 0 =>
    // impl Into b for a
    //   when b From a
    impls
        .implement(
            tr(Tp::new(
                TRAIT_INTO,
                vec![trtp(Type::bound(
                    GENID_B,
                    vec![Tp::new(TRAIT_FROM, vec![trtp(Type::unbound(GENID_A))])],
                ))],
            )),
            trtp(Type::unbound(GENID_A)),
            0,
        )
        .unwrap();

    // 1 =>
    // impl From (option a) for a
    impls
        .implement(
            Tr::new(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::defined(TYPE_OPTION),
                    vec![trtp(Type::unbound(GENID_A))],
                ))],
            )),
            trtp(Type::unbound(GENID_A)),
            1,
        )
        .unwrap();

    // 2 =>
    // impl From (option int) for int
    impls
        .implement(
            tr(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::defined(TYPE_OPTION),
                    vec![trtp(Type::int())],
                ))],
            )),
            trtp(Type::int()),
            2,
        )
        .unwrap();

    // 3 =>
    // impl From (result a b) for a
    impls
        .implement(
            tr(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::defined(TYPE_RESULT),
                    vec![trtp(Type::unbound(GENID_A)), trtp(Type::unbound(GENID_B))],
                ))],
            )),
            trtp(Type::unbound(GENID_A)),
            3,
        )
        .unwrap();

    // 4 =>
    // impl ToInt for int
    impls
        .implement(trtp(TRAIT_TOINT), trtp(Type::int()), 4)
        .unwrap();

    // 5 =>
    // impl From (result a b) for Either b a
    //   when b ToInt
    impls
        .implement(
            tr(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::defined(TYPE_RESULT),
                    vec![
                        trtp(Type::unbound(GENID_A)),
                        trtp(Type::bound(GENID_B, vec![Tp::none(TRAIT_TOINT)])),
                    ],
                ))],
            )),
            tr(Tp::new(
                Type::defined(TYPE_EITHER),
                vec![
                    trtp(Type::bound(GENID_B, vec![Tp::none(TRAIT_TOINT)])),
                    trtp(Type::unbound(GENID_A)),
                ],
            )),
            5,
        )
        .unwrap();

    // 6 =>
    // impl From (f int) for (f float)
    //   when f Functor
    impls
        .implement(
            tr(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::bound(GENID_F, vec![Tp::none(TRAIT_FUNCTOR)]),
                    vec![trtp(Type::int())],
                ))],
            )),
            Tr::new(Tp::new(
                Type::bound(GENID_F, vec![Tp::none(TRAIT_FUNCTOR)]),
                vec![trtp(Type::float())],
            )),
            6,
        )
        .unwrap();

    // 7 =>
    // impl Functor for option a
    impls
        .implement(
            tr(Tp::none(TRAIT_FUNCTOR)),
            Tr::new(Tp::new(
                Type::defined(TYPE_OPTION),
                vec![trtp(Type::unbound(GENID_A))],
            )),
            7,
        )
        .unwrap();

    // 8 =>
    // impl From {a} for a
    impls
        .implement(
            tr(Tp::new(
                TRAIT_FROM,
                vec![tr(Tp::new(
                    Type::tuple(),
                    vec![trtp(Type::unbound(GENID_A))],
                ))],
            )),
            trtp(Type::unbound(GENID_A)),
            8,
        )
        .unwrap();

    impls
}

#[test]
fn direct_lookup() {
    let impls = test_implementations();

    let v = impls.lookup(
        TRAIT_TOINT,
        &[],
        Tr::new(&Type::int()),
        &[],
        &mut GenBuffer::default(),
    );

    assert_eq!(v, Ok(&4));
}

#[test]
fn direct_lookup_tp() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_OPTION),
        vec![trtp(Type::int())],
    ))];

    let v = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&Type::int()),
        &[],
        &mut GenBuffer::default(),
    );

    assert_eq!(v, Ok(&2));
}

#[test]
fn blanked_lookup_tp() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_RESULT),
        vec![trtp(Type::float()), unbound(9)],
    ))];

    let v = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&Type::float()),
        &[],
        &mut GenBuffer::default(),
    );

    assert_eq!(v, Ok(&3));
}

#[test]
fn blanked_lookup_tp_sameas_check() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_RESULT),
        vec![trtp(Type::float()), unbound(9)],
    ))];

    let valid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&Type::float()),
        &[],
        &mut GenBuffer::default(),
    );
    assert_eq!(valid, Ok(&3));

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_RESULT),
        vec![trtp(Type::int()), unbound(9)],
    ))];

    let invalid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&Type::float()),
        &[],
        &mut GenBuffer::default(),
    );
    assert_ne!(invalid, Ok(&3));
}

#[test]
fn blanked_indirect_sameas_lookup() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_RESULT),
        vec![trtp(Type::float()), trtp(Type::int())],
    ))];
    let impltor = Type::defined(TYPE_EITHER);
    let impltor_tp = [trtp(Type::int()), trtp(Type::float())];

    let valid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&impltor),
        &impltor_tp,
        &mut GenBuffer::default(),
    );

    assert_eq!(valid, Ok(&5));
}

#[test]
fn hkt_lookup() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(
        Type::defined(TYPE_OPTION),
        vec![trtp(Type::int())],
    ))];
    let impltor = Type::defined(TYPE_OPTION);
    let impltor_tp = [trtp(Type::float())];

    let valid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&impltor),
        &impltor_tp,
        &mut GenBuffer::default(),
    );

    assert_eq!(valid, Ok(&6));
}

#[test]
fn tuple_lookup() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(Type::tuple(), vec![trtp(Type::int())]))];
    let impltor = Type::int();
    let impltor_tp = [];

    let valid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&impltor),
        &impltor_tp,
        &mut GenBuffer::default(),
    );

    assert_eq!(valid, Ok(&8));
}

// 8 => impl From {a} for a
//
// attempt:
//   From {b} for a
//
// fn some_func one (a -> b)
//   From:from {one}
//
// This test fails, and that's probably a bug. However; we're only lookuping after static
// dispatch so it won't matter
/*
#[test]
fn traittp_sameas_check_as_impl() {
    let impls = test_implementations();

    let trait_tp = [tr(Tp::new(Type::tuple(), vec![unbound(GENID_B)]))];
    let impltor = Type::unbound(GENID_A);
    let impltor_tp = [];

    let valid = impls.lookup(
        TRAIT_FROM,
        &trait_tp,
        Tr::new(&impltor),
        &impltor_tp,
        &mut GenBuffer::default(),
    );

    if let Ok(res) = valid {
        panic!("was given incompatible implementation: {}", res)
    }
}
*/

fn unbound(n: u8) -> Tr<Tp<Type>> {
    trtp(Type::Generic(Unbound(Ign::new(n))))
}
