use trans::prelude::*;
use trans::*;

#[test]
fn test_expected_eof() {
    deserialize::<bool>(&trans::version(), b"\x00\xFF")
        .ensure_err_contains(error_format::expected_eof())
        .unwrap();
}

#[test]
fn test_failed_to_read_field_invalid_usize() {
    #[derive(Trans, Debug)]
    struct TestStruct<T> {
        field: T,
    }
    let value: i32 = -1;
    deserialize::<TestStruct<usize>>(
        &trans::version(),
        &serialize(&trans::version(), &TestStruct { field: value }).unwrap(),
    )
    .ensure_err_contains(error_format::read_field::<TestStruct<usize>>("field"))
    .unwrap()
    .ensure_err_contains(error_format::invalid_value_of_type::<usize, _>(value))
    .unwrap();
}

#[test]
fn test_failed_to_read_variant_field_eof() {
    #[derive(Trans, Debug)]
    enum TestEnum {
        Variant { field: String },
    }
    deserialize::<TestEnum>(
        &trans::version(),
        &serialize::<i32>(&trans::version(), &0).unwrap(),
    )
    .ensure_err_contains(error_format::read_variant_field::<TestEnum>(
        "Variant", "field",
    ))
    .unwrap()
    .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
    .unwrap();
}

#[test]
fn test_enum_variant_versions() {
    #[derive(Trans, Debug, PartialEq)]
    enum TestEnum {
        First,
        #[trans(version = ">=2")]
        SinceV2,
    }

    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("2.0.0").unwrap();

    let schema_v1 = Schema::of::<TestEnum>(&v1);
    assert!(matches!(schema_v1.as_ref(), Schema::Enum { variants, ..} if variants.len() == 1 ));

    let schema_v2 = Schema::of::<TestEnum>(&v2);
    assert!(matches!(schema_v2.as_ref(), Schema::Enum { variants, ..} if variants.len() == 2 ));

    let test_first = TestEnum::First;
    test_serde_eq(&v1, &test_first);
    test_serde_eq(&v2, &test_first);

    let test_v2 = TestEnum::SinceV2;
    // serialize(&v1, &test_v2).unwrap_err();
    test_serde_eq(&v2, &test_v2);

    // deserialize::<TestEnum>(&v1, &serialize(&v1, &1i32).unwrap())
    //     .ensure_err_contains(error_format::unexpected_tag::<TestEnum>(1))
    //     .unwrap();
}

#[test]
fn test_oneof_variant_versions() {
    #[derive(Trans, Debug, PartialEq)]
    enum TestOneOf {
        First {
            first: i32,
            #[trans(version = ">=2", default = "0")]
            since_v2: i32,
            last: i32,
        },
        #[trans(version = ">=2")]
        SinceV2 { field: i32 },
    }

    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("2.0.0").unwrap();

    let schema_v1 = Schema::of::<TestOneOf>(&v1);
    assert!(
        matches!(schema_v1.as_ref(), Schema::OneOf { variants, ..} if variants.len() == 1 && variants[0].fields.len() == 2 )
    );

    let schema_v2 = Schema::of::<TestOneOf>(&v2);
    assert!(
        matches!(schema_v2.as_ref(), Schema::OneOf { variants, ..} if variants.len() == 2 && variants[0].fields.len() == 3 )
    );

    let test_first = TestOneOf::First {
        first: 1,
        since_v2: 2,
        last: 3,
    };
    assert_eq!(
        deserialize::<TestOneOf>(&v1, &serialize(&v1, &test_first).unwrap()).unwrap(),
        TestOneOf::First {
            first: 1,
            since_v2: 0,
            last: 3
        },
    );
    test_serde_eq(&v2, &test_first);

    let test_v2 = TestOneOf::SinceV2 { field: 123 };
    // TODO
    // serialize(&v1, &test_v2).unwrap_err();
    test_serde_eq(&v2, &test_v2);

    // TODO
    // deserialize::<TestOneOf>(&v1, b"\x01\x00\x00\x00\xFF\xFF\xFF\xFF")
    //     .ensure_err_contains(error_format::unexpected_tag::<TestOneOf>(1))
    //     .unwrap();
    deserialize::<TestOneOf>(&v1, &serialize(&v2, &test_first).unwrap())
        .ensure_err_contains(error_format::expected_eof())
        .unwrap();
    deserialize::<TestOneOf>(&v2, &serialize(&v1, &test_first).unwrap())
        .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
        .unwrap();
}

#[test]
fn test_struct_variant_versions() {
    #[derive(Trans, Debug, PartialEq)]
    struct Test {
        first: i32,
        #[trans(version = ">=2", default = "-1")]
        since_v2: i32,
        last: i32,
    }

    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("2.0.0").unwrap();

    let schema_v1 = Schema::of::<Test>(&v1);
    assert!(
        matches!(schema_v1.as_ref(), Schema::Struct{ definition: Struct { fields, .. }, .. } if fields.len() == 2 )
    );

    let schema_v2 = Schema::of::<Test>(&v2);
    assert!(
        matches!(schema_v2.as_ref(), Schema::Struct { definition: Struct { fields, .. }, .. } if fields.len() == 3 )
    );

    let test = Test {
        first: 1,
        since_v2: 2,
        last: 3,
    };
    assert_eq!(
        deserialize::<Test>(&v1, &serialize(&v1, &test).unwrap()).unwrap(),
        Test {
            first: 1,
            since_v2: -1,
            last: 3
        },
    );
    test_serde_eq(&v2, &test);

    deserialize::<Test>(&v1, &serialize(&v2, &test).unwrap())
        .ensure_err_contains(error_format::expected_eof())
        .unwrap();
    deserialize::<Test>(&v2, &serialize(&v1, &test).unwrap())
        .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
        .unwrap();
}

#[test]
fn test_namespace() {
    #[derive(Trans)]
    enum Test {
        Variant1 { field: i32 },
        Variant2 { field: i32 },
    }
    assert!(
        matches!(Schema::of::<Test>(&version()).as_ref(), Schema::OneOf { namespace, .. } if namespace.parts == vec![] )
    );
    #[derive(Trans)]
    #[trans(namespace = "foo")]
    struct TestFoo {
        field: i32,
    }
    assert!(
        matches!(Schema::of::<TestFoo>(&version()).as_ref(), Schema::Struct { namespace, .. } if namespace.parts == vec![Name::new("foo".to_owned())] )
    );
    #[derive(Trans)]
    #[trans(namespace = "foo::bar")]
    enum TestFooBar {
        Variant1,
        Variant2,
    }
    assert!(
        matches!(Schema::of::<TestFooBar>(&version()).as_ref(), Schema::Enum { namespace, .. } if namespace.parts == vec![Name::new("foo".to_owned()), Name::new("bar".to_owned())] )
    );
}
