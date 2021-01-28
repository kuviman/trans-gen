pub fn invalid_value_of_type<T, V: std::fmt::Debug>(value: V) -> String {
    format!(
        "{:?} is invalid value of type {}",
        value,
        std::any::type_name::<T>(),
    )
}

pub fn invalid_bool(value: u8) -> String {
    format!(
        "Bool values should be encoded as 0 or 1, got 0x{:X?}",
        value,
    )
}

pub fn expected_eof() -> String {
    "Expected EOF".to_owned()
}
