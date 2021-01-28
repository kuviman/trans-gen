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

pub fn write_field<T>(field_name: &str) -> String {
    format!(
        "Failed to write {}::{}",
        std::any::type_name::<T>(),
        field_name,
    )
}

pub fn read_field<T>(field_name: &str) -> String {
    format!(
        "Failed to read {}::{}",
        std::any::type_name::<T>(),
        field_name,
    )
}

pub fn write_variant_field<T>(variant_name: &str, field_name: &str) -> String {
    format!(
        "Failed to write {}::{}::{}",
        std::any::type_name::<T>(),
        variant_name,
        field_name,
    )
}

pub fn read_variant_field<T>(variant_name: &str, field_name: &str) -> String {
    format!(
        "Failed to read {}::{}::{}",
        std::any::type_name::<T>(),
        variant_name,
        field_name,
    )
}

pub fn unexpected_tag<T>(tag: i32) -> String {
    format!(
        "Unexpected tag {:?} for {}",
        tag,
        std::any::type_name::<T>(),
    )
}

pub fn read_tag<T>() -> String {
    format!("Failed to read tag of {}", std::any::type_name::<T>())
}

pub fn write_tag<T>(variant_name: &str) -> String {
    format!(
        "Failed to write tag of {}::{}",
        std::any::type_name::<T>(),
        variant_name,
    )
}
