use byteorder::{ReadBytesExt, WriteBytesExt};
use heck::*;
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

pub use trans_derive::*;

pub mod prelude {
    pub use super::test_utils::ResultExt as _;
    pub use super::Trans;
    pub use trans_derive::*;
}

pub mod err_fmt;
mod impls;
mod schema;
pub mod test_utils;

#[cfg(test)]
use test_utils::*;

pub use schema::*;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn try_from<U: std::fmt::Debug + Copy, T: std::convert::TryFrom<U>>(
    value: U,
) -> std::io::Result<T> {
    T::try_from(value).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::Other,
            err_fmt::invalid_value_of_type::<T, _>(value),
        )
    })
}

pub trait Trans: Sized + 'static {
    fn create_schema() -> Schema;
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()>;
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self>;
}

pub fn deserialize_only_from<T: Trans>(reader: impl std::io::Read) -> std::io::Result<T> {
    use std::io::BufRead;
    let mut reader = std::io::BufReader::new(reader);
    let value = T::read_from(&mut reader)?;
    if reader.fill_buf()?.is_empty() {
        Ok(value)
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            err_fmt::expected_eof(),
        ))
    }
}

pub fn deserialize<T: Trans>(bytes: &[u8]) -> std::io::Result<T> {
    deserialize_only_from(bytes)
}

pub fn serialize<T: Trans>(value: &T) -> std::io::Result<Vec<u8>> {
    let mut buffer = Vec::<u8>::new();
    value.write_to(&mut buffer)?;
    Ok(buffer)
}

#[test]
fn test_eof() {
    deserialize::<i32>(&[])
        .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
        .unwrap();
}
