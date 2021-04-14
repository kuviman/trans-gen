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

mod batbox_impls;
mod diff;
pub mod error_format;
mod impls;
mod schema;
pub mod test_utils;

pub use diff::*;
pub use schema::*;
pub use test_utils::*;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub use semver::{Version, VersionReq};

pub fn version() -> Version {
    Version::parse(VERSION).unwrap()
}

fn try_from<U: std::fmt::Debug + Copy, T: std::convert::TryFrom<U>>(
    value: U,
) -> std::io::Result<T> {
    T::try_from(value).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::Other,
            error_format::invalid_value_of_type::<T, _>(value),
        )
    })
}

pub trait Trans: Sized + 'static {
    fn create_schema(version: &Version) -> Schema;
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()>;
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self>;
}

pub fn deserialize_only_from<T: Trans>(
    version: &Version,
    reader: impl std::io::Read,
) -> std::io::Result<T> {
    use std::io::BufRead;
    let mut reader = std::io::BufReader::new(reader);
    let value = T::read_from(&mut reader, version)?;
    if reader.fill_buf()?.is_empty() {
        Ok(value)
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            error_format::expected_eof(),
        ))
    }
}

pub fn deserialize<T: Trans>(version: &Version, bytes: &[u8]) -> std::io::Result<T> {
    deserialize_only_from(version, bytes)
}

pub fn serialize<T: Trans>(version: &Version, value: &T) -> std::io::Result<Vec<u8>> {
    let mut buffer = Vec::<u8>::new();
    value.write_to(&mut buffer, version)?;
    Ok(buffer)
}

#[test]
fn test_eof() {
    deserialize::<i32>(&crate::version(), &[])
        .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
        .unwrap();
}
