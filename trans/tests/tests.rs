use trans::prelude::*;
use trans::*;

#[test]
fn test_expected_eof() {
    deserialize::<bool>(b"\x00\xFF")
        .ensure_err_contains(err_fmt::expected_eof())
        .unwrap();
}
