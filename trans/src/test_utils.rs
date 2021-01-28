use super::*;

pub trait ResultExt<T, E> {
    fn ensure_err(&self) -> anyhow::Result<&E>;
    fn ensure_err_contains(&self, substring: impl AsRef<str>) -> anyhow::Result<&Self>;
    fn ensure_err_kind(&self, kind: std::io::ErrorKind) -> anyhow::Result<&Self>;
}

impl<T: std::fmt::Debug> ResultExt<T, std::io::Error> for Result<T, std::io::Error> {
    fn ensure_err(&self) -> anyhow::Result<&std::io::Error> {
        match self {
            Ok(value) => anyhow::bail!("Expected error, got Ok({:?})", value),
            Err(e) => Ok(e),
        }
    }
    fn ensure_err_contains(&self, substring: impl AsRef<str>) -> anyhow::Result<&Self> {
        let substring = substring.as_ref();
        let err = self.ensure_err()?;
        if format!("{:?}", err).contains(substring) {
            Ok(self)
        } else {
            anyhow::bail!(
                "Error expected to contain {:?}, but doesn't: {}",
                substring,
                err,
            )
        }
    }
    fn ensure_err_kind(&self, kind: std::io::ErrorKind) -> anyhow::Result<&Self> {
        let err = self.ensure_err()?;
        let actual_kind = err.kind();
        if actual_kind == kind {
            Ok(self)
        } else {
            anyhow::bail!(
                "Error expected to be of kind {:?}, but is {:?}: {}",
                kind,
                actual_kind,
                err,
            )
        }
    }
}

pub fn test_serde<T: Trans + std::fmt::Debug + PartialEq>(value: &T) {
    let serded: T = deserialize(&serialize(value).expect("Failed to serialize"))
        .expect("Failed to deserialize");
    assert_eq!(*value, serded);
}
