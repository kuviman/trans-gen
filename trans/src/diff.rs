use super::*;

// This should not be necessary and should instead be just same as `Self: Diff + Trans where <Self as Diff>::Delta: Trans`
// https://github.com/rust-lang/rust/issues/20671
pub trait Diff:
    std::fmt::Debug
    + Trans
    + serde::Serialize
    + for<'de> serde::Deserialize<'de>
    + Sync
    + Send
    + Clone
    + PartialEq
    + 'static
{
    type Delta: std::fmt::Debug
        + Trans
        + serde::Serialize
        + for<'de> serde::Deserialize<'de>
        + Sync
        + Send
        + Clone
        + 'static;
    fn diff(&self, to: &Self) -> Self::Delta;
    fn update(&mut self, delta: &Self::Delta);
}

impl<T: batbox::Diff> Diff for T
where
    T: Trans,
    <T as batbox::Diff>::Delta: Trans,
{
    type Delta = <T as batbox::Diff>::Delta;
    fn diff(&self, to: &Self) -> Self::Delta {
        <T as batbox::Diff>::diff(self, to)
    }
    fn update(&mut self, delta: &Self::Delta) {
        <T as batbox::Diff>::update(self, delta)
    }
}
