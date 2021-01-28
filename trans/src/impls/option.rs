use super::*;

impl<T: Trans> Trans for Option<T> {
    fn create_schema() -> Schema {
        Schema::Option(Schema::of::<T>())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let is_some = bool::read_from(reader)?;
        Ok(if is_some {
            Some(T::read_from(reader)?)
        } else {
            None
        })
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.is_some().write_to(writer)?;
        if let Some(value) = self {
            value.write_to(writer)?;
        }
        Ok(())
    }
}

#[test]
fn test_schema() {
    assert_eq!(
        *Schema::of::<Option<bool>>(),
        Schema::Option(Schema::of::<bool>())
    );
}

#[test]
fn test_serde() {
    crate::test_serde(&Some(123));
    crate::test_serde(&None::<i32>);
}
