use super::*;

impl<T: Trans> Trans for Option<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Option(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let is_some = bool::read_from(reader, version)?;
        Ok(if is_some {
            Some(T::read_from(reader, version)?)
        } else {
            None
        })
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.is_some().write_to(writer, version)?;
        if let Some(value) = self {
            value.write_to(writer, version)?;
        }
        Ok(())
    }
}

#[test]
fn test_schema() {
    assert_eq!(
        *Schema::of::<Option<bool>>(&crate::version()),
        Schema::Option(Arc::new(Schema::Bool))
    );
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &Some(123));
    test_serde_eq(&crate::version(), &None::<i32>);
}
