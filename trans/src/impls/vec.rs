use super::*;

impl<T: Trans> Trans for Vec<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Vec(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = Vec::new();
        for _ in 0..len {
            result.push(T::read_from(reader, version)?);
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.len().write_to(writer, version)?;
        for item in self {
            item.write_to(writer, version)?;
        }
        Ok(())
    }
}

#[test]
fn test_schema() {
    assert_eq!(
        *Schema::of::<Vec<i32>>(&crate::version()),
        Schema::Vec(Arc::new(Schema::Int32))
    );
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &Vec::<String>::new());
    test_serde_eq(&crate::version(), &vec![1, 2, 3, 100500]);
}

#[test]
fn test_oom() {
    #[derive(Debug)]
    struct BigData([u8; 100500]);
    impl Trans for BigData {
        fn create_schema(_version: &Version) -> Schema {
            unimplemented!()
        }
        fn read_from(_reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
            Err(std::io::Error::from(std::io::ErrorKind::Interrupted))
        }
        fn write_to(
            &self,
            _writer: &mut dyn std::io::Write,
            _version: &Version,
        ) -> std::io::Result<()> {
            unimplemented!()
        }
    }
    <Vec<BigData> as Trans>::read_from(
        &mut serialize(&crate::version(), &i32::MAX).unwrap().as_slice(),
        &crate::version(),
    )
    .ensure_err_kind(std::io::ErrorKind::Interrupted)
    .unwrap();
}
