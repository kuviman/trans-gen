use super::*;

impl<T: Trans + Eq + std::hash::Hash> Trans for HashSet<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Vec(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = HashSet::new();
        for _ in 0..len {
            result.insert(T::read_from(reader, version)?);
        }
        Ok(result)
    }
    fn read_from_limited(
        reader: &mut dyn std::io::Read,
        limit: usize,
        version: &Version,
    ) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        if len > limit {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Length limit of {} exceeded, got {}", limit, len),
            ));
        }
        let mut result = HashSet::new();
        for _ in 0..len {
            result.insert(T::read_from(reader, version)?);
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
        *Schema::of::<HashSet<String>>(&crate::version()),
        Schema::Vec(Arc::new(Schema::String)),
    );
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &HashSet::<i32>::new());
    test_serde_eq(&crate::version(), &{
        let mut set = HashSet::new();
        set.insert("hello".to_owned());
        set.insert("world".to_owned());
        set
    });
}

#[test]
fn test_oom() {
    #[derive(Debug, PartialEq, Eq, Hash)]
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
    <HashSet<BigData> as Trans>::read_from(
        &mut serialize(&crate::version(), &(i32::MAX, 0i32))
            .unwrap()
            .as_slice(),
        &crate::version(),
    )
    .ensure_err_kind(std::io::ErrorKind::Interrupted)
    .unwrap();
}
