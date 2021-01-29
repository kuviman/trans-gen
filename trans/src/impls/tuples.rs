use super::*;

macro_rules! impl_for_tuple {
    ($($name:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($name: Trans),*> Trans for ($($name,)*) {
            fn create_schema(_version: &Version) -> Schema {
                todo!()
            }
            fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
                Ok(($(<$name as Trans>::read_from(reader, version)?,)*))
            }
            fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
                let ($($name,)*) = self;
                $($name.write_to(writer, version)?;)*
                Ok(())
            }
        }
    };
}

impl_for_tuple!();
impl_for_tuple!(A);
impl_for_tuple!(A, B);
impl_for_tuple!(A, B, C);
impl_for_tuple!(A, B, C, D);
