use super::*;

#[derive(Clone, Debug)]
pub struct Example {
    pub one_of: OneOf,
    pub hash_map: std::collections::HashMap<Enumeration, i32>,
    pub optional_int: Option<i32>,
    pub optional_bool: Option<bool>,
    pub optional_one_of: Option<OneOf>,
    pub optional_struct: Option<Structure>,
    pub optional_enum: Option<Enumeration>,
}

impl trans::Trans for Example {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.one_of.write_to(writer)?;
        self.hash_map.write_to(writer)?;
        self.optional_int.write_to(writer)?;
        self.optional_bool.write_to(writer)?;
        self.optional_one_of.write_to(writer)?;
        self.optional_struct.write_to(writer)?;
        self.optional_enum.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let one_of: OneOf = trans::Trans::read_from(reader)?;
        let hash_map: std::collections::HashMap<Enumeration, i32> = trans::Trans::read_from(reader)?;
        let optional_int: Option<i32> = trans::Trans::read_from(reader)?;
        let optional_bool: Option<bool> = trans::Trans::read_from(reader)?;
        let optional_one_of: Option<OneOf> = trans::Trans::read_from(reader)?;
        let optional_struct: Option<Structure> = trans::Trans::read_from(reader)?;
        let optional_enum: Option<Enumeration> = trans::Trans::read_from(reader)?;
        Ok(Self {
            one_of,
            hash_map,
            optional_int,
            optional_bool,
            optional_one_of,
            optional_struct,
            optional_enum,
        })
    }
}