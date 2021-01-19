use super::*;

#[derive(Clone, Debug)]
pub struct Structure {
    pub one_of_one: OneOf,
    pub one_of_two: OneOf,
    pub hash_map: std::collections::HashMap<Enumeration, i32>,
    pub text: String,
    pub float_number: f32,
    pub double_number: f64,
}

impl trans::Trans for Structure {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.one_of_one.write_to(writer)?;
        self.one_of_two.write_to(writer)?;
        self.hash_map.write_to(writer)?;
        self.text.write_to(writer)?;
        self.float_number.write_to(writer)?;
        self.double_number.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let one_of_one: OneOf = trans::Trans::read_from(reader)?;
        let one_of_two: OneOf = trans::Trans::read_from(reader)?;
        let hash_map: std::collections::HashMap<Enumeration, i32> = trans::Trans::read_from(reader)?;
        let text: String = trans::Trans::read_from(reader)?;
        let float_number: f32 = trans::Trans::read_from(reader)?;
        let double_number: f64 = trans::Trans::read_from(reader)?;
        Ok(Self {
            one_of_one,
            one_of_two,
            hash_map,
            text,
            float_number,
            double_number,
        })
    }
}