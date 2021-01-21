use super::*;

#[derive(Clone, Debug)]
pub struct Structure {
    pub text: String,
    pub float_number: f32,
    pub double_number: f64,
}

impl trans::Trans for Structure {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.text.write_to(writer)?;
        self.float_number.write_to(writer)?;
        self.double_number.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let text: String = trans::Trans::read_from(reader)?;
        let float_number: f32 = trans::Trans::read_from(reader)?;
        let double_number: f64 = trans::Trans::read_from(reader)?;
        Ok(Self {
            text,
            float_number,
            double_number,
        })
    }
}