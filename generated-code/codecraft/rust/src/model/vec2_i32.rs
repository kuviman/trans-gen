use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Vec2I32 {
    pub x: i32,
    pub y: i32,
}

impl trans::Trans for Vec2I32 {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.x.write_to(writer)?;
        self.y.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let x: i32 = trans::Trans::read_from(reader)?;
        let y: i32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            x,
            y,
        })
    }
}
