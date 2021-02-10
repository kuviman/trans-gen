use super::*;

/// RGBA Color
#[derive(Clone, Debug)]
pub struct Color {
    /// Red component
    pub r: f32,
    /// Green component
    pub g: f32,
    /// Blue component
    pub b: f32,
    /// Alpha (opacity) component
    pub a: f32,
}

impl trans::Trans for Color {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.r.write_to(writer)?;
        self.g.write_to(writer)?;
        self.b.write_to(writer)?;
        self.a.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let r: f32 = trans::Trans::read_from(reader)?;
        let g: f32 = trans::Trans::read_from(reader)?;
        let b: f32 = trans::Trans::read_from(reader)?;
        let a: f32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            r,
            g,
            b,
            a,
        })
    }
}