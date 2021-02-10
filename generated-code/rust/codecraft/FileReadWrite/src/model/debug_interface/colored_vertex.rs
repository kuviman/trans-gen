use super::*;

/// Vertex for debug rendering
#[derive(Clone, Debug)]
pub struct ColoredVertex {
    /// Position in world coordinates (if none, screen position (0, 0) is used)
    pub world_pos: Option<Vec2F32>,
    /// Additional offset in screen coordinates
    pub screen_offset: Vec2F32,
    /// Color to use
    pub color: Color,
}

impl trans::Trans for ColoredVertex {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.world_pos.write_to(writer)?;
        self.screen_offset.write_to(writer)?;
        self.color.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let world_pos: Option<Vec2F32> = trans::Trans::read_from(reader)?;
        let screen_offset: Vec2F32 = trans::Trans::read_from(reader)?;
        let color: Color = trans::Trans::read_from(reader)?;
        Ok(Self {
            world_pos,
            screen_offset,
            color,
        })
    }
}