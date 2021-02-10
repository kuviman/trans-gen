use super::*;

/// Move action
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MoveAction {
    /// Target position
    pub target: Vec2I32,
    /// Whether to try find closest position, if path to target is not found
    pub find_closest_position: bool,
    /// Whether to destroy other entities on the way
    pub break_through: bool,
}

impl trans::Trans for MoveAction {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.target.write_to(writer)?;
        self.find_closest_position.write_to(writer)?;
        self.break_through.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let target: Vec2I32 = trans::Trans::read_from(reader)?;
        let find_closest_position: bool = trans::Trans::read_from(reader)?;
        let break_through: bool = trans::Trans::read_from(reader)?;
        Ok(Self {
            target,
            find_closest_position,
            break_through,
        })
    }
}