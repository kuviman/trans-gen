use super::*;

/// Repair action
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RepairAction {
    /// Target entity's ID
    pub target: i32,
}

impl trans::Trans for RepairAction {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.target.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let target: i32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            target,
        })
    }
}