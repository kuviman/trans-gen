use super::*;

/// Auto attack options
#[derive(Clone, Debug)]
pub struct AutoAttack {
    /// Maximum distance to pathfind
    pub pathfind_range: i32,
    /// List of target entity types to try to attack. If empty, all types but resource are considered
    pub valid_targets: Vec<model::EntityType>,
}

impl trans::Trans for AutoAttack {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.pathfind_range.write_to(writer)?;
        self.valid_targets.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let pathfind_range: i32 = trans::Trans::read_from(reader)?;
        let valid_targets: Vec<model::EntityType> = trans::Trans::read_from(reader)?;
        Ok(Self {
            pathfind_range,
            valid_targets,
        })
    }
}