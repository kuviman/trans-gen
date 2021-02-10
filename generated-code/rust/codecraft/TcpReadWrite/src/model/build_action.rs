use super::*;

/// Build action
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BuildAction {
    /// Type of an entity to build
    pub entity_type: model::EntityType,
    /// Desired position of new entity
    pub position: Vec2I32,
}

impl trans::Trans for BuildAction {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.entity_type.write_to(writer)?;
        self.position.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let entity_type: model::EntityType = trans::Trans::read_from(reader)?;
        let position: Vec2I32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            entity_type,
            position,
        })
    }
}