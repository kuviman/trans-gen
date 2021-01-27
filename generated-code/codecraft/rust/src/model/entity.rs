use super::*;

/// Game entity
#[derive(Clone, Debug)]
pub struct Entity {
    /// Entity's ID. Unique for each entity
    pub id: i32,
    /// Entity's owner player ID, if owned by a player
    pub player_id: Option<i32>,
    /// Entity's type
    pub entity_type: EntityType,
    /// Entity's position (corner with minimal coordinates)
    pub position: Vec2I32,
    /// Current health
    pub health: i32,
    /// If entity is active, it can perform actions
    pub active: bool,
}

impl trans::Trans for Entity {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.id.write_to(writer)?;
        self.player_id.write_to(writer)?;
        self.entity_type.write_to(writer)?;
        self.position.write_to(writer)?;
        self.health.write_to(writer)?;
        self.active.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let id: i32 = trans::Trans::read_from(reader)?;
        let player_id: Option<i32> = trans::Trans::read_from(reader)?;
        let entity_type: EntityType = trans::Trans::read_from(reader)?;
        let position: Vec2I32 = trans::Trans::read_from(reader)?;
        let health: i32 = trans::Trans::read_from(reader)?;
        let active: bool = trans::Trans::read_from(reader)?;
        Ok(Self {
            id,
            player_id,
            entity_type,
            position,
            health,
            active,
        })
    }
}