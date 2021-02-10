use super::*;

/// Information available to the player
#[derive(Clone, Debug)]
pub struct PlayerView {
    /// Your player's ID
    pub my_id: i32,
    /// Size of the map
    pub map_size: i32,
    /// Whether fog of war is enabled
    pub fog_of_war: bool,
    /// Entity properties for each entity type
    pub entity_properties: std::collections::HashMap<model::EntityType, model::EntityProperties>,
    /// Max tick count for the game
    pub max_tick_count: i32,
    /// Max pathfind nodes when performing pathfinding in the game simulator
    pub max_pathfind_nodes: i32,
    /// Current tick
    pub current_tick: i32,
    /// List of players
    pub players: Vec<model::Player>,
    /// List of entities
    pub entities: Vec<model::Entity>,
}

impl trans::Trans for PlayerView {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.my_id.write_to(writer)?;
        self.map_size.write_to(writer)?;
        self.fog_of_war.write_to(writer)?;
        self.entity_properties.write_to(writer)?;
        self.max_tick_count.write_to(writer)?;
        self.max_pathfind_nodes.write_to(writer)?;
        self.current_tick.write_to(writer)?;
        self.players.write_to(writer)?;
        self.entities.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let my_id: i32 = trans::Trans::read_from(reader)?;
        let map_size: i32 = trans::Trans::read_from(reader)?;
        let fog_of_war: bool = trans::Trans::read_from(reader)?;
        let entity_properties: std::collections::HashMap<model::EntityType, model::EntityProperties> = trans::Trans::read_from(reader)?;
        let max_tick_count: i32 = trans::Trans::read_from(reader)?;
        let max_pathfind_nodes: i32 = trans::Trans::read_from(reader)?;
        let current_tick: i32 = trans::Trans::read_from(reader)?;
        let players: Vec<model::Player> = trans::Trans::read_from(reader)?;
        let entities: Vec<model::Entity> = trans::Trans::read_from(reader)?;
        Ok(Self {
            my_id,
            map_size,
            fog_of_war,
            entity_properties,
            max_tick_count,
            max_pathfind_nodes,
            current_tick,
            players,
            entities,
        })
    }
}