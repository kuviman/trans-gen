use super::*;

/// Entity properties
#[derive(Clone, Debug)]
pub struct EntityProperties {
    /// Size. Entity has a form of a square with side of this length
    pub size: i32,
    /// Score for building this entity
    pub build_score: i32,
    /// Score for destroying this entity
    pub destroy_score: i32,
    /// Whether this entity can move
    pub can_move: bool,
    /// Number of population points this entity provides, if active
    pub population_provide: i32,
    /// Number of population points this entity uses
    pub population_use: i32,
    /// Maximum health points
    pub max_health: i32,
    /// Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    pub initial_cost: i32,
    /// If fog of war is enabled, maximum distance at which other entities are considered visible
    pub sight_range: i32,
    /// Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    pub resource_per_health: i32,
    /// Build properties, if entity can build
    pub build: Option<model::BuildProperties>,
    /// Attack properties, if entity can attack
    pub attack: Option<model::AttackProperties>,
    /// Repair properties, if entity can repair
    pub repair: Option<model::RepairProperties>,
}

impl trans::Trans for EntityProperties {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.size.write_to(writer)?;
        self.build_score.write_to(writer)?;
        self.destroy_score.write_to(writer)?;
        self.can_move.write_to(writer)?;
        self.population_provide.write_to(writer)?;
        self.population_use.write_to(writer)?;
        self.max_health.write_to(writer)?;
        self.initial_cost.write_to(writer)?;
        self.sight_range.write_to(writer)?;
        self.resource_per_health.write_to(writer)?;
        self.build.write_to(writer)?;
        self.attack.write_to(writer)?;
        self.repair.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let size: i32 = trans::Trans::read_from(reader)?;
        let build_score: i32 = trans::Trans::read_from(reader)?;
        let destroy_score: i32 = trans::Trans::read_from(reader)?;
        let can_move: bool = trans::Trans::read_from(reader)?;
        let population_provide: i32 = trans::Trans::read_from(reader)?;
        let population_use: i32 = trans::Trans::read_from(reader)?;
        let max_health: i32 = trans::Trans::read_from(reader)?;
        let initial_cost: i32 = trans::Trans::read_from(reader)?;
        let sight_range: i32 = trans::Trans::read_from(reader)?;
        let resource_per_health: i32 = trans::Trans::read_from(reader)?;
        let build: Option<model::BuildProperties> = trans::Trans::read_from(reader)?;
        let attack: Option<model::AttackProperties> = trans::Trans::read_from(reader)?;
        let repair: Option<model::RepairProperties> = trans::Trans::read_from(reader)?;
        Ok(Self {
            size,
            build_score,
            destroy_score,
            can_move,
            population_provide,
            population_use,
            max_health,
            initial_cost,
            sight_range,
            resource_per_health,
            build,
            attack,
            repair,
        })
    }
}