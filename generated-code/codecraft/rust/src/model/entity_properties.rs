use super::*;

#[derive(Clone, Debug)]
pub struct EntityProperties {
    pub size: i32,
    pub build_score: i32,
    pub destroy_score: i32,
    pub can_move: bool,
    pub population_provide: i32,
    pub population_use: i32,
    pub max_health: i32,
    pub initial_cost: i32,
    pub sight_range: i32,
    pub resource_per_health: i32,
    pub build: Option<BuildProperties>,
    pub attack: Option<AttackProperties>,
    pub repair: Option<RepairProperties>,
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
        let build: Option<BuildProperties> = trans::Trans::read_from(reader)?;
        let attack: Option<AttackProperties> = trans::Trans::read_from(reader)?;
        let repair: Option<RepairProperties> = trans::Trans::read_from(reader)?;
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