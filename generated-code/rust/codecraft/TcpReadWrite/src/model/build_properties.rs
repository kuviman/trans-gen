use super::*;

/// Entity's build properties
#[derive(Clone, Debug)]
pub struct BuildProperties {
    /// Valid new entity types
    pub options: Vec<model::EntityType>,
    /// Initial health of new entity. If absent, it will have full health
    pub init_health: Option<i32>,
}

impl trans::Trans for BuildProperties {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.options.write_to(writer)?;
        self.init_health.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let options: Vec<model::EntityType> = trans::Trans::read_from(reader)?;
        let init_health: Option<i32> = trans::Trans::read_from(reader)?;
        Ok(Self {
            options,
            init_health,
        })
    }
}