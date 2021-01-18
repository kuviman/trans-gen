use super::*;

#[derive(Clone, Debug)]
pub struct RepairProperties {
    pub valid_targets: Vec<EntityType>,
    pub power: i32,
}

impl trans::Trans for RepairProperties {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.valid_targets.write_to(writer)?;
        self.power.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let valid_targets: Vec<EntityType> = trans::Trans::read_from(reader)?;
        let power: i32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            valid_targets,
            power,
        })
    }
}
