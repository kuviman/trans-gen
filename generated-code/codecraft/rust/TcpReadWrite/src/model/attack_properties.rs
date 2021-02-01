use super::*;

/// Entity's attack properties
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AttackProperties {
    /// Maximum attack range
    pub attack_range: i32,
    /// Damage dealt in one tick
    pub damage: i32,
    /// If true, dealing damage will collect resource from target
    pub collect_resource: bool,
}

impl trans::Trans for AttackProperties {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.attack_range.write_to(writer)?;
        self.damage.write_to(writer)?;
        self.collect_resource.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let attack_range: i32 = trans::Trans::read_from(reader)?;
        let damage: i32 = trans::Trans::read_from(reader)?;
        let collect_resource: bool = trans::Trans::read_from(reader)?;
        Ok(Self {
            attack_range,
            damage,
            collect_resource,
        })
    }
}