use super::*;

/// Attack action
#[derive(Clone, Debug)]
pub struct AttackAction {
    /// If specified, target entity's ID
    pub target: Option<i32>,
    /// If specified, configures auto attacking
    pub auto_attack: Option<model::AutoAttack>,
}

impl trans::Trans for AttackAction {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.target.write_to(writer)?;
        self.auto_attack.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let target: Option<i32> = trans::Trans::read_from(reader)?;
        let auto_attack: Option<model::AutoAttack> = trans::Trans::read_from(reader)?;
        Ok(Self {
            target,
            auto_attack,
        })
    }
}