use super::*;

/// Entity's action
#[derive(Clone, Debug)]
pub struct EntityAction {
    /// Move action
    pub move_action: Option<model::MoveAction>,
    /// Build action
    pub build_action: Option<model::BuildAction>,
    /// Attack action
    pub attack_action: Option<model::AttackAction>,
    /// Repair action
    pub repair_action: Option<model::RepairAction>,
}

impl trans::Trans for EntityAction {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.move_action.write_to(writer)?;
        self.build_action.write_to(writer)?;
        self.attack_action.write_to(writer)?;
        self.repair_action.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let move_action: Option<model::MoveAction> = trans::Trans::read_from(reader)?;
        let build_action: Option<model::BuildAction> = trans::Trans::read_from(reader)?;
        let attack_action: Option<model::AttackAction> = trans::Trans::read_from(reader)?;
        let repair_action: Option<model::RepairAction> = trans::Trans::read_from(reader)?;
        Ok(Self {
            move_action,
            build_action,
            attack_action,
            repair_action,
        })
    }
}