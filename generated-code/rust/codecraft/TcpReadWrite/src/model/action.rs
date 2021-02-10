use super::*;

/// Player's action
#[derive(Clone, Debug)]
pub struct Action {
    /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
    pub entity_actions: std::collections::HashMap<i32, model::EntityAction>,
}

impl trans::Trans for Action {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.entity_actions.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let entity_actions: std::collections::HashMap<i32, model::EntityAction> = trans::Trans::read_from(reader)?;
        Ok(Self {
            entity_actions,
        })
    }
}