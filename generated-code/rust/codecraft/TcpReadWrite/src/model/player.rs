use super::*;

/// Player (strategy, client)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Player {
    /// Player's ID
    pub id: i32,
    /// Current score
    pub score: i32,
    /// Current amount of resource
    pub resource: i32,
}

impl trans::Trans for Player {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.id.write_to(writer)?;
        self.score.write_to(writer)?;
        self.resource.write_to(writer)?;
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let id: i32 = trans::Trans::read_from(reader)?;
        let score: i32 = trans::Trans::read_from(reader)?;
        let resource: i32 = trans::Trans::read_from(reader)?;
        Ok(Self {
            id,
            score,
            resource,
        })
    }
}