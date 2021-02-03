use super::*;

/// Entity type
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EntityType {
    /// Wall, can be used to prevent enemy from moving through
    Wall,
    /// House, used to increase population
    House,
    /// Base for recruiting new builder units
    BuilderBase,
    /// Builder unit can build buildings
    BuilderUnit,
    /// Base for recruiting new melee units
    MeleeBase,
    /// Melee unit
    MeleeUnit,
    /// Base for recruiting new ranged units
    RangedBase,
    /// Ranged unit
    RangedUnit,
    /// Resource can be harvested
    Resource,
    /// Ranged attacking building
    Turret,
}

impl trans::Trans for EntityType {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        let tag: i32 = match self {
            Self::Wall => 0,
            Self::House => 1,
            Self::BuilderBase => 2,
            Self::BuilderUnit => 3,
            Self::MeleeBase => 4,
            Self::MeleeUnit => 5,
            Self::RangedBase => 6,
            Self::RangedUnit => 7,
            Self::Resource => 8,
            Self::Turret => 9,
        };
        trans::Trans::write_to(&tag, writer)
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => Ok(Self::Wall),
            1 => Ok(Self::House),
            2 => Ok(Self::BuilderBase),
            3 => Ok(Self::BuilderUnit),
            4 => Ok(Self::MeleeBase),
            5 => Ok(Self::MeleeUnit),
            6 => Ok(Self::RangedBase),
            7 => Ok(Self::RangedUnit),
            8 => Ok(Self::Resource),
            9 => Ok(Self::Turret),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}