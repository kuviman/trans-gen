use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Enumeration {
    ValueOne,
    ValueTwo,
}

impl trans::Trans for Enumeration {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        let tag: i32 = match self {
            Self::ValueOne => 0,
            Self::ValueTwo => 1,
        };
        trans::Trans::write_to(&tag, writer)
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => Ok(Self::ValueOne),
            1 => Ok(Self::ValueTwo),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}
