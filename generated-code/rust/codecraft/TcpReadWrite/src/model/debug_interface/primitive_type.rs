use super::*;

/// Primitive type for debug rendering
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    /// Lines, number of vertices should be divisible by 2
    Lines,
    /// Triangles, number of vertices should be divisible by 3
    Triangles,
}

impl trans::Trans for PrimitiveType {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        let tag: i32 = match self {
            Self::Lines => 0,
            Self::Triangles => 1,
        };
        trans::Trans::write_to(&tag, writer)
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => Ok(Self::Lines),
            1 => Ok(Self::Triangles),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}