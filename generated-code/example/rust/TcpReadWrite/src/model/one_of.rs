use super::*;

/// Oneof example
#[derive(Clone, Debug)]
pub enum OneOf {
    /// First option
    OptionOne {
        /// List of integers
        vec_i32: Vec<i32>,
        /// Long integer
        long_int: i64,
    },
    /// Second option
    OptionTwo {
        /// usize
        value: i32,
    },
}

impl trans::Trans for OneOf {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        match self {
            Self::OptionOne {
                vec_i32,
                long_int,
            } => {
                <i32 as trans::Trans>::write_to(&0, writer)?;
                vec_i32.write_to(writer)?;
                long_int.write_to(writer)?;
            }
            Self::OptionTwo {
                value,
            } => {
                <i32 as trans::Trans>::write_to(&1, writer)?;
                value.write_to(writer)?;
            }
        }
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => {
                let vec_i32: Vec<i32> = trans::Trans::read_from(reader)?;
                let long_int: i64 = trans::Trans::read_from(reader)?;
                Ok(Self::OptionOne {
                    vec_i32,
                    long_int,
                })
            }
            1 => {
                let value: i32 = trans::Trans::read_from(reader)?;
                Ok(Self::OptionTwo {
                    value,
                })
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}