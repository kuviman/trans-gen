use super::*;

/// Client or server message
#[derive(Clone, Debug)]
pub enum MessageGameModel {
    /// Client message
    Client {
        /// Message
        message: codegame::ClientMessage,
    },
    /// Server message
    Server {
        /// Message
        message: codegame::ServerMessage,
    },
}

impl trans::Trans for MessageGameModel {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        match self {
            Self::Client {
                message,
            } => {
                <i32 as trans::Trans>::write_to(&0, writer)?;
                message.write_to(writer)?;
            }
            Self::Server {
                message,
            } => {
                <i32 as trans::Trans>::write_to(&1, writer)?;
                message.write_to(writer)?;
            }
        }
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => {
                let message: codegame::ClientMessage = trans::Trans::read_from(reader)?;
                Ok(Self::Client {
                    message,
                })
            }
            1 => {
                let message: codegame::ServerMessage = trans::Trans::read_from(reader)?;
                Ok(Self::Server {
                    message,
                })
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}