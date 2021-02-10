use super::*;

/// Debug data can be drawn in the app
#[derive(Clone, Debug)]
pub enum DebugData {
    /// Log some text
    Log {
        /// Text to show
        text: String,
    },
    /// Draw primitives
    Primitives {
        /// Vertices
        vertices: Vec<model::debug_interface::ColoredVertex>,
        /// Primitive type
        primitive_type: model::debug_interface::PrimitiveType,
    },
    /// Draw text
    PlacedText {
        /// Vertex to determine text position and color
        vertex: model::debug_interface::ColoredVertex,
        /// Text
        text: String,
        /// Text alignment (0 means left, 0.5 means center, 1 means right)
        alignment: f32,
        /// Font size in pixels
        size: f32,
    },
}

impl trans::Trans for DebugData {
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        match self {
            Self::Log {
                text,
            } => {
                <i32 as trans::Trans>::write_to(&0, writer)?;
                text.write_to(writer)?;
            }
            Self::Primitives {
                vertices,
                primitive_type,
            } => {
                <i32 as trans::Trans>::write_to(&1, writer)?;
                vertices.write_to(writer)?;
                primitive_type.write_to(writer)?;
            }
            Self::PlacedText {
                vertex,
                text,
                alignment,
                size,
            } => {
                <i32 as trans::Trans>::write_to(&2, writer)?;
                vertex.write_to(writer)?;
                text.write_to(writer)?;
                alignment.write_to(writer)?;
                size.write_to(writer)?;
            }
        }
        Ok(())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let tag = <i32 as trans::Trans>::read_from(reader)?;
        match tag {
            0 => {
                let text: String = trans::Trans::read_from(reader)?;
                Ok(Self::Log {
                    text,
                })
            }
            1 => {
                let vertices: Vec<model::debug_interface::ColoredVertex> = trans::Trans::read_from(reader)?;
                let primitive_type: model::debug_interface::PrimitiveType = trans::Trans::read_from(reader)?;
                Ok(Self::Primitives {
                    vertices,
                    primitive_type,
                })
            }
            2 => {
                let vertex: model::debug_interface::ColoredVertex = trans::Trans::read_from(reader)?;
                let text: String = trans::Trans::read_from(reader)?;
                let alignment: f32 = trans::Trans::read_from(reader)?;
                let size: f32 = trans::Trans::read_from(reader)?;
                Ok(Self::PlacedText {
                    vertex,
                    text,
                    alignment,
                    size,
                })
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unexpected tag {:?}", tag))),
        }
    }
}