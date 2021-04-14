use super::*;

use crate as trans;

/// 2 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec2")]
pub struct Vec2<T> {
    /// `x` coordinate of the vector
    pub x: T,
    /// `y` coordinate of the vector
    pub y: T,
}

/// 3 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec3")]
pub struct Vec3<T> {
    /// `x` coordinate of the vector
    pub x: T,
    /// `y` coordinate of the vector
    pub y: T,
    /// `z` coordinate of the vector
    pub z: T,
}

/// 4 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec4")]
pub struct Vec4<T> {
    /// `x` coordinate of the vector
    pub x: T,
    /// `y` coordinate of the vector
    pub y: T,
    /// `z` coordinate of the vector
    pub z: T,
    /// `w` coordinate of the vector
    pub w: T,
}

/// RGBA Color
#[derive(Trans)]
#[trans(for = "batbox::Color")]
#[trans(no_generics_in_name)]
pub struct Color<T> {
    /// Red component
    pub r: T,
    /// Green component
    pub g: T,
    /// Blue component
    pub b: T,
    /// Alpha (opacity) component
    pub a: T,
}

impl<T: batbox::Float + Trans> Trans for batbox::RealImpl<T> {
    fn create_schema(version: &trans::Version) -> trans::Schema {
        T::create_schema(version)
    }
    fn read_from(
        reader: &mut dyn std::io::Read,
        version: &trans::Version,
    ) -> Result<Self, std::io::Error> {
        let value = T::read_from(reader, version)?;
        if value.is_finite() {
            Ok(Self::new_unchecked(value))
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Value must be finite",
            ))
        }
    }
    fn write_to(
        &self,
        writer: &mut dyn std::io::Write,
        version: &trans::Version,
    ) -> Result<(), std::io::Error> {
        self.raw().write_to(writer, version)
    }
}
