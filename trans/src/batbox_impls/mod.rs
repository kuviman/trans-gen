#![allow(dead_code)] // because we dont use these structs, only original ones

use super::*;

use crate as trans;

impl<T: batbox::num::Float + Trans> Trans for batbox::la::Angle<T> {
    fn create_schema(version: &Version) -> Schema {
        T::create_schema(version)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.as_radians().write_to(writer, version)
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        Ok(Self::from_radians(T::read_from(reader, version)?))
    }
}

/// 2 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::la::vec2")]
// #[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 2-х мерном пространстве"]
pub struct Vec2<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    #[trans(original_field = "0")]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    #[trans(original_field = "1")]
    pub y: T,
}

/// 3 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::la::vec3")]
// #[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 3-х мерном пространстве"]
pub struct Vec3<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    #[trans(original_field = "0")]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    #[trans(original_field = "1")]
    pub y: T,
    /// `z` coordinate of the vector
    #[trans_doc = "ru:Координата z"]
    #[trans(original_field = "2")]
    pub z: T,
}

/// 4 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::la::vec4")]
// #[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 4-х мерном пространстве"]
pub struct Vec4<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    #[trans(original_field = "0")]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    #[trans(original_field = "1")]
    pub y: T,
    /// `z` coordinate of the vector
    #[trans_doc = "ru:Координата z"]
    #[trans(original_field = "2")]
    pub z: T,
    /// `w` coordinate of the vector
    #[trans_doc = "ru:Координата w"]
    #[trans(original_field = "3")]
    pub w: T,
}

/// RGBA Color
#[derive(Trans)]
#[trans(for = "batbox::color::Rgba")]
#[trans(no_generics_in_name)]
#[trans(namespace = "debugging")] // TODO
#[trans_doc = "ru:Цвет в RGBA формате"]
pub struct Color<T: batbox::color::ColorComponent> {
    /// Red component
    #[trans_doc = "ru:Компонента красного цвета"]
    pub r: T,
    /// Green component
    #[trans_doc = "ru:Компонента зеленого цвета"]
    pub g: T,
    #[trans_doc = "ru:"]
    /// Blue component
    #[trans_doc = "ru:Компонента синего цвета"]
    pub b: T,
    /// Alpha (opacity) component
    #[trans_doc = "ru:Альфа компонента (непрозрачность)"]
    pub a: T,
}

impl<T: batbox::num::Float + Trans> Trans for batbox::num::RealImpl<T> {
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

impl<T: Trans + batbox::collection::HasId> Trans for batbox::collection::Collection<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Vec(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = batbox::collection::Collection::new();
        for _ in 0..len {
            result.insert(T::read_from(reader, version)?);
        }
        Ok(result)
    }
    fn read_from_limited(
        reader: &mut dyn std::io::Read,
        limit: usize,
        version: &Version,
    ) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        if len > limit {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Length limit of {} exceeded, got {}", limit, len),
            ));
        }
        let mut result = batbox::collection::Collection::new();
        for _ in 0..len {
            result.insert(T::read_from(reader, version)?);
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.len().write_to(writer, version)?;
        for item in self {
            item.write_to(writer, version)?;
        }
        Ok(())
    }
}
