use super::*;

use crate as trans;

/// 2 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec2")]
#[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 2-х мерном пространстве"]
pub struct Vec2<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    pub y: T,
}

/// 3 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec3")]
#[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 3-х мерном пространстве"]
pub struct Vec3<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    pub y: T,
    /// `z` coordinate of the vector
    #[trans_doc = "ru:Координата z"]
    pub z: T,
}

/// 4 dimensional vector.
#[derive(Trans)]
#[trans(for = "batbox::Vec4")]
#[trans(no_generics_in_name)]
#[trans(namespace = "model")] // TODO
#[trans_doc = "ru:Вектор в 4-х мерном пространстве"]
pub struct Vec4<T> {
    /// `x` coordinate of the vector
    #[trans_doc = "ru:Координата x"]
    pub x: T,
    /// `y` coordinate of the vector
    #[trans_doc = "ru:Координата y"]
    pub y: T,
    /// `z` coordinate of the vector
    #[trans_doc = "ru:Координата z"]
    pub z: T,
    /// `w` coordinate of the vector
    #[trans_doc = "ru:Координата w"]
    pub w: T,
}

/// RGBA Color
#[derive(Trans)]
#[trans(for = "batbox::Color")]
#[trans(no_generics_in_name)]
#[trans(namespace = "debugging")] // TODO
#[trans_doc = "ru:Цвет в RGBA формате"]
pub struct Color<T: batbox::ColorComponent> {
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

impl<T: Trans + batbox::HasId> Trans for batbox::Collection<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Vec(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = batbox::Collection::new();
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
        let mut result = batbox::Collection::new();
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
