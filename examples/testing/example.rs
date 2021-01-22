use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use trans::Trans;

/// Example enumeration
#[trans_doc = "ru:Пример enum"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Eq, Hash, Copy, Clone)]
pub enum Enumeration {
    /// First option
    #[trans_doc = "ru:Первый вариант"]
    ValueOne,
    /// Second option
    #[trans_doc = "ru:Второй вариант"]
    ValueTwo,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Eq, Hash, Copy, Clone)]
pub struct NewTypeInt32(i32);

/// Oneof example
#[trans_doc = "ru:Пример one of"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub enum OneOf {
    /// First option
    #[trans_doc = "ru:Первый вариант"]
    OptionOne {
        /// List of integers
        #[trans_doc = "ru:Список целых чисел"]
        vec_int32: Vec<i32>,
        /// Long integer
        #[trans_doc = "ru:Длинное целое"]
        long_int: i64,
    },
    /// Second option
    #[trans_doc = "ru:Первый вариант"]
    OptionTwo {
        /// usize
        #[trans_doc = "ru:usize"]
        value: usize,
    },
}

/// Example structure
#[trans_doc = "ru:Пример структуры"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Structure {
    /// Text
    #[trans_doc = "ru:Текст"]
    text: String,
    /// 32-bit float
    #[trans_doc = "ru:32-битное число с плавающей точкой"]
    float_number: f32,
    /// 64-bit float
    #[trans_doc = "ru:64-битное число с плавающей точкой"]
    double_number: f64,
}

/// Example
#[trans_doc = "ru:Пример"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans, Clone)]
pub struct Example {
    /// OneOf
    #[trans_doc = "ru:OneOf"]
    one_of: OneOf,
    /// Dictionary
    #[trans_doc = "ru:Таблица"]
    hash_map: HashMap<Enumeration, NewTypeInt32>,
    /// Optional int
    #[trans_doc = "ru:Опциональное целое число"]
    optional_int: Option<i32>,
    /// Optional boolean
    #[trans_doc = "ru:Опциональный boolean"]
    optional_bool: Option<bool>,
    /// Optional OneOf
    #[trans_doc = "ru:Опциональный OneOf"]
    optional_one_of: Option<OneOf>,
    /// Optional struct
    #[trans_doc = "ru:Опциональная структура"]
    optional_struct: Option<Structure>,
    /// Optional enum
    #[trans_doc = "ru:Опциональное перечисление"]
    optional_enum: Option<Enumeration>,
}

pub type Model = Example;
