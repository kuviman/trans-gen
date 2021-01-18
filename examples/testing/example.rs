use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use trans::Trans;

/// Example enumeration
#[trans_doc = "ru:Пример enum"]
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Copy, Clone, Trans)]
pub enum Enumeration {
    /// First option
    #[trans_doc = "ru:Первый вариант"]
    ValueOne,
    /// Second option
    #[trans_doc = "ru:Второй вариант"]
    ValueTwo,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Copy, Clone, Trans)]
pub struct NewTypeInt32(i32);

/// Oneof example
#[trans_doc = "ru:Пример one of"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans)]
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

/// Example model
#[trans_doc = "ru:Пример структуры"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans)]
pub struct Structure {
    /// First oneof
    #[trans_doc = "ru:Первый oneof"]
    one_of_one: OneOf,
    /// Second oneof
    #[trans_doc = "ru:Второй oneof"]
    one_of_two: OneOf,
    /// Dictionary
    #[trans_doc = "ru:Таблица"]
    hash_map: HashMap<Enumeration, NewTypeI32>,
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

pub type Model = Structure;
