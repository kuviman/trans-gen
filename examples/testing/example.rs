use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use trans::Trans;

/// Example enumeration
#[trans_doc = "ru:Пример enum"]
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Copy, Clone, Trans)]
pub enum Enumeration {
    ValueOne,
    ValueTwo,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Copy, Clone, Trans)]
pub struct NewTypeI32(i32);

/// Oneof example
#[trans_doc = "ru:Пример one of"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans)]
pub enum OneOf {
    OptionOne { vec_i32: Vec<i32>, long_int: i64 },
    OptionTwo { value: usize },
}

/// Example model
#[trans_doc = "ru:Пример структуры"]
#[derive(PartialEq, Debug, Serialize, Deserialize, Trans)]
pub struct Structure {
    one_of_one: OneOf,
    one_of_two: OneOf,
    hash_map: HashMap<Enumeration, NewTypeI32>,
    text: String,
    float_number: f32,
    double_number: f64,
}

pub type Model = Structure;
