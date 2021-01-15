use super::*;

#[derive(Clone, Debug, trans::Trans)]
pub struct Structure {
    pub one_of_one: OneOf,
    pub one_of_two: OneOf,
    pub hash_map: std::collections::HashMap<Enumeration, i32>,
    pub text: String,
    pub real_number: f64,
}
