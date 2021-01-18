use super::*;

#[derive(Clone, Debug, trans::Trans)]
pub enum OneOf {
    OptionOne {
        vec_i32: Vec<i32>,
        long_int: i64,
    },
    OptionTwo {
        value: i32,
    },
}
