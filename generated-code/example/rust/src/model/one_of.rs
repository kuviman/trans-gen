use super::*;

#[derive(Clone, Debug, trans::Trans)]
pub enum OneOf {
    OptionOne {
        value: Vec<i32>,
    },
    OptionTwo {
        value: i32,
    },
}
