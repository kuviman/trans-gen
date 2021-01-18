## `OneOf`

Oneof example

One of:

- `OptionOne` - First option

    Fields:

    - `vec_i32`: `[int32]` - List of integers
    - `long_int`: `int64` - Long integer

- `OptionTwo` - Second option

    Fields:

    - `value`: `int32` - usize


## `Enumeration`

Example enumeration

Variants:

- `ValueOne` - First option
- `ValueTwo` - Second option

## `Structure`

Example model

Fields:

- `one_of_one`: `OneOf` - First oneof
- `one_of_two`: `OneOf` - Second oneof
- `hash_map`: `Map<Enumeration -> int32>` - Dictionary
- `text`: `string` - Text
- `float_number`: `float32` - 32-bit float
- `double_number`: `float64` - 64-bit float
