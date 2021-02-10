## Common

### `OneOf`

Oneof example

One of:

* `OptionOne` &mdash; First option

  Fields:

  + `vec_int32`: `[int32]` &mdash; List of integers
  + `long_int`: `int64` &mdash; Long integer
* `OptionTwo` &mdash; Second option

  Fields:

  + `value`: `int32` &mdash; usize

### `Enumeration`

Example enumeration

Variants:

* `ValueOne` &mdash; First option
* `ValueTwo` &mdash; Second option

### `Structure`

Example structure

Fields:

* `text`: `string` &mdash; Text
* `float_number`: `float32` &mdash; 32-bit float
* `double_number`: `float64` &mdash; 64-bit float

### `Example`

Example

Fields:

* `one_of`: `OneOf` &mdash; OneOf
* `hash_map`: `Map<Enumeration -> int32>` &mdash; Dictionary
* `optional_int`: `Option<int32>` &mdash; Optional int
* `optional_bool`: `Option<boolean>` &mdash; Optional boolean
* `optional_one_of`: `Option<OneOf>` &mdash; Optional OneOf
* `optional_struct`: `Option<Structure>` &mdash; Optional struct
* `optional_enum`: `Option<Enumeration>` &mdash; Optional enum