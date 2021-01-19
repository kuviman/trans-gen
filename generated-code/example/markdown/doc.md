## `OneOf`

Oneof example

One of:

* `OptionOne` &mdash; First option

  Fields:

  + `vec_int32`: `[int32]` &mdash; List of integers
  + `long_int`: `int64` &mdash; Long integer
* `OptionTwo` &mdash; Second option

  Fields:

  + `value`: `int32` &mdash; usize

## `Enumeration`

Example enumeration

Variants:

* `ValueOne` &mdash; First option
* `ValueTwo` &mdash; Second option

## `Structure`

Example model

Fields:

* `one_of_one`: `OneOf` &mdash; First oneof
* `one_of_two`: `OneOf` &mdash; Second oneof
* `hash_map`: `Map<Enumeration -> int32>` &mdash; Dictionary
* `text`: `string` &mdash; Text
* `float_number`: `float32` &mdash; 32-bit float
* `double_number`: `float64` &mdash; 64-bit float