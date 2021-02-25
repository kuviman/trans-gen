# C\#

Implementation for data types:

- `Bool` — represented as `bool`.
- `Int32` — represented as `int`.
- `Int64` — represented as `long`.
- `Float32` — represented as `float`.
- `Float64` — represented as `double`.
- `String` — represented as `string`.
- `Option<T>` — if `T` is already nullable, represented same as `T` with `null` considered valid value. Otherwise, represented as `T?`
- `Vec<T>` — represented as `T[]`.
- `Map<K, V>` — represented as `System.Collections.Generic.IDictionary<K, V>`.
- `Struct` — represented as a regular struct.
- `Enum` — represented as a regular enum.
- `OneOf` — represented as an abstract class with each variant inheriting from it.
