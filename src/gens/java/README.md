# Java

Implementation for data types:

- `Bool` — represented as `boolean`.
- `Int32` — represented as `int`.
- `Int64` — represented as `long`.
- `Float32` — represented as `float`.
- `Float64` — represented as `double`.
- `String` — represented as `String`.
- `Option<T>` — if `T` is nullable, represented same as `T` with `null` representing absent value. Otherwise, means that `T` is a primitive and a corresponding object type is used, e.g. `Integer` for `int`.
- `Vec<T>` — represented as `T[]`.
- `Map<K, V>` — represented as `java.util.Map<K, V>`.
- `Struct` — represented as a regular class.
- `Enum` — represented as a regular enum.
- `OneOf` — represented as an abstract class with each variant inheriting from it.
