# Pascal

Implementation for data types:

- `Bool` — represented as `Boolean`.
- `Int32` — represented as `Int32`.
- `Int64` — represented as `Int64`.
- `Float32` — represented as `Single`.
- `Float64` — represented as `Double`.
- `String` — represented as `String`.
- `Option<T>` — represented as `TNullable<T>` (unit included).
- `Vec<T>` — represented as `TArray<T>` (`array of T`).
- `Map<K, V>` — represented as `TDictionary<K, V>` (from `Generics.Collections` unit).
- `Struct` — represented as a regular class.
- `Enum` — represented as a regular enum.
- `OneOf` — represented as an abstract class with each variant inheriting from it.
