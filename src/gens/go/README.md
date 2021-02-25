# Go

Implementation for data types:

- `Bool` — represented as `bool`.
- `Int32` — represented as `int32`.
- `Int64` — represented as `int64`.
- `Float32` — represented as `float32`.
- `Float64` — represented as `float64`.
- `String` — represented as `string`.
- `Option<T>` — represented as `*T`, with `nil` representing absent value.
- `Vec<T>` — represented as `[]T`.
- `Map<K, V>` — represented as `map[K]V`.
- `Struct` — represented as a regular struct.
- `Enum` — represented as a constants for each variant.
- `OneOf` — represented as an interface and structs for each variant. Stored as a pointer to the interface.
