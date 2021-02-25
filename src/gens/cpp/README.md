# C++

Implementation for data types:

- `Bool` — represented as `bool`.
- `Int32` — represented as `int`.
- `Int64` — represented as `long long`.
- `Float32` — represented as `float`.
- `Float64` — represented as `double`.
- `String` — represented as `std::string`.
- `Option<T>` — represented as `std::optional<T>` for C++17 and later, or `std::shared_ptr` for previous standards, with `nullptr` representing absent value.
- `Vec<T>` — represented as `std::vector<T>`.
- `Map<K, V>` — represented as `std::unordered_map<K, V>`.
- `Struct` — represented as a regular class.
- `Enum` — represented as a regular enum.
- `OneOf` — represented as an abstract class with each variant inheriting from it. The value is then stored using `std::shared_ptr`.
