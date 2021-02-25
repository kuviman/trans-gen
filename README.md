# trans-gen

![Continuous integration](https://github.com/kuviman/trans-gen/workflows/Continuous%20integration/badge.svg)

`trans-gen` is a code generator for `trans` — simple binary serialization protocol, for translating data between different programming languages.

## Trans

Trans is a simple binary serialization protocol.

### Data types

Trans supports following data types:

- `Bool` — a boolean value (`true`/`false`). Serialized as one byte: `0` for `false` and `1` for true, other byte values are invalid representation.
- `Int32` — 32-bit integer number. Serialized in little endian byte order.
- `Int64` — 64-bit integer number. Serialized in little endian byte order.
- `Float32` — 32-bit floating point number. Serialized in little endian byte order.
- `Float64` — 64-bit floating point number. Serialized in little endian byte order.
- `String` — **UTF-8 encoded** string. Serialized as length **in bytes** (as an `Int32`), followed by the bytes
- `Option<T>` — an optional value of type `T`. If absent, serialized as `false`. Otherwise, serialized as `true`, followed by the value.
- `Vec<T>` — a vector (array) with elements of type `T`. Serialized as number of elements (as an `Int32`), followed by the elements.
- `Map<K, V>` — a map (dictionary) with keys of type `K` and values of type `V`. Serialized as number of key-value pairs (as an `Int32`), followed by key-value pairs (key first, value second).
- `Struct` — a structure containing multiple fields. Each field may be of its own type. Serialized as all the fields one after another without any extra data, so field order is important.
- `Enum` — an enumeration, a type whose value is restricted to a set of variants. Serialized as index of the variant (as an `Int32`), starting with `0`.
- `OneOf` — an algebraic (sum) data type, a type that can be one of the given set of other types. Each variant type is actually, like a `Struct`, just a set of fields. Serialized as index of the type variant (as an `Int32`), starting with `0`, followed by the fields.

### Client vs Server

Trans makes a distinction between a "client" and "server". Server is the code that contains model definitions and needs to handle incorrect data when receiving data. Client is the code that needs to read data and is relying on it to be valid, thus can possibly implement some optimizations (like preallocating collection types).

### Versioning

One feature that is present only on "server" side is versioning. Basically, server should support clients of all possible versions, while client knows only of its specific version.

Versioning only makes sense for custom data types: `Struct`, `Enum` and `OneOf`.

For `Struct`s, different versions may contain different set of fields. All the fields are always present on the server, but client only knows about those present in its version. In case we are receiving data from a version that does not contain a specific field, a default value must be provided for constructing a struct on the server.

For `Enum`s, later versions may add extra variants. Removing variants is not yet supported.

`OneOf`s act like a combination of `Enum` and `Struct`, thus they can add extra variants and/or change the set of fields in each variant.

## Code generator

Code generator takes schemas provided by the server and produces client code for a specified version.

Supported languages (follow link for language implementation details):

- [C++](src/gens/cpp/README.md)
- [C#](src/gens/csharp/README.md)
- [D](src/gens/dlang/README.md)
- [F#](src/gens/fsharp/README.md)
- [Go](src/gens/go/README.md)
- [Haskell](src/gens/haskell/README.md)
- [Java](src/gens/java/README.md)
- [JavaScript](src/gens/javascript/README.md)
- [Kotlin](src/gens/kotlin/README.md)
- [Pascal](src/gens/pascal/README.md)
- [PHP](src/gens/php/README.md)
- [Python](src/gens/python/README.md)
- [Ruby](src/gens/ruby/README.md)
- [Rust](src/gens/rust/README.md)
- [Scala](src/gens/scala/README.md)
- [Swift](src/gens/swift/README.md)
- [TypeScript](src/gens/typescript/README.md)
- Markdown (generates documentation)

## Example of generated code & benchmarks

The `testing` example contains tests. You can see generated code for the it in the [`testing` branch of this repository](https://github.com/kuviman/trans-gen/tree/testing).

You can also see benchmarks for all the languages, platforms, tests and models that are run on GitHub Actions there. Alternatively, see [local benchmarks](local-benchmarks.md) (may be outdated).

Models:

- `example` — simple example model
- `codecraft` — models from AI Cup 2020 "CodeCraft"

Tests:

- FileReadWrite — reads data from a file and then writes it to another.
- TcpReadWrite — reads a stream of data from TCP connection to the server and writes it back.
