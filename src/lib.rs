pub type Python = trans_gen_core::GeneratorImpl<trans_gen_python::Generator>;
pub type JavaScript = trans_gen_core::GeneratorImpl<trans_gen_javascript::Generator>;
pub type Ruby = trans_gen_core::GeneratorImpl<trans_gen_ruby::Generator>;
pub type Rust = trans_gen_core::GeneratorImpl<trans_gen_rust::Generator>;
pub type Java = trans_gen_core::GeneratorImpl<trans_gen_java::Generator>;
pub type Kotlin = trans_gen_core::GeneratorImpl<trans_gen_kotlin::Generator>;
pub type Scala = trans_gen_core::GeneratorImpl<trans_gen_scala::Generator>;
pub type CSharp = trans_gen_core::GeneratorImpl<trans_gen_csharp::Generator>;
pub type FSharp = trans_gen_core::GeneratorImpl<trans_gen_fsharp::Generator>;
pub type Cpp = trans_gen_core::GeneratorImpl<trans_gen_cpp::Generator>;
pub type Dlang = trans_gen_core::GeneratorImpl<trans_gen_dlang::Generator>;
pub type Go = trans_gen_core::GeneratorImpl<trans_gen_go::Generator>;

pub use trans_gen_core::*;
