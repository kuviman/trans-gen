use super::*;

#[macro_export]
macro_rules! all_runnable_gens {
    ($macro:ident) => {
        $macro!(cpp);
        $macro!(csharp);
        $macro!(dlang);
        $macro!(fsharp);
        $macro!(go);
        $macro!(haskell);
        $macro!(java);
        $macro!(javascript);
        $macro!(kotlin);
        $macro!(pascal);
        $macro!(php);
        $macro!(python);
        $macro!(ruby);
        $macro!(rust);
        $macro!(scala);
        $macro!(swift);
        $macro!(typescript);
    };
}

#[macro_export]
macro_rules! all_gens {
    ($macro:ident) => {
        $crate::all_runnable_gens!($macro);
        $macro!(markdown);
    };
}

macro_rules! declare_mod {
    ($lang:ident) => {
        pub mod $lang;
    };
}

all_gens!(declare_mod);
