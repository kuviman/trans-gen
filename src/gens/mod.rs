use super::*;

#[macro_export]
macro_rules! all_runnable_gens {
    ($macro:ident) => {
        $macro!(cpp);
        $macro!(csharp);
        $macro!(dlang);
        $macro!(go);
        $macro!(python);
        $macro!(ruby);
        $macro!(rust);
        $macro!(java);
        $macro!(kotlin);
        $macro!(scala);
        $macro!(fsharp);
        $macro!(javascript);
        $macro!(typescript);
    };
}

#[macro_export]
macro_rules! all_gens {
    ($macro:ident) => {
        all_runnable_gens!($macro);
        $macro!(markdown);
    };
}

macro_rules! declare_mod {
    ($lang:ident) => {
        pub mod $lang;
    };
}

all_gens!(declare_mod);
