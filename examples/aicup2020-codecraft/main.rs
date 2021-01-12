mod model;

macro_rules! all_langs {
    ($macro:ident) => {
        $macro!(cpp);
        $macro!(csharp);
        $macro!(dlang);
        $macro!(fsharp);
        $macro!(go);
        $macro!(java);
        $macro!(javascript);
        $macro!(kotlin);
        $macro!(markdown);
        $macro!(python);
        $macro!(ruby);
        $macro!(rust);
        $macro!(scala);
    };
}

#[derive(structopt::StructOpt)]
enum Opt {
    Generate { path: std::path::PathBuf },
    Test,
}

fn generate<T: trans_gen::Generator>(path: &std::path::Path) {
    let mut generator =
        trans_gen::GeneratorImpl::<T>::new("aicup2020-codecraft", "1.0.0", Default::default());
    generator.add(&trans::Schema::of::<model::PlayerView>());
    let result = generator.result();
    result.write_to(path).unwrap();
}

fn generate_all(path: &std::path::Path) {
    macro_rules! generate {
        ($lang:ident) => {
            generate::<trans_gen::gens::$lang::Generator>(&path.join(stringify!($lang)));
        };
    }
    all_langs!(generate);
}

fn main() {
    let snapshot: model::PlayerView =
        serde_json::from_str(include_str!("snapshot.json")).expect("Failed to read snapshot");
    let opt: Opt = structopt::StructOpt::from_args();

    match opt {
        Opt::Generate { path } => {
            generate_all(&path);
        }
        Opt::Test => {
            let tempdir = tempfile::tempdir().unwrap();
            generate_all(tempdir.as_ref());
        }
    }
}
