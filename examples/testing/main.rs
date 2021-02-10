use anyhow::Context as _;
use std::path::PathBuf;
use trans_gen::TestExt as _;

#[derive(structopt::StructOpt)]
struct Opt {
    #[structopt(long = "model")]
    models: Vec<String>,
    #[structopt(long = "language")]
    langs: Vec<String>,
    #[structopt(long = "test")]
    tests: Vec<String>,
    #[structopt(long, default_value = "1")]
    repeat: usize,
    #[structopt(long)]
    generate: Option<PathBuf>,
    #[structopt(long)]
    show_stdout: Option<bool>,
}

macro_rules! all_models {
    ($macro:ident) => {
        $macro!(codecraft);
        $macro!(example);
    };
}

macro_rules! declare_mod {
    ($mod:ident) => {
        mod $mod;
    };
}
all_models!(declare_mod);

fn main() -> anyhow::Result<()> {
    let opt: Opt = structopt::StructOpt::from_args();
    if let Some(path) = &opt.generate {
        if path.is_dir() {
            std::fs::remove_dir_all(path).context("Failed to clear target directory")?;
        }
        std::fs::create_dir_all(path).context("Failed to create target directory")?;
    }
    macro_rules! test_lang {
        ($lang:ident) => {
            if opt.langs.is_empty()
                || opt.langs.contains(&stringify!($lang).to_owned())
                || opt.langs.contains(
                    &<trans_gen::gens::$lang::Generator as trans_gen::Generator>::NAME.to_owned(),
                )
            {
                let generate = opt.generate.as_ref().map(|path| {
                    if opt.langs.len() == 1 {
                        path.to_owned()
                    } else {
                        path.join(stringify!($lang))
                    }
                });
                macro_rules! test_model {
                    ($model:ident) => {
                        if opt.models.is_empty()
                            || opt.models.contains(&stringify!($model).to_owned())
                        {
                            let generate = generate.as_ref().map(|path| {
                                if opt.models.len() == 1 {
                                    path.to_owned()
                                } else {
                                    path.join(stringify!($model))
                                }
                            });
                            let snapshot: $model::Model = serde_json::from_str(include_str!(
                                concat!(stringify!($model), "-snapshot.json")
                            ))
                            .expect("Failed to read snapshot");
                            macro_rules! test {
                                ($test:ident) => {
                                    if opt.tests.is_empty()
                                        || opt.tests.contains(&stringify!($test).to_owned())
                                    {
                                        let generate = generate.as_ref().map(|path| {
                                            if opt.tests.len() == 1 {
                                                path.to_owned()
                                            } else {
                                                path.join(stringify!($test))
                                            }
                                        });
                                        let test = trans_gen::testing::$test {
                                            snapshot: snapshot.clone(),
                                            show_stdout: opt.show_stdout.unwrap_or(
                                                if opt.repeat == 1 {
                                                    $model::SHOW_STDOUT
                                                } else {
                                                    false
                                                },
                                            ),
                                            version: $model::version(),
                                            repeat: opt.repeat,
                                        };
                                        if let Some(path) = generate {
                                            test.generate::<trans_gen::gens::$lang::Generator>(
                                                &path,
                                            )
                                            .context(format!(
                                                "Failed to generate {}",
                                                stringify!($lang)
                                            ))?;
                                        } else {
                                            test.test::<trans_gen::gens::$lang::Generator>()
                                                .context(format!(
                                                    "Failed to test {}",
                                                    stringify!($lang)
                                                ))?;
                                        }
                                    }
                                };
                            }
                            test!(FileReadWrite);
                            test!(TcpReadWrite);
                        }
                    };
                }
                all_models!(test_model);
            }
        };
    }
    trans_gen::all_gens!(test_lang);
    Ok(())
}
