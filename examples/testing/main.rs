use anyhow::Context as _;
use std::path::PathBuf;

#[derive(structopt::StructOpt)]
enum Sub {
    Generate { path: PathBuf },
    Test {},
}

#[derive(structopt::StructOpt)]
struct Opt {
    #[structopt(long = "model")]
    models: Vec<String>,
    #[structopt(long = "lang")]
    langs: Vec<String>,
    #[structopt(subcommand)]
    sub: Sub,
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

    match opt {
        Opt {
            langs,
            models,
            sub: Sub::Generate { path },
        } => {
            std::fs::remove_dir_all(&path).context("Failed to clear target directory")?;
            macro_rules! generate_model {
                ($model:ident) => {
                    if models.is_empty() || models.contains(&stringify!($model).to_owned()) {
                        let path = if models.len() == 1 {
                            path.clone()
                        } else {
                            path.join(stringify!($model))
                        };
                        macro_rules! generate_runnable {
                            ($lang:ident) => {
                                if langs.is_empty() || langs.contains(&stringify!($lang).to_owned())
                                {
                                    let path = if langs.len() == 1 {
                                        path.clone()
                                    } else {
                                        path.join(stringify!($lang))
                                    };
                                    trans_gen::testing::generate_file_read_write::<
                                        $model::Model,
                                        trans_gen::gens::$lang::Generator,
                                    >(&path)
                                    .context(format!("Failed to generate {}", stringify!($lang)))?;
                                }
                            };
                        }
                        macro_rules! generate {
                            ($lang:ident) => {
                                if langs.is_empty() || langs.contains(&stringify!($lang).to_owned())
                                {
                                    let path = if langs.len() == 1 {
                                        path.clone()
                                    } else {
                                        path.join(stringify!($lang))
                                    };
                                    trans_gen::generate::<trans_gen::gens::$lang::Generator>(
                                        "trans-gen-test",
                                        env!("CARGO_PKG_VERSION"),
                                        Default::default(),
                                        &[trans::Schema::of::<$model::Model>()],
                                        vec![],
                                    )
                                    .write_to(path)
                                    .context(format!("Failed to generate {}", stringify!($lang)))?;
                                }
                            };
                        }
                        trans_gen::all_runnable_gens!(generate_runnable);
                        generate!(markdown);
                    }
                };
            }
            all_models!(generate_model);
        }
        Opt {
            langs,
            models,
            sub: Sub::Test {},
        } => {
            macro_rules! test_model {
                ($model:ident) => {
                    if models.is_empty() || models.contains(&stringify!($model).to_owned()) {
                        let snapshot: $model::Model = serde_json::from_str(include_str!(concat!(
                            stringify!($model),
                            "-snapshot.json"
                        )))
                        .expect("Failed to read snapshot");
                        macro_rules! test {
                            ($lang:ident) => {
                                if langs.is_empty() || langs.contains(&stringify!($lang).to_owned())
                                {
                                    trans_gen::testing::test_file_read_write::<
                                        $model::Model,
                                        trans_gen::gens::$lang::Generator,
                                    >(&snapshot)
                                    .context(format!("Failed to test {}", stringify!($lang)))?;
                                }
                            };
                        }
                        trans_gen::all_runnable_gens!(test);
                    }
                };
            }
            all_models!(test_model);
        }
    }
    Ok(())
}
