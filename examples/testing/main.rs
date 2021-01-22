use anyhow::Context as _;
use std::path::PathBuf;
use trans_gen::TestExt as _;

#[derive(structopt::StructOpt)]
struct Opt {
    #[structopt(long = "model")]
    models: Vec<String>,
    #[structopt(long = "lang")]
    langs: Vec<String>,
    #[structopt(long)]
    generate: Option<PathBuf>,
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
    macro_rules! test_model {
        ($model:ident) => {
            if opt.models.is_empty() || opt.models.contains(&stringify!($model).to_owned()) {
                let generate = opt.generate.as_ref().map(|path| {
                    if opt.models.len() == 1 {
                        path.to_owned()
                    } else {
                        path.join(stringify!($model))
                    }
                });
                let snapshot: $model::Model = serde_json::from_str(include_str!(concat!(
                    stringify!($model),
                    "-snapshot.json"
                )))
                .expect("Failed to read snapshot");
                macro_rules! test {
                    ($lang:ident) => {
                        if opt.langs.is_empty() || opt.langs.contains(&stringify!($lang).to_owned())
                        {
                            let generate = generate.as_ref().map(|path| {
                                if opt.langs.len() == 1 {
                                    path.to_owned()
                                } else {
                                    path.join(stringify!($lang))
                                }
                            });
                            let test = trans_gen::testing::FileReadWrite {
                                snapshot: snapshot.clone(),
                            };
                            if let Some(path) = generate {
                                test.generate::<trans_gen::gens::$lang::Generator>(&path)
                                    .context(format!("Failed to generate {}", stringify!($lang)))?;
                            } else {
                                test.test::<trans_gen::gens::$lang::Generator>()
                                    .context(format!("Failed to test {}", stringify!($lang)))?;
                            }
                        }
                    };
                }
                trans_gen::all_runnable_gens!(test);
            }
        };
    }
    all_models!(test_model);
    Ok(())
}
