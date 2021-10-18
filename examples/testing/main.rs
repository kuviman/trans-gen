use anyhow::Context as _;
use std::collections::BTreeMap;
use std::path::PathBuf;
use templing::*;
use trans_gen::util::*;
use trans_gen::TestExt as _;

#[derive(clap::Parser)]
struct Opt {
    #[clap(long = "model")]
    models: Vec<String>,
    #[clap(long = "language")]
    langs: Vec<String>,
    #[clap(long = "exclude-language")]
    exclude_langs: Vec<String>,
    #[clap(long = "test")]
    tests: Vec<String>,
    #[clap(long, default_value = "1")]
    repeat: usize,
    #[clap(long)]
    generate: Option<PathBuf>,
    #[clap(long)]
    verbose: bool,
    #[clap(long)]
    save_results: Option<PathBuf>,
    #[clap(long)]
    load_results: Vec<PathBuf>,
    #[clap(long)]
    code_path: Option<PathBuf>,
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
    let opt: Opt = clap::Parser::parse();
    if let Some(path) = &opt.generate {
        if path.is_dir() {
            std::fs::remove_dir_all(path).context("Failed to clear target directory")?;
        }
        std::fs::create_dir_all(path).context("Failed to create target directory")?;
    }
    type Results =
        BTreeMap<String, BTreeMap<String, BTreeMap<String, trans_gen::testing::TestResult>>>;
    let mut results: Results = Results::new();
    macro_rules! test_lang {
        ($lang:ident) => {
            if !(opt.exclude_langs.contains(&stringify!($lang).to_owned())
                || opt.exclude_langs.contains(
                    &<trans_gen::gens::$lang::Generator as trans_gen::Generator>::NAME.to_owned(),
                ))
                && (opt.langs.is_empty()
                    || opt.langs.contains(&stringify!($lang).to_owned())
                    || opt.langs.contains(
                        &<trans_gen::gens::$lang::Generator as trans_gen::Generator>::NAME
                            .to_owned(),
                    ))
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
                                            version: $model::version(),
                                            repeat: opt.repeat,
                                        };
                                        if let Some(path) = generate {
                                            test.generate::<trans_gen::gens::$lang::Generator>(
                                                &path,
                                                Default::default(),
                                            )
                                            .context(format!(
                                                "Failed to generate {}",
                                                stringify!($lang)
                                            ))?;
                                        } else {
                                            println!("Testing {}::{}::{}", <trans_gen::gens::$lang::Generator as trans_gen::Generator>::NAME, stringify!($model), stringify!($test));
                                            let result = test.test::<trans_gen::gens::$lang::Generator>(
                                                opt.code_path.as_ref().map(|path| path.as_ref()),
                                                Default::default(),
                                                opt.verbose,
                                            ).context("Test failed")?;
                                            let average_result = result.into_average(opt.repeat);
                                            if results.entry(<trans_gen::gens::$lang::Generator as trans_gen::Generator>::NAME.to_owned()).or_default()
                                                .entry(stringify!($model).to_owned()).or_default()
                                                .insert(stringify!($test).to_owned(), average_result).is_some() {
                                                panic!("WTF");
                                            }
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
    if opt.load_results.is_empty() {
        trans_gen::all_gens!(test_lang);
    } else {
        for path in &opt.load_results {
            results.extend::<Results>(
                serde_json::from_reader(std::fs::File::open(path).unwrap()).unwrap(),
            );
        }
    }
    if let Some(path) = opt.save_results {
        if let Some(ext) = path.extension() {
            if ext == "json" {
                serde_json::to_writer_pretty(std::fs::File::create(path).unwrap(), &results)
                    .unwrap();
            } else if ext == "md" {
                std::fs::write(
                    path,
                    include_templing!("examples/testing/results.md.templing"),
                )
                .context("Failed to write results")?;
            } else {
                anyhow::bail!("Unexpected results extension {:?}", ext);
            }
        }
    }
    Ok(())
}
