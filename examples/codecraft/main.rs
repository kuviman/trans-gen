use anyhow::Context as _;
use std::path::{Path, PathBuf};
use std::process::Command;
use templing::*;

mod model;

macro_rules! write_file {
    ($dir:expr, $file:literal, $include_file:literal) => {{
        let file = $dir.join($file);
        let dir = file.parent().unwrap();
        std::fs::create_dir_all(&dir).context(format!("Failed to create {:?}", dir))?;
        std::fs::write(file, include_str!(concat!("files/", $include_file)))
            .context(format!("Failed to write {:?}", $file))
    }};
    ($dir:expr, $file:literal) => {
        write_file!($dir, $file, $file)
    };
}

macro_rules! all_runnable_langs {
    ($macro:ident) => {
        $macro!(cpp);
        $macro!(csharp);
        $macro!(dlang);
        $macro!(go);
        $macro!(python);
        $macro!(ruby);
        $macro!(rust);
    };
}

macro_rules! all_langs {
    ($macro:ident) => {
        all_runnable_langs!($macro);
        $macro!(fsharp);
        $macro!(java);
        $macro!(javascript);
        $macro!(kotlin);
        $macro!(markdown);
        $macro!(scala);
    };
}

macro_rules! declare_mod {
    ($lang:ident) => {
        mod $lang;
    };
}
all_runnable_langs!(declare_mod);

fn command(cmd: &str) -> Command {
    let mut parts = cmd.split_whitespace();
    let mut command = if cfg!(windows) {
        let mut command = Command::new("cmd");
        command.arg("/C").arg(parts.next().unwrap());
        command
    } else {
        Command::new(parts.next().unwrap())
    };
    for part in parts {
        command.arg(part);
    }
    command
}

trait CommandExt {
    fn run(&mut self) -> anyhow::Result<()>;
}

impl CommandExt for Command {
    fn run(&mut self) -> anyhow::Result<()> {
        let status = self.status().context("Failed to get process status")?;
        if !status.success() {
            anyhow::bail!("Process exited with {}", status);
        }
        Ok(())
    }
}

fn format_duration(duration: std::time::Duration) -> String {
    if duration.as_nanos() < 1000 {
        format!("{} ns", duration.as_nanos())
    } else if duration.as_micros() < 1000 {
        format!("{} us", duration.as_micros())
    } else if duration.as_millis() < 1000 {
        format!("{} ms", duration.as_millis())
    } else {
        format!("{:.1} s", duration.as_secs_f64())
    }
}

#[derive(structopt::StructOpt)]
enum Opt {
    Generate { path: PathBuf },
    Test { langs: Vec<String> },
}

trait Generator: trans_gen::Generator {
    const NAME: &'static str;
    fn generate(path: &Path) -> anyhow::Result<()>;
    fn build_local(path: &Path) -> anyhow::Result<()>;
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()>;
}

fn generate_model<T: trans_gen::Generator>(path: &Path) -> anyhow::Result<()> {
    let mut generator =
        trans_gen::GeneratorImpl::<T>::new("codecraft", "1.0.0", Default::default());
    generator.add(&trans::Schema::of::<model::PlayerView>());
    let result = generator.result();
    result
        .write_to(path)
        .context("Failed to write generated result")?;
    Ok(())
}

fn test<T: Generator>(input: &model::PlayerView) -> anyhow::Result<()> {
    println!("Testing {}", T::NAME);
    let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
    let path = tempdir.as_ref();
    T::generate(path).context("Failed to generate code")?;

    let start_time = std::time::Instant::now();
    T::build_local(path).context("Failed to build locally")?;
    let running_duration = std::time::Instant::now().duration_since(start_time);
    println!("Build duration: {}", format_duration(running_duration));

    let input_file = path.join("input.trans");
    trans::Trans::write_to(
        input,
        &mut std::io::BufWriter::new(
            std::fs::File::create(&input_file).context("Failed to create input file")?,
        ),
    )
    .context("Failed to write input")?;
    let output_file = path.join("output.trans");

    let start_time = std::time::Instant::now();
    T::run_local(path, &input_file, &output_file).context("Failed to run locally")?;
    let running_duration = std::time::Instant::now().duration_since(start_time);
    println!("Run duration: {}", format_duration(running_duration));

    let output: model::PlayerView = trans::Trans::read_from(&mut std::io::BufReader::new(
        std::fs::File::open(&output_file).context("Failed to open output file")?,
    ))
    .context("Failed to read output")?;
    if *input != output {
        anyhow::bail!("Input and output differ");
    }
    println!("Test finished successfully");
    Ok(())
}

fn generate_all(path: &Path) -> anyhow::Result<()> {
    macro_rules! generate {
        ($lang:ident) => {
            generate_model::<trans_gen::gens::$lang::Generator>(&path.join(stringify!($lang)))
                .context(format!("Failed to generate {}", stringify!($lang)))?;
        };
    }
    all_langs!(generate);
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let snapshot: model::PlayerView =
        serde_json::from_str(include_str!("snapshot.json")).expect("Failed to read snapshot");
    let opt: Opt = structopt::StructOpt::from_args();

    match opt {
        Opt::Generate { path } => {
            generate_all(&path)?;
        }
        Opt::Test { langs } => {
            macro_rules! test {
                ($lang:ident) => {
                    if langs.is_empty() || langs.contains(&stringify!($lang).to_owned()) {
                        test::<trans_gen::gens::$lang::Generator>(&snapshot)?;
                    }
                };
            }
            all_runnable_langs!(test);
        }
    }
    Ok(())
}
