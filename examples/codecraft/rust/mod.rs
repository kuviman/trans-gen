use super::*;

impl Generator for trans_gen::gens::rust::Generator {
    const NAME: &'static str = "Rust";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("model")).context("Failed to generate model")?;
        let crate_name = "codecraft";
        let crate_version = env!("CARGO_PKG_VERSION");
        let model_crate_name = format!("{}-model", crate_name);
        std::fs::write(
            path.join("Cargo.toml"),
            &include_templing!("examples/codecraft/rust/files/Cargo.toml.templing"),
        )?;
        write_file!(path, "src/main.rs", "main.rs")?;
        Ok(())
    }
}

impl RunnableGenerator for trans_gen::gens::rust::Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("cargo")
            .arg("build")
            .arg("--release")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let crate_name = "codecraft";
        command(
            path.join("target")
                .join("release")
                .join(format!(
                    "{}{}",
                    crate_name,
                    if cfg!(windows) { ".exe" } else { "" }
                ))
                .to_str()
                .unwrap(),
        )
        .arg(input_file)
        .arg(output_file)
        .current_dir(path)
        .run()
    }
}
