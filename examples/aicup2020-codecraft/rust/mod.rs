use super::*;

impl Generator for trans_gen::gens::rust::Generator {
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("model")).context("Failed to generate model")?;
        let crate_name = "aicup2020-codecraft";
        let crate_version = env!("CARGO_PKG_VERSION");
        let model_crate_name = format!("{}-model", crate_name);
        std::fs::write(
            path.join("Cargo.toml"),
            &include_templing!("examples/aicup2020-codecraft/rust/files/Cargo.toml.templing"),
        )?;
        write_file!(path, "src/main.rs", "main.rs")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("cargo")
            .arg("build")
            .arg("--release")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command("cargo")
            .arg("run")
            .arg("--release")
            .arg("--")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
