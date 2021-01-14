use super::*;

impl Generator for trans_gen::gens::javascript::Generator {
    const NAME: &'static str = "JavaScript";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        write_file!(path, "main.js")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command("node")
            .arg("main.js")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
