use super::*;

impl Generator for trans_gen::gens::ruby::Generator {
    const NAME: &'static str = "Ruby";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        write_file!(path, "main.rb")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command("ruby")
            .arg("main.rb")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
