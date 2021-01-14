use super::*;

impl Generator for trans_gen::gens::python::Generator {
    const NAME: &'static str = "Python";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        write_file!(path, "main.py")?;
        Ok(())
    }
}

impl RunnableGenerator for trans_gen::gens::python::Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command(if cfg!(windows) { "py -3" } else { "python3" })
            .arg("main.py")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
