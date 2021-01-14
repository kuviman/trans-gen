use super::*;

impl Generator for trans_gen::gens::kotlin::Generator {
    const NAME: &'static str = "Kotlin";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("src").join("main").join("kotlin"))
            .context("Failed to generate model")?;
        let project_name = "codecraft";
        std::fs::write(
            path.join("pom.xml"),
            &include_templing!("examples/codecraft/kotlin/files/pom.xml.templing"),
        )?;
        write_file!(path, "src/main/kotlin/Runner.kt", "Runner.kt")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("mvn")
            .arg("package")
            .arg("--batch-mode")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let project_name = "codecraft";
        command("java")
            .arg("-jar")
            .arg(format!("target/{}-jar-with-dependencies.jar", project_name))
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
