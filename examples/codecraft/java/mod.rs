use super::*;

impl Generator for trans_gen::gens::java::Generator {
    const NAME: &'static str = "Java";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("src").join("main").join("java"))
            .context("Failed to generate model")?;
        let project_name = "codecraft";
        std::fs::write(
            path.join("pom.xml"),
            &include_templing!("examples/codecraft/java/files/pom.xml.templing"),
        )?;
        write_file!(path, "src/main/java/Runner.java", "Runner.java")?;
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
