use super::*;

impl Generator for trans_gen::gens::go::Generator {
    const NAME: &'static str = "Go";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        let project_name = "aicup2020-codecraft-model";
        std::fs::write(
            path.join("go.mod"),
            &include_templing!("examples/aicup2020-codecraft/go/files/go.mod.templing"),
        )?;
        write_file!(path, "main.go")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        let project_name = "aicup2020-codecraft-model";
        command("go")
            .arg("build")
            .arg("-o")
            .arg(format!(
                "{}{}",
                project_name,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let project_name = "aicup2020-codecraft-model";
        command(
            path.join(format!(
                "{}{}",
                project_name,
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
