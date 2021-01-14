use super::*;

impl Generator for trans_gen::gens::dlang::Generator {
    const NAME: &'static str = "D";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("source")).context("Failed to generate model")?;
        let project_name = "codecraft";
        std::fs::write(
            path.join("dub.json"),
            &include_templing!("examples/codecraft/dlang/files/dub.json.templing"),
        )?;
        write_file!(path, "source/app.d", "app.d")?;
        Ok(())
    }
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("dub")
            .arg("build")
            .arg("-b")
            .arg("release")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let project_name = "codecraft";
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
