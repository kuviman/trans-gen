use super::*;

impl Generator for trans_gen::gens::fsharp::Generator {
    const NAME: &'static str = "F#";
    fn generate(path: &Path) -> anyhow::Result<()> {
        let mut source_files = generate_model::<Self>(path).context("Failed to generate model")?;
        write_file!(path, "Runner.fs")?;
        source_files.push("Runner.fs".to_owned());
        std::fs::write(
            path.join("codecraft.fsproj"),
            &include_templing!("examples/codecraft/fsharp/files/project.fsproj.templing"),
        )?;
        Ok(())
    }
}

impl RunnableGenerator for trans_gen::gens::fsharp::Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("dotnet")
            .current_dir(path)
            .arg("publish")
            .arg("-c")
            .arg("Release")
            .arg("-o")
            .arg(".")
            .run()
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command("dotnet")
            .arg("codecraft.dll")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
