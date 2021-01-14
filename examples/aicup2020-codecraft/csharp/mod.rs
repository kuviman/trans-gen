use super::*;

impl Generator for trans_gen::gens::csharp::Generator {
    const NAME: &'static str = "C#";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        write_file!(path, "aicup2020-codecraft.csproj")?;
        write_file!(path, "Runner.cs")?;
        Ok(())
    }
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
            .arg("aicup2020-codecraft.dll")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path)
            .run()
    }
}
