use super::*;

impl Generator for trans_gen::gens::typescript::Generator {
    const NAME: &'static str = "TypeScript";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(&path.join("src")).context("Failed to generate model")?;
        let project_name = "codecraft";
        let project_version = env!("CARGO_PKG_VERSION");
        std::fs::write(
            path.join("package.json"),
            &include_templing!("examples/codecraft/typescript/files/package.json.templing"),
        )?;
        write_file!(path, "src/main.ts", "main.ts")?;
        write_file!(path, "tsconfig.json")?;
        Ok(())
    }
}

impl RunnableGenerator for trans_gen::gens::typescript::Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("npm").arg("install").current_dir(path).run()?;
        command("npm")
            .arg("run")
            .arg("build")
            .current_dir(path)
            .run()?;
        Ok(())
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        command("node")
            .arg("main.js")
            .arg(input_file)
            .arg(output_file)
            .current_dir(path.join("build"))
            .run()
    }
}
