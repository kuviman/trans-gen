use super::*;

impl Generator for trans_gen::gens::cpp::Generator {
    const NAME: &'static str = "C++";
    fn generate(path: &Path) -> anyhow::Result<()> {
        generate_model::<Self>(path).context("Failed to generate model")?;
        let project_name = "codecraft";
        std::fs::write(
            path.join("CMakeLists.txt"),
            &include_templing!("examples/codecraft/cpp/files/CMakeLists.txt.templing"),
        )?;
        write_file!(path, "main.cpp")?;
        Ok(())
    }
}

impl RunnableGenerator for trans_gen::gens::cpp::Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        let standard: &str = "17";
        command("cmake")
            .current_dir(path)
            .arg(format!("-DCMAKE_CXX_STANDARD={}", standard))
            .arg("-DCMAKE_BUILD_TYPE=RELEASE")
            .arg("-DCMAKE_VERBOSE_MAKEFILE=ON")
            .arg(".")
            .run()?;
        command("cmake")
            .current_dir(path)
            .arg("--build")
            .arg(".")
            .arg("--config")
            .arg("Release")
            .run()?;
        Ok(())
    }
    fn run_local(path: &Path, input_file: &Path, output_file: &Path) -> anyhow::Result<()> {
        let project_name = "codecraft";
        command(
            PathBuf::from(if cfg!(windows) { "Release" } else { "." })
                .join(format!(
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
