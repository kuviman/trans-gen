use super::*;

mod file_read_write;

pub use file_read_write::FileReadWrite;

pub trait Test {
    fn schemas(&self) -> Vec<Arc<Schema>>;
    fn run_test(&self, run_code: Command) -> anyhow::Result<()>;
}

pub trait TestExt: Test {
    fn generate<G: TestableGenerator<Self>>(&self, path: &Path) -> anyhow::Result<()>;
    fn test<G: TestableGenerator<Self>>(&self) -> anyhow::Result<()>;
}

impl<T: Test> TestExt for T {
    fn generate<G: TestableGenerator<Self>>(&self, path: &Path) -> anyhow::Result<()> {
        generate::<G>(
            "trans-gen-test",
            env!("CARGO_PKG_VERSION"),
            Default::default(),
            &self.schemas(),
            G::extra_files(self),
        )
        .write_to(path)
        .context("Failed to write generated code")?;
        Ok(())
    }
    fn test<G: TestableGenerator<Self>>(&self) -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
        let path = tempdir.as_ref();
        self.generate::<G>(path)?;
        let start_time = std::time::Instant::now();
        G::build_local(path).context("Failed to build locally")?;
        let running_duration = std::time::Instant::now().duration_since(start_time);
        println!("Build duration: {}", format_duration(running_duration));
        self.run_test(G::run_local(path).context("Failed to figure out running command")?)
            .context("Failed to run test")?;
        Ok(())
    }
}

pub trait TestableGenerator<T: Test + ?Sized>: RunnableGenerator {
    fn extra_files(test: &T) -> Vec<File>;
}
