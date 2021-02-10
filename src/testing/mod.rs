use super::*;

mod file_read_write;
mod tcp_read_write;

pub use file_read_write::FileReadWrite;
pub use tcp_read_write::TcpReadWrite;

#[derive(Debug, Serialize, Deserialize)]
pub struct TestRunResult {
    run_duration: std::time::Duration,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestResult {
    build_duration: std::time::Duration,
    run: TestRunResult,
}

impl TestResult {
    fn fake() -> Self {
        Self {
            build_duration: std::time::Duration::from_millis(0),
            run: TestRunResult {
                run_duration: std::time::Duration::from_millis(0),
            },
        }
    }
}

pub trait Test {
    fn schemas(&self) -> Vec<Arc<Schema>>;
    fn run_test(&self, run_code: Command, verbose: bool) -> anyhow::Result<TestRunResult>;
}

pub trait TestExt: Test {
    fn generate<G: TestableGenerator<Self>>(&self, path: &Path) -> anyhow::Result<()>;
    fn test<G: TestableGenerator<Self>>(&self, verbose: bool) -> anyhow::Result<TestResult>;
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
    fn test<G: TestableGenerator<Self>>(&self, verbose: bool) -> anyhow::Result<TestResult> {
        if !G::is_runnable() {
            return Ok(TestResult::fake());
        }
        let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
        let path = tempdir.as_ref();
        self.generate::<G>(path)?;
        let build_start_time = std::time::Instant::now();
        G::build_local(path, verbose).context("Failed to build locally")?;
        let build_duration = std::time::Instant::now().duration_since(build_start_time);
        println!("Build duration: {}", format_duration(build_duration));
        let run_result = self
            .run_test(
                G::run_local(path).context("Failed to figure out running command")?,
                verbose,
            )
            .context("Failed to run test")?;
        Ok(TestResult {
            build_duration,
            run: run_result,
        })
    }
}

pub trait TestableGenerator<T: Test + ?Sized>: RunnableGenerator {
    fn extra_files(test: &T) -> Vec<File>;
}
