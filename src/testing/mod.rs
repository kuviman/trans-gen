use super::*;

mod file_read_write;
mod tcp_read_write;

pub use file_read_write::FileReadWrite;
pub use tcp_read_write::TcpReadWrite;

#[derive(Debug, Serialize, Deserialize)]
pub struct TestRunResult {
    pub run_duration: std::time::Duration,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestResult {
    pub build_duration: std::time::Duration,
    pub run: TestRunResult,
}

impl TestResult {
    pub fn fake() -> Self {
        Self {
            build_duration: std::time::Duration::from_millis(0),
            run: TestRunResult {
                run_duration: std::time::Duration::from_millis(0),
            },
        }
    }
    pub fn into_average(self, n: usize) -> Self {
        Self {
            build_duration: std::time::Duration::from_secs_f64(
                self.build_duration.as_secs_f64() / n as f64,
            ),
            run: TestRunResult {
                run_duration: std::time::Duration::from_secs_f64(
                    self.run.run_duration.as_secs_f64() / n as f64,
                ),
            },
        }
    }
}

pub trait Test {
    fn name(&self) -> String;
    fn version(&self) -> Version;
    fn schemas(&self) -> Vec<Arc<Schema>>;
    fn run_test(&self, run_code: Command, verbose: bool) -> anyhow::Result<TestRunResult>;
}

pub trait TestExt: Test {
    fn generate<G: TestableGenerator<Self>>(
        &self,
        path: &Path,
        options: G::Options,
    ) -> anyhow::Result<()>;
    fn test<G: TestableGenerator<Self>>(
        &self,
        path: Option<&Path>,
        options: G::Options,
        verbose: bool,
    ) -> anyhow::Result<TestResult>;
}

impl<T: Test> TestExt for T {
    fn generate<G: TestableGenerator<Self>>(
        &self,
        path: &Path,
        options: G::Options,
    ) -> anyhow::Result<()> {
        generate::<G>(
            &self.name(),
            &self.version().to_string(),
            options,
            &self.schemas(),
            &|generator| G::extra_files(generator, self),
        )
        .write_to(path)
        .context("Failed to write generated code")?;
        Ok(())
    }
    fn test<G: TestableGenerator<Self>>(
        &self,
        path: Option<&Path>,
        options: G::Options,
        verbose: bool,
    ) -> anyhow::Result<TestResult> {
        if !G::is_runnable() {
            return Ok(TestResult::fake());
        }
        let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
        let path = match path {
            Some(path) => path,
            None => {
                let path = tempdir.as_ref();
                self.generate::<G>(path, options)?;
                path
            }
        };
        let path = std::fs::canonicalize(path).context("Failed to canonicalize path")?;
        let path = path.to_str().unwrap();
        let path: &Path = path.strip_prefix("\\\\?\\").unwrap_or(path).as_ref();
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
    fn extra_files(&self, test: &T) -> Vec<File>;
}
