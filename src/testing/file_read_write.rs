use super::*;

pub struct FileReadWrite<D> {
    pub version: Version,
    pub snapshot: D,
    pub show_stdout: bool,
    pub repeat: usize,
}

impl<D: Trans + PartialEq + Debug> Test for FileReadWrite<D> {
    fn schemas(&self) -> Vec<Arc<Schema>> {
        vec![Schema::of::<D>(&self.version)]
    }
    fn run_test(&self, mut run_code: Command) -> anyhow::Result<()> {
        let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
        let path = tempdir.as_ref();
        let input_file = path.join("input.trans");
        trans::Trans::write_to(
            &self.snapshot,
            &mut std::io::BufWriter::new(
                std::fs::File::create(&input_file).context("Failed to create input file")?,
            ),
            &self.version,
        )
        .context("Failed to write input")?;
        let output_file = path.join("output.trans");
        let start_time = std::time::Instant::now();
        run_code
            .arg(&input_file)
            .arg(&output_file)
            .arg(self.repeat.to_string())
            .run(self.show_stdout)
            .context("Failed to run code")?;
        let running_duration = std::time::Instant::now().duration_since(start_time);
        println!("Run duration: {}", format_duration(running_duration));
        let output: D = trans::Trans::read_from(
            &mut std::io::BufReader::new(
                std::fs::File::open(&output_file).context("Failed to open output file")?,
            ),
            &self.version,
        )
        .context("Failed to read output")?;
        if self.snapshot != output {
            anyhow::bail!(
                "Input and output differ: expected {:?}, got {:?}",
                self.snapshot,
                output,
            );
        }
        println!("Test finished successfully");
        Ok(())
    }
}
