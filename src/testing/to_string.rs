use super::*;

pub struct ToString<D> {
    pub snapshot: D,
}

impl<D: Trans + Debug> Test for ToString<D> {
    fn schemas(&self) -> Vec<Arc<Schema>> {
        vec![Schema::of::<D>()]
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
        )
        .context("Failed to write input")?;
        let start_time = std::time::Instant::now();
        let output = run_code
            .arg(&input_file)
            .stdout(std::process::Stdio::piped())
            .output()
            .context("Failed to run code")?;
        let running_duration = std::time::Instant::now().duration_since(start_time);
        if !output.status.success() {
            anyhow::bail!("Failed to run code: {}", output.status);
        }
        println!("Run duration: {}", format_duration(running_duration));
        let output = String::from_utf8(output.stdout).context("Output is not UTF-8")?;
        let expected = format!("{:#?}", self.snapshot);
        if expected != output {
            anyhow::bail!(
                "Input and output differ:\nExpected:\n{}\nOutput\n:{}",
                expected,
                output,
            );
        }
        println!("Test finished successfully");
        Ok(())
    }
}
