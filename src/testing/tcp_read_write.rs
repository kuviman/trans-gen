use super::*;

pub struct TcpReadWrite<D> {
    pub version: Version,
    pub snapshot: D,
    pub repeat: usize,
}

impl<D: Trans + PartialEq + Debug> Test for TcpReadWrite<D> {
    fn schemas(&self) -> Vec<Arc<Schema>> {
        vec![Schema::of::<D>(&self.version)]
    }
    fn run_test(&self, mut run_code: Command, verbose: bool) -> anyhow::Result<TestRunResult> {
        use std::io::Write as _;
        let show_stdout =
            format!("{:?}", self.snapshot).len() < 1000 && verbose && self.repeat == 1;
        let port: u16 = 31001;
        let listener =
            std::net::TcpListener::bind(("127.0.0.1", port)).context("Failed to bind tcp port")?;
        listener.set_nonblocking(true)?;
        let mut child = run_code
            .arg("127.0.0.1")
            .arg(port.to_string())
            .arg(show_stdout.to_string())
            .show_stdout(show_stdout)
            .spawn()
            .context("Failed to spawn code")?;
        let mut accept_try = 0;
        let (tcp_stream, _) = loop {
            match listener.accept() {
                Ok(result) => break result,
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    accept_try += 1;
                    if accept_try > 100 {
                        anyhow::bail!("Timeout accepting connection");
                    }
                    std::thread::sleep(std::time::Duration::from_millis(100));
                }
                Err(e) => {
                    return Err(e).context("Failed to accept connection")?;
                }
            }
        };
        tcp_stream.set_nonblocking(false)?;
        tcp_stream.set_nodelay(true)?;
        tcp_stream.set_read_timeout(Some(std::time::Duration::from_millis(1000)))?;
        tcp_stream.set_write_timeout(Some(std::time::Duration::from_millis(1000)))?;
        let mut input_stream = std::io::BufReader::new(tcp_stream.try_clone().unwrap());
        let mut output_stream = std::io::BufWriter::new(tcp_stream.try_clone().unwrap());
        let start_time = std::time::Instant::now();
        for index in 0..self.repeat {
            || -> anyhow::Result<()> {
                Trans::write_to(&true, &mut output_stream, &self.version)
                    .context("Failed to write 'true'")?;
                self.snapshot
                    .write_to(&mut output_stream, &self.version)
                    .context("Failed to write snapshot")?;
                output_stream.flush().context("Failed to flush")?;
                let output: D = Trans::read_from(&mut input_stream, &self.version)
                    .context("Failed to read output")?;
                if self.snapshot != output {
                    anyhow::bail!(
                        "Input and output differ: expected {:?}, got {:?}",
                        self.snapshot,
                        output,
                    );
                }
                Ok(())
            }()
            .context(format!("Failed at iteration {:?}", index))?;
        }
        Trans::write_to(&false, &mut output_stream, &self.version)
            .context("Failed to write 'false'")?;
        output_stream.flush().context("Failed to flush")?;
        let child_status = child.wait().context("Failed to wait for child process")?;
        if !child_status.success() {
            anyhow::bail!("Child process exited with {}", child_status);
        }
        let run_duration = std::time::Instant::now().duration_since(start_time);
        println!(
            "Run duration: {} ({} avg)",
            format_duration(run_duration),
            format_duration(std::time::Duration::from_secs_f64(
                run_duration.as_secs_f64() / self.repeat as f64
            )),
        );
        println!("Test finished successfully");
        Ok(TestRunResult { run_duration })
    }
}
