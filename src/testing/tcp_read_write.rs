use super::*;

pub struct TcpReadWrite<D> {
    pub version: Version,
    pub snapshot: D,
    pub show_stdout: bool,
}

impl<D: Trans + PartialEq + Debug> Test for TcpReadWrite<D> {
    fn schemas(&self) -> Vec<Arc<Schema>> {
        vec![Schema::of::<D>(&self.version)]
    }
    fn run_test(&self, mut run_code: Command) -> anyhow::Result<()> {
        let start_time = std::time::Instant::now();
        if !self.show_stdout {
            run_code.stdout(std::process::Stdio::null());
        }
        let port: u16 = 31001;
        let listener =
            std::net::TcpListener::bind(("127.0.0.1", port)).context("Failed to bind tcp port")?;
        listener.set_nonblocking(true)?;
        let mut child = run_code
            .arg("127.0.0.1")
            .arg(port.to_string())
            .spawn()
            .context("Failed to spawn code")?;
        let mut accept_try = 0;
        let (mut tcp_stream, _) = loop {
            match listener.accept() {
                Ok(result) => break result,
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    accept_try += 1;
                    if accept_try > 10 {
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
        self.snapshot
            .write_to(&mut tcp_stream, &self.version)
            .context("Failed to write snapshot")?;
        let output: D =
            Trans::read_from(&mut tcp_stream, &self.version).context("Failed to read output")?;
        if self.snapshot != output {
            anyhow::bail!(
                "Input and output differ: expected {:?}, got {:?}",
                self.snapshot,
                output,
            );
        }
        let child_status = child.wait().context("Failed to wait for child process")?;
        if !child_status.success() {
            anyhow::bail!("Child process exited with {}", child_status);
        }
        let running_duration = std::time::Instant::now().duration_since(start_time);
        println!("Run duration: {}", format_duration(running_duration));
        println!("Test finished successfully");
        Ok(())
    }
}
