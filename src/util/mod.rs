use super::*;

pub fn command(cmd: &str) -> Command {
    let mut parts = cmd.split_whitespace();
    let mut command = if cfg!(windows) {
        let mut command = Command::new("cmd");
        command.arg("/C").arg(parts.next().unwrap());
        command
    } else {
        Command::new(parts.next().unwrap())
    };
    for part in parts {
        command.arg(part);
    }
    command
}

pub trait CommandExt {
    fn run(&mut self, show_output: bool) -> anyhow::Result<()>;
}

impl CommandExt for Command {
    fn run(&mut self, show_output: bool) -> anyhow::Result<()> {
        if !show_output {
            self.stderr(std::process::Stdio::null());
            self.stdout(std::process::Stdio::null());
        }
        let status = self.status().context("Failed to execute process")?;
        if !status.success() {
            anyhow::bail!("Process exited with {}", status);
        }
        Ok(())
    }
}

pub fn format_duration(duration: std::time::Duration) -> String {
    if duration.as_nanos() < 1000 {
        format!("{} ns", duration.as_nanos())
    } else if duration.as_micros() < 1000 {
        format!("{} us", duration.as_micros())
    } else if duration.as_millis() < 1000 {
        format!("{} ms", duration.as_millis())
    } else {
        format!("{:.1} s", duration.as_secs_f64())
    }
}
