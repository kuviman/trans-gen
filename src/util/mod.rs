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
    fn run(&mut self) -> anyhow::Result<()>;
}

impl CommandExt for Command {
    fn run(&mut self) -> anyhow::Result<()> {
        let status = self.status().context("Failed to get process status")?;
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

pub struct Writer {
    content: String,
    ident_level: usize,
}

impl Writer {
    pub fn new() -> Self {
        Self {
            content: String::new(),
            ident_level: 0,
        }
    }
    pub fn inc_ident(&mut self) {
        self.ident_level += 1;
    }
    pub fn dec_ident(&mut self) {
        self.ident_level -= 1;
    }
    pub fn get(self) -> String {
        assert_eq!(self.ident_level, 0, "Incorrect indentation");
        self.content
    }
}

impl std::fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            if c != '\n' && self.content.chars().last().unwrap_or('\n') == '\n' {
                for _ in 0..self.ident_level * 4 {
                    self.content.push(' ');
                }
            }
            self.content.push(c);
        }
        Ok(())
    }
}
