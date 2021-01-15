use super::*;

pub trait FileReadWrite: RunnableGenerator {
    fn extra_files(schema: &Schema) -> Vec<File>;
}

pub fn generate_file_read_write<T: Trans + PartialEq, G: FileReadWrite>(
    path: &Path,
) -> anyhow::Result<()> {
    generate::<G>(
        "trans-gen-test",
        env!("CARGO_PKG_VERSION"),
        Default::default(),
        &[Schema::of::<T>()],
        <G as FileReadWrite>::extra_files(&Schema::of::<T>()),
    )
    .write_to(path)
    .context("Failed to write generated code")?;
    Ok(())
}

pub fn test_file_read_write<T: Trans + PartialEq, G: FileReadWrite>(
    input: &T,
) -> anyhow::Result<()> {
    println!("Testing {}", G::NAME);
    let tempdir = tempfile::tempdir().context("Failed to create temp dir")?;
    let path = tempdir.as_ref();
    generate_file_read_write::<T, G>(path)?;

    let start_time = std::time::Instant::now();
    G::build_local(path).context("Failed to build locally")?;
    let running_duration = std::time::Instant::now().duration_since(start_time);
    println!("Build duration: {}", format_duration(running_duration));

    let input_file = path.join("input.trans");
    trans::Trans::write_to(
        input,
        &mut std::io::BufWriter::new(
            std::fs::File::create(&input_file).context("Failed to create input file")?,
        ),
    )
    .context("Failed to write input")?;
    let output_file = path.join("output.trans");

    let start_time = std::time::Instant::now();
    G::run_local(path)
        .context("Failed to get local run command")?
        .arg(&input_file)
        .arg(&output_file)
        .run()
        .context("Failed to run locally")?;
    let running_duration = std::time::Instant::now().duration_since(start_time);
    println!("Run duration: {}", format_duration(running_duration));

    let output: T = trans::Trans::read_from(&mut std::io::BufReader::new(
        std::fs::File::open(&output_file).context("Failed to open output file")?,
    ))
    .context("Failed to read output")?;
    if *input != output {
        anyhow::bail!("Input and output differ");
    }
    println!("Test finished successfully");
    Ok(())
}
