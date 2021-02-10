use anyhow::Context;

use trans_gen_test::*;

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args();
    args.next().unwrap();
    let input_file = args
        .next()
        .context("Provide input file path as first arg")?;
    let output_file = args
        .next()
        .context("Provide output file path as second arg")?;
    let repeat = args.next().context("Provide repeat times as third arg")?.parse().context("Failed to parse repeat")?;

    for _ in 0..repeat {
        let input: codegame::MessageGameModel = trans::Trans::read_from(&mut std::io::BufReader::new(
            std::fs::File::open(&input_file).context("Failed to open input file")?,
        ))
        .context("Failed to read input")?;
        if repeat == 1 {
            println!("{:?}", input);
        }
        trans::Trans::write_to(
            &input,
            &mut std::io::BufWriter::new(
                std::fs::File::create(&output_file).context("Failed to create output file")?,
            ),
        )
        .context("Failed to write output")?;
    }

    Ok(())
}