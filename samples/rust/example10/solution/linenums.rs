//OUT output.txt
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io::{BufWriter, Write};

fn main() -> Result<(), std::io::Error> {
    let infile = File::open("input.txt")?;
    let outfile = File::create("output.txt")?;
    let mut reader = BufReader::new(infile);
    let mut writer = BufWriter::new(outfile);

    let mut line = String::new();
    let mut num = 0;
    loop {
        num += 1;
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 {
            break;
        }
        line = line.trim().to_string();
        line = format!("   {}: {}\n", num, line);
        writer.write_all(line.as_bytes());
        line.clear();
    }

    Ok(())
}