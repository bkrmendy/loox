use std::io::Write;

use anyhow::Ok;

mod eval;
mod parse;
mod scan;
mod utils;

fn run(source: &str) -> anyhow::Result<()> {
    let tokens = scan::scan(source)?;
    let expr = parse::parse(&tokens)?;
    let result = eval::eval(expr)?;
    println!("< {result}");
    Ok(())
}

fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(path)?;
    run(&contents)
}

fn run_prompt() -> anyhow::Result<()> {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;
        if buffer.is_empty() {
            return Ok(());
        }
        run(&buffer)?;
    }
}

// TODO
// tests
// use nom for scanning (for the line number + offset)
// add the goodness from https://eyalkalderon.com/blog/nom-error-recovery/

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        println!("Usage: loox [script]");
        std::process::exit(65);
    }

    if args.len() == 2 {
        run_file(args.get(1).unwrap())?;
    } else {
        run_prompt()?;
    }

    Ok(())
}
