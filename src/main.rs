use std::io::Write;

use anyhow::Ok;
use parse::Expression;

mod eval;
mod parse;
mod scan;
mod utils;

fn run(source: &str) -> anyhow::Result<Expression> {
    scan::scan(source)
        .and_then(|tokens| parse::parse(&tokens))
        .and_then(eval::eval)
}

fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(path)?;
    let _ = run(&contents);
    Ok(())
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
        let result = run(&buffer)?;
        println!("< {result}");
    }
}

// TODO

// use nom for scanning (for the line number + offset)
// add the goodness from https://eyalkalderon.com/blog/nom-error-recovery/

// parse/eval variable declarations
// parse/eval functions
// parse/eval ifs
// parse/eval fors, whiles
// parse/eval scopes

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

#[cfg(test)]
mod tests {
    use crate::run;

    #[test]
    fn test_add_expression() {
        let src = "1 + 2";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                Number(
                    3.0,
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_precedences() {
        let src = "1 + 2 * 4";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                Number(
                    9.0,
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_boolean() {
        let src = "true and false";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                False,
            ),
        )
        "###);
    }

    #[test]
    fn test_boolean_with_grouping() {
        let src = "(true and false) or (false or true)";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                True,
            ),
        )
        "###);
    }

    #[test]
    fn test_boolean_with_grouping_with_eq() {
        let src = "(true and false) == (false or true)";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                False,
            ),
        )
        "###);
    }

    #[test]
    fn test_number_eq() {
        let src = "5 == 3";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                False,
            ),
        )
        "###);
    }

    #[test]
    fn test_number_gt() {
        let src = "5 > 3";
        let result = run(src);
        insta::assert_debug_snapshot!(result, @r###"
        Ok(
            Literal(
                True,
            ),
        )
        "###);
    }
}
