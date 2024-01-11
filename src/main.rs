use std::io::Write;

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
        let result = run(&buffer);
        match result {
            Ok(res) => println!("<| {res}"),
            Err(err) => println!("<! {err}"),
        }
    }
}

// TODO

// add the goodness from https://eyalkalderon.com/blog/nom-error-recovery/
// parse/eval variable declarations
// parse/eval functions (incl. persistent map for env

// use nom for scanning (for the line number + offset)
// parse/eval ifs
// parse/eval fors, whiles

// parse/eval objects
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

    fn run_expr_expect_ok(source: &str) -> String {
        let result = run(source).expect("expected to be OK");
        format!("{result}")
    }

    fn run_expr_expect_err(source: &str) -> String {
        let result = run(source).unwrap_err();
        format!("{result}")
    }

    #[test]
    fn test_add_expression() {
        let src = "1 + 2";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""3""###);
    }

    #[test]
    fn test_incomplete_input() {
        let src = "1 +";
        let result = run_expr_expect_err(src);
        insta::assert_debug_snapshot!(result, @r###""Expected operand""###);
    }

    #[test]
    fn test_precedences() {
        let src = "1 + 2 * 4";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""9""###);
    }

    #[test]
    fn test_add_unary() {
        let src = "1 + -2";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""-1""###);
    }

    #[test]
    fn test_precedences_3() {
        let src = "1 + 2 * 4 + 1";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""10""###);
    }

    #[test]
    fn test_precedences_long() {
        let src = "1 + 1 + 2 * 4 + 5 * 6 + 7 * 2 * 2";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""68""###);
    }

    #[test]
    fn test_boolean() {
        let src = "true and false";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_boolean_with_grouping() {
        let src = "(true and false) or (false or true)";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_boolean_with_grouping_with_eq() {
        let src = "(true and false) == (false or true)";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_number_eq() {
        let src = "5 == 3";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_number_gt() {
        let src = "5 > 3";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_number_gte() {
        let src = "111 <= 111";
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }
}
