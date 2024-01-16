use std::io::Write;

use eval::{Environment, LooxReference};
use parse::Expression;
use rpds::HashTrieMap;

mod eval;
mod parse;
mod scan;
mod utils;

fn run(
    env: Environment,
    source: &str,
) -> anyhow::Result<(Environment, Option<LooxReference<Expression>>)> {
    let tokens = scan::scan(source)?;
    let ast = parse::parse(&tokens).expect("should always succeed");
    eval::eval(env, ast)
}

fn run_file(path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(path)?;
    let env: Environment = HashTrieMap::new();
    let _ = run(env, &contents);
    Ok(())
}

fn run_prompt() -> anyhow::Result<()> {
    let mut env: Environment = HashTrieMap::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;
        if buffer.is_empty() {
            return Ok(());
        }
        let result = run(env.clone(), &buffer);
        match result {
            Ok((next_env, Some(res))) => {
                env = next_env;
                println!("<| {}", res.borrow())
            }
            Ok((next_env, None)) => {
                env = next_env;
                print!("")
            }
            Err(err) => println!("<! {err}"),
        }
    }
}

// TODO

// propagate errors from parsing

// parse/eval ifs
// parse/eval fors, whiles

// parse/eval objects
// make sure only objects have reference semantics
// make sure numbers have value semantics
// parse/eval scopes

// add support for function literals
// add support for return statements

// use nom for scanning (for the line number + offset)

// type system
// VSCode extension

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
    use rpds::HashTrieMap;

    use crate::{eval::Environment, run};

    fn run_expr_expect_ok(source: &str) -> String {
        let env: Environment = HashTrieMap::new();
        let (_, result) = run(env, source).expect("expected to be OK");
        let expr = result.expect("Expected at least one expression");
        format!("{}", expr.borrow())
    }

    fn run_expr_expect_err(source: &str) -> String {
        let env: Environment = HashTrieMap::new();
        let result = run(env, source).unwrap_err();
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

    #[test]
    fn test_variable_decl() {
        let src = r###"
        var a = 3;
        a"###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""3""###);
    }

    #[test]
    fn test_variable_addition() {
        let src = r###"
        var a = 3;
        a + 4"###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""7""###);
    }

    #[test]
    fn test_variables_boolean_op() {
        let src = r###"
        var a = true;
        var b = false;
        a and b"###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_simple_function() {
        let src = r###"
        fun add(a, b) {
            a + b
        }
        add(11, 22)
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""33""###);
    }

    #[test]
    fn test_function_with_var_decl() {
        let src = r###"
        fun add3(n) {
            var c = 3;
            c + n;
        }
        add3(22)
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""25""###);
    }

    #[test]
    fn test_function_with_binary_op() {
        let src = r###"
        fun add3(n) {
            var c = 3;
            c + n;
        }
        add3(22) + 2
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""27""###);
    }

    #[test]
    fn test_variable_reassignment() {
        let src = r###"
        var a = 1;
        a = 2;
        a
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""2""###);
    }

    #[test]
    fn test_variable_points_to_other_variable() {
        let src = r###"
        var a = 1;
        var b = a;
        a == b
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_variable_points_to_reassignment() {
        let src = r###"
        var a = 1;
        var b = a;
        a = 2;
        a == b
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_adder() {
        let src = r###"
        fun adder(a) {
            fun add(b) {
                a + b
            }
        }

        var add2 = adder(2)
        add2(3)
        "###;
        let result = run_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""5""###);
    }
}
