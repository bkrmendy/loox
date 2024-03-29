use std::{cell::RefCell, io::Write, rc::Rc};

use anyhow::bail;
use eval::{EnvPtr, LooxReference};
use parse::Expression;
use rpds::HashTrieMap;

mod eval;
mod parse;
mod report_errors;
mod scan;
mod vm;

fn run(env: EnvPtr, source: &str) -> anyhow::Result<Option<LooxReference<Expression>>> {
    let tokens = scan::scan(source)?;
    let (ast, messages) = parse::parse(&tokens);
    if !messages.errors.is_empty() {
        bail!(format!("{:?}", messages));
    }
    eval::eval(env, ast)
}

fn run_file(file_path: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(file_path)?;
    let tokens = scan::scan(&contents)?;
    let (ast, messages) = parse::parse(&tokens);
    report_errors::report_errors(&messages, file_path)?;

    if !messages.errors.is_empty() {
        return Ok(());
    }
    let env: EnvPtr = Rc::new(RefCell::new(HashTrieMap::new()));
    let _ = eval::eval(env, ast)?;
    Ok(())
}

fn run_prompt() -> anyhow::Result<()> {
    let env: EnvPtr = Rc::new(RefCell::new(HashTrieMap::new()));
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
            Ok(Some(res)) => {
                println!("<| {}", res.borrow());
            }
            Ok(None) => {
                print!("")
            }
            Err(err) => println!("<! {err}"),
        }
    }
}

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
    use std::{cell::RefCell, rc::Rc};

    use rpds::HashTrieMap;

    use crate::{
        eval::EnvPtr,
        parse::parse,
        run, scan,
        vm::{compile::compile_program, compiler::Compiler, machine::Machine},
    };

    fn eval_expr_expect_ok(source: &str) -> String {
        let env: EnvPtr = Rc::new(RefCell::new(HashTrieMap::new()));
        let result = run(env, source).expect("expected to be OK");
        let expr = result.expect("Expected at least one expression");
        format!("{}", expr.borrow())
    }

    fn eval_expr_expect_err(source: &str) -> String {
        let env: EnvPtr = Rc::new(RefCell::new(HashTrieMap::new()));
        let result = run(env, source).unwrap_err();
        format!("{result}")
    }

    fn compile_and_run(source: &str) -> Machine {
        let tokens = scan::scan(source).unwrap();
        let (ast, _) = parse(&tokens);
        let mut compiler = Compiler::new();
        compile_program(&mut compiler, &ast);
        let program = compiler.finish();
        let mut machine = Machine::new(program);
        machine.run();
        machine
    }

    #[test]
    fn test_add_expression() {
        let src = "1 + 2";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""3""###);
    }

    #[test]
    fn test_add_expression_compile_run() {
        let src = "1 + 2";
        let machine = compile_and_run(src);
        insta::assert_debug_snapshot!(machine.peek_stack_top().unwrap(), @r###"
        ImmediateNumber(
            3.0,
        )
        "###);
    }

    #[test]
    fn test_incomplete_input() {
        let src = "1 +";
        let result = eval_expr_expect_err(src);
        insta::assert_debug_snapshot!(result, @r###""Messages { warnings: [], errors: [Message { level: Error, location: SourceRange { start: 3, end: 4 }, message: \"Expected operand\" }] }""###);
    }

    #[test]
    fn test_precedences() {
        let src = "1 + 2 * 4";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""9""###);
    }

    #[test]
    fn test_precedences_vm() {
        let src = "1 + 2 * 4";
        let machine = compile_and_run(src);
        insta::assert_debug_snapshot!(machine.peek_stack_top().unwrap(), @r###"
        ImmediateNumber(
            9.0,
        )
        "###);
    }

    #[test]
    fn test_add_unary() {
        let src = "1 + -2";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""-1""###);
    }

    #[test]
    fn test_precedences_3() {
        let src = "1 + 2 * 4 + 1";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""10""###);
    }

    #[test]
    fn test_precedences_long() {
        let src = "1 + 1 + 2 * 4 + 5 * 6 + 7 * 2 * 2";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""68""###);
    }

    #[test]
    fn test_precedences_long_vm() {
        let src = "1 + 1 + 2 * 4 + 5 * 6 + 7 * 2 * 2";
        let machine = compile_and_run(src);
        insta::assert_debug_snapshot!(machine.peek_stack_top().unwrap(), @r###"
        ImmediateNumber(
            68.0,
        )
        "###);
    }

    #[test]
    fn test_boolean() {
        let src = "true and false";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_boolean_vm() {
        let src = "true and false";
        let machine = compile_and_run(src);
        insta::assert_debug_snapshot!(machine.peek_stack_top().unwrap(), @r###"
        ImmediateBoolean(
            false,
        )
        "###);
    }

    #[test]
    fn test_boolean_with_grouping() {
        let src = "(true and false) or (false or true)";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_boolean_with_grouping_vm() {
        let src = "(true and false) or (false or true)";
        let machine = compile_and_run(src);
        insta::assert_debug_snapshot!(machine.peek_stack_top().unwrap(), @r###"
        ImmediateBoolean(
            true,
        )
        "###);
    }

    #[test]
    fn test_boolean_with_grouping_with_eq() {
        let src = "(true and false) == (false or true)";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_number_eq() {
        let src = "5 == 3";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_number_gt() {
        let src = "5 > 3";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_number_gte() {
        let src = "111 <= 111";
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_variable_decl() {
        let src = r###"
        var a = 3;
        a"###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""3""###);
    }

    #[test]
    fn test_variable_addition() {
        let src = r###"
        var a = 3;
        a + 4"###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""7""###);
    }

    #[test]
    fn test_string_var() {
        let src = r###"
        var a = "hello";
        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""hello""###);
    }

    #[test]
    fn test_string_concat() {
        let src = r###"
        var a = "hello";

        a + "_world"
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""hello_world""###);
    }

    #[test]
    fn test_variables_boolean_op() {
        let src = r###"
        var a = true;
        var b = false;
        a and b"###;
        let result = eval_expr_expect_ok(src);
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
        let result = eval_expr_expect_ok(src);
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
        let result = eval_expr_expect_ok(src);
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
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""27""###);
    }

    #[test]
    fn test_variable_reassignment() {
        let src = r###"
        var a = 1;
        a = 2;
        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""2""###);
    }

    #[test]
    fn test_variable_points_to_other_variable() {
        let src = r###"
        var a = 1;
        var b = a;
        a == b
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_number_variable_value_semantics() {
        let src = r###"
        var a = 1;
        var b = a;
        a = 2;
        a != b
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_string_variable_copy_semantics() {
        let src = r###"
        var a = "hello";
        var b = "abc";
        b = a;
        b
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""hello""###);
    }

    #[test]
    fn test_string_variable_value_semantics() {
        let src = r###"
        var a = "hello";
        var b = "abc";
        b = a;
        a = "not hello anymore";
        a != b
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""true""###);
    }

    #[test]
    fn test_adder() {
        let src = r###"
        fun adder(a) {
            fun add(b) {
                a + b
            }
            add
        }

        var add2 = adder(2);
        add2(3)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""5""###);
    }

    #[test]
    fn test_if_expression() {
        let src = r###"
        var a = -4;
        if a < 0 {
            a = 0;
        }

        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""0""###);
    }

    #[test]
    fn test_if_else_expression() {
        let src = r###"
        var a = 7;
        if a < 0 {
            a = 0;
        } else {
            a = 10;
        }

        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""10""###);
    }

    #[test]
    fn test_min_function() {
        let src = r###"
        fun min(a, b) {
            if a < b {
                a
            } else {
                b
            }
        }

        min(2, 7)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""2""###);
    }

    #[test]
    fn test_greeting() {
        let src = r###"
        fun greet(morning) {
            if morning {
                "Good morning!"
            } else {
                "Hello there!"
            }
        }

        greet(true) == greet(false)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""false""###);
    }

    #[test]
    fn test_fibonacci() {
        let src = r###"
        fun fib(n) {
            if n <= 1 {
                n
            } else {
                fib(n - 2) + fib(n - 1)
            }
        }

        fib(5)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""5""###);
    }

    #[test]
    fn test_simple_while() {
        let src = r###"
        var a = 0;
        while a < 10 {
            a = a + 1;
        }

        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""10""###);
    }

    #[test]
    fn test_while_with_string() {
        let src = r###"
        fun repeat(str, n) {
            var start = "";
            var cnt = 0;
            while cnt < n {
                start = start + str;
                cnt = cnt + 1;
            }
            start
        }

        repeat("woot ", 3)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""woot woot woot ""###);
    }

    #[test]
    fn test_object_literal() {
        let src = r###"
        var a = { hello: 12, world: "aaa" };
        a
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""{hello: 12, world: aaa}""###);
    }

    #[test]
    fn test_property_access_literal() {
        let src = r###"
        var a = { hello: 12, world: "aaa" };
        a.hello
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""12""###);
    }

    #[test]
    fn test_property_access_literal_operand() {
        let src = r###"
        var a = { hello: 12, there: 2 };
        fun double(n) { n * 2 }
        a.there + double(a.hello)
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""26""###);
    }

    #[test]
    fn test_property_access_literal_assignment() {
        let src = r###"
        var a = { hello: 12, there: 2 };
        a.there = 22;
        a.there
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""22""###);
    }

    #[test]
    fn test_property_access_pointer() {
        let src = r###"
        var a = { hello: 12, there: 2 };
        var b = a;
        a.there = 22;
        b.there
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""22""###);
    }

    #[test]
    fn test_closure_aliasing() {
        let src = r###"
        var x = "global";
        fun outer() {
            var x = "outer";
            fun inner() {
                x
            }
            inner();
        }
        outer()
        "###;
        let result = eval_expr_expect_ok(src);
        insta::assert_debug_snapshot!(result, @r###""outer""###);
    }
}
