use std::cell::RefCell;
use std::io::sink;
use std::rc::Rc;

macro_rules! e2e {
    ($src:literal, $exp:expr, $name:ident) => {
        #[test]
        fn $name() {
            let src = $src;
            let exp = $exp;
            let mut lexer = rpinky::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().unwrap();
            let ast = rpinky::parser::Parser::new(tokens.to_vec()).expr().unwrap();
            let result = rpinky::interpreter::expr(&ast, Rc::new(RefCell::new(sink()))).unwrap();
            assert_eq!(result, rpinky::interpreter::Outcome::Done(exp));
        }
    };
}

macro_rules! e2e_runtime_error {
    ($src:literal, $msg:literal, $name:ident) => {
        #[test]
        fn $name() {
            let mut lexer = rpinky::lexer::Lexer::new($src);
            let tokens = lexer.tokenize().unwrap();
            let ast = rpinky::parser::Parser::new(tokens.to_vec()).expr().unwrap();
            let err = rpinky::interpreter::expr(&ast, Rc::new(RefCell::new(sink()))).unwrap_err();
            assert_eq!(err.message(), $msg);
        }
    };
}

e2e!(b"7.7", rpinky::interpreter::Type::Number(7.7), number_primary);
e2e!(b"false", rpinky::interpreter::Type::Bool(false), bool_primary);
e2e!(b"2 + 2", rpinky::interpreter::Type::Number(4.0), add);
e2e!(b"2 * 9", rpinky::interpreter::Type::Number(18.0), mul);
e2e!(b"9 / 2", rpinky::interpreter::Type::Number(4.5), div);
e2e!(b"2 * 9 + 13", rpinky::interpreter::Type::Number(31.0), precedence);
e2e!(b"2 * 9 + - -5", rpinky::interpreter::Type::Number(23.0), unary_minus);
e2e!(b"2 ^ 3 ^ 3 - 1", rpinky::interpreter::Type::Number(134217727.0), exponent);
e2e!(b"(2 ^ 3 ^ 3 - 1) % 2", rpinky::interpreter::Type::Number(1.0), rem);
e2e!(b"2 * (9 + 13) / 2", rpinky::interpreter::Type::Number(22.0), paren_1);
e2e!(
    b"2 * (9 + 13) + 2 ^ 2 + (((3 * 3) - 3) + 3.324) / 2.1",
    rpinky::interpreter::Type::Number(52.44),
    paren_2
);
e2e!(b"14 / (12 / 2) / 2", rpinky::interpreter::Type::Number(1.1666666666666667), paren_3);
e2e!(b"(44 >= 2) or false and 1 > 0", rpinky::interpreter::Type::Bool(true), bool_or);
e2e!(b"~(44 >= 2)", rpinky::interpreter::Type::Bool(false), bool_not);
e2e!(b"~(3 ~= 2)", rpinky::interpreter::Type::Bool(false), noteq);
e2e!(b"(3 == 2 + 1)", rpinky::interpreter::Type::Bool(true), eqeq);
e2e!(b"-2 ^ 3", rpinky::interpreter::Type::Number(-8.0), exponent_unary_minus);
e2e!(b"~''", rpinky::interpreter::Type::Bool(true), not_string);
e2e!(b"~0", rpinky::interpreter::Type::Bool(true), not_num);
e2e!(b"10 - 3", rpinky::interpreter::Type::Number(7.0), sub);
e2e!(b"5 - 10", rpinky::interpreter::Type::Number(-5.0), sub_negative_result);
e2e!(b"0 - 0", rpinky::interpreter::Type::Number(0.0), sub_zero);
e2e!(b"3 < 5", rpinky::interpreter::Type::Bool(true), less_than_true);
e2e!(b"5 < 3", rpinky::interpreter::Type::Bool(false), less_than_false);
e2e!(b"3 <= 3", rpinky::interpreter::Type::Bool(true), less_eq_equal);
e2e!(b"3 <= 5", rpinky::interpreter::Type::Bool(true), less_eq_less);
e2e!(b"5 <= 3", rpinky::interpreter::Type::Bool(false), less_eq_greater);
e2e!(b"5 > 3", rpinky::interpreter::Type::Bool(true), greater_than_true);
e2e!(b"3 > 5", rpinky::interpreter::Type::Bool(false), greater_than_false);
e2e!(b"3 >= 3", rpinky::interpreter::Type::Bool(true), greater_eq_equal);
e2e!(b"true and true", rpinky::interpreter::Type::Bool(true), and_true_true);
e2e!(b"true and false", rpinky::interpreter::Type::Bool(false), and_true_false);
e2e!(b"false and true", rpinky::interpreter::Type::Bool(false), and_false_true);
e2e!(b"false and false", rpinky::interpreter::Type::Bool(false), and_false_false);
e2e!(b"true or true", rpinky::interpreter::Type::Bool(true), or_true_true);
e2e!(b"true or false", rpinky::interpreter::Type::Bool(true), or_true_false);
e2e!(b"false or true", rpinky::interpreter::Type::Bool(true), or_false_true);
e2e!(b"false or false", rpinky::interpreter::Type::Bool(false), or_false_false);
e2e!(b"~true", rpinky::interpreter::Type::Bool(false), not_true);
e2e!(b"~false", rpinky::interpreter::Type::Bool(true), not_false);
e2e!(b"~~true", rpinky::interpreter::Type::Bool(true), double_not_true);
e2e!(b"~~false", rpinky::interpreter::Type::Bool(false), double_not_false);
e2e!(b"5 == 5", rpinky::interpreter::Type::Bool(true), eq_numbers_true);
e2e!(b"5 == 3", rpinky::interpreter::Type::Bool(false), eq_numbers_false);
e2e!(b"true == true", rpinky::interpreter::Type::Bool(true), eq_bools_true);
e2e!(b"true == false", rpinky::interpreter::Type::Bool(false), eq_bools_false);
e2e!(b"'foo' == 'foo'", rpinky::interpreter::Type::Bool(true), eq_strings_true);
e2e!(b"'foo' == 'bar'", rpinky::interpreter::Type::Bool(false), eq_strings_false);
e2e!(b"5 ~= 3", rpinky::interpreter::Type::Bool(true), neq_numbers_true);
e2e!(b"5 ~= 5", rpinky::interpreter::Type::Bool(false), neq_numbers_false);
e2e!(b"'foo' ~= 'bar'", rpinky::interpreter::Type::Bool(true), neq_strings_true);
e2e!(b"2 + 3 * 4", rpinky::interpreter::Type::Number(14.0), precedence_add_mul);
e2e!(b"2 * 3 + 4", rpinky::interpreter::Type::Number(10.0), precedence_mul_add);
e2e!(b"10 - 2 * 3", rpinky::interpreter::Type::Number(4.0), precedence_sub_mul);
e2e!(b"10 / 2 + 3", rpinky::interpreter::Type::Number(8.0), precedence_div_add);
e2e!(b"2 + 3 == 5", rpinky::interpreter::Type::Bool(true), precedence_arith_eq);
e2e!(b"1 < 2 and 3 < 4", rpinky::interpreter::Type::Bool(true), precedence_cmp_and);
e2e!(b"1 > 2 or 3 < 4", rpinky::interpreter::Type::Bool(true), precedence_cmp_or);
e2e!(b"-0", rpinky::interpreter::Type::Number(0.0), unary_minus_zero);
e2e!(b"true", rpinky::interpreter::Type::Bool(true), true_primary);
e2e!(b"0", rpinky::interpreter::Type::Number(0.0), zero_primary);
e2e!(b"''", rpinky::interpreter::Type::String(String::from("")), empty_string_primary);
e2e!(b"'test'", rpinky::interpreter::Type::String(String::from("test")), string_primary);
e2e!(
    b"'hello' + ' ' + 'world'",
    rpinky::interpreter::Type::String(String::from("hello world")),
    string_concatenation
);
e2e!(
    b"(33 + 36) + '' + 21 * 20",
    rpinky::interpreter::Type::String(String::from("69420")),
    string_number_concatenation
);
e2e!(b"~1", rpinky::interpreter::Type::Bool(false), not_positive_num);
e2e!(b"~(-1)", rpinky::interpreter::Type::Bool(false), not_negative_num);
e2e!(b"~'hello'", rpinky::interpreter::Type::Bool(false), not_nonempty_string);
e2e!(
    b"(1 + 2) * (3 + 4) / (5 - 3)",
    rpinky::interpreter::Type::Number(10.5),
    complex_grouped_arithmetic
);
e2e!(b"~(1 > 2) and (3 < 4)", rpinky::interpreter::Type::Bool(true), complex_boolean);
e2e!(b"true and false or true", rpinky::interpreter::Type::Bool(true), and_or_precedence);
e2e!(b"false or true and false", rpinky::interpreter::Type::Bool(false), or_and_precedence);
e2e!(
    b"'value: ' + 42",
    rpinky::interpreter::Type::String(String::from("value: 42")),
    string_concat_number
);
e2e!(
    b"100 + ' percent'",
    rpinky::interpreter::Type::String(String::from("100 percent")),
    number_concat_string
);
e2e!(b"'is ' + true", rpinky::interpreter::Type::String(String::from("is true")), string_concat_bool);
e2e!(
    b"false + ' alarm'",
    rpinky::interpreter::Type::String(String::from("false alarm")),
    bool_concat_string
);

e2e_runtime_error!(b"1 / 0", "division by zero", division_by_zero);
e2e_runtime_error!(b"1 % 0", "modulo by zero", modulo_by_zero);
e2e_runtime_error!(b"-true", "bad operand type for unary -: bool", bool_negation);
e2e_runtime_error!(b"-''", "bad operand type for unary -: string", string_negation);
e2e_runtime_error!(b"'hello' / 'world'", "division is not implemented for string", string_division);
e2e_runtime_error!(b"'hello' % 'world'", "modulo is not implemented for string", string_modulo);
e2e_runtime_error!(b"'hello' - 'world'", "subtraction is not implemented for string", string_subtraction);
e2e_runtime_error!(b"'hello' * 'world'", "multiplication is not implemented for string", string_multiplication);
e2e_runtime_error!(b"'hello' ^ 2", "exponentiation is not implemented for string", string_exponentiation);
