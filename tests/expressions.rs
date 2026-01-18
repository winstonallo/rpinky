use rpinky;

macro_rules! e2e {
    ($src:literal, $exp:expr, $name:ident) => {
        #[test]
        fn $name() {
            let src = $src;
            let exp = $exp;
            let mut lexer = rpinky::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().unwrap();
            let ast = rpinky::parser::Parser::new(tokens.to_vec()).expr().unwrap();
            let result = rpinky::interpreter::expr(&ast).unwrap();
            assert_eq!(result, exp);
        }
    };
}

macro_rules! e2e_runtime_error {
    ($src:literal, $exp:expr, $name:ident) => {
        #[test]
        fn $name() {
            let src = $src;
            let exp = $exp;
            let mut lexer = rpinky::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().unwrap();
            let ast = rpinky::parser::Parser::new(tokens.to_vec()).expr().unwrap();
            let result = rpinky::interpreter::expr(&ast);
            assert_eq!(result, Err(exp));
        }
    };
}

e2e!(b"7.7", rpinky::interpreter::Type::Number { value: 7.7, line: 1 }, number_primary);
e2e!(b"false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, bool_primary);
e2e!(b"2 + 2", rpinky::interpreter::Type::Number { value: 4.0, line: 1 }, add);
e2e!(b"2 * 9", rpinky::interpreter::Type::Number { value: 18.0, line: 1 }, mul);
e2e!(b"9 / 2", rpinky::interpreter::Type::Number { value: 4.5, line: 1 }, div);
e2e!(b"2 * 9 + 13", rpinky::interpreter::Type::Number { value: 31.0, line: 1 }, precedence);
e2e!(b"2 * 9 + - -5", rpinky::interpreter::Type::Number { value: 23.0, line: 1 }, unary_minus);
e2e!(b"2 ^ 3 ^ 3 - 1", rpinky::interpreter::Type::Number { value: 134217727.0, line: 1 }, exponent);
e2e!(b"(2 ^ 3 ^ 3 - 1) % 2", rpinky::interpreter::Type::Number { value: 1.0, line: 1 }, rem);
e2e!(b"2 * (9 + 13) / 2", rpinky::interpreter::Type::Number { value: 22.0, line: 1 }, paren_1);
e2e!(
    b"2 * (9 + 13) + 2 ^ 2 + (((3 * 3) - 3) + 3.324) / 2.1",
    rpinky::interpreter::Type::Number { value: 52.44, line: 1 },
    paren_2
);
e2e!(
    b"14 / (12 / 2) / 2",
    rpinky::interpreter::Type::Number {
        value: 1.1666666666666667,
        line: 1
    },
    paren_3
);
e2e!(
    b"(44 >= 2) or false and 1 > 0",
    rpinky::interpreter::Type::Bool { value: true, line: 1 },
    bool_or
);
e2e!(b"~(44 >= 2)", rpinky::interpreter::Type::Bool { value: false, line: 1 }, bool_not);
e2e!(b"~(3 ~= 2)", rpinky::interpreter::Type::Bool { value: false, line: 1 }, noteq);
e2e!(b"(3 == 2 + 1)", rpinky::interpreter::Type::Bool { value: true, line: 1 }, eqeq);
e2e!(b"-2 ^ 3", rpinky::interpreter::Type::Number { value: -8.0, line: 1 }, exponent_unary_minus);
e2e!(b"~''", rpinky::interpreter::Type::Bool { value: true, line: 1 }, not_string);
e2e!(b"~0", rpinky::interpreter::Type::Bool { value: true, line: 1 }, not_num);
e2e!(b"10 - 3", rpinky::interpreter::Type::Number { value: 7.0, line: 1 }, sub);
e2e!(b"5 - 10", rpinky::interpreter::Type::Number { value: -5.0, line: 1 }, sub_negative_result);
e2e!(b"0 - 0", rpinky::interpreter::Type::Number { value: 0.0, line: 1 }, sub_zero);
e2e!(b"3 < 5", rpinky::interpreter::Type::Bool { value: true, line: 1 }, less_than_true);
e2e!(b"5 < 3", rpinky::interpreter::Type::Bool { value: false, line: 1 }, less_than_false);
e2e!(b"3 <= 3", rpinky::interpreter::Type::Bool { value: true, line: 1 }, less_eq_equal);
e2e!(b"3 <= 5", rpinky::interpreter::Type::Bool { value: true, line: 1 }, less_eq_less);
e2e!(b"5 <= 3", rpinky::interpreter::Type::Bool { value: false, line: 1 }, less_eq_greater);
e2e!(b"5 > 3", rpinky::interpreter::Type::Bool { value: true, line: 1 }, greater_than_true);
e2e!(b"3 > 5", rpinky::interpreter::Type::Bool { value: false, line: 1 }, greater_than_false);
e2e!(b"3 >= 3", rpinky::interpreter::Type::Bool { value: true, line: 1 }, greater_eq_equal);
e2e!(b"true and true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, and_true_true);
e2e!(b"true and false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, and_true_false);
e2e!(b"false and true", rpinky::interpreter::Type::Bool { value: false, line: 1 }, and_false_true);
e2e!(b"false and false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, and_false_false);
e2e!(b"true or true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, or_true_true);
e2e!(b"true or false", rpinky::interpreter::Type::Bool { value: true, line: 1 }, or_true_false);
e2e!(b"false or true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, or_false_true);
e2e!(b"false or false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, or_false_false);
e2e!(b"~true", rpinky::interpreter::Type::Bool { value: false, line: 1 }, not_true);
e2e!(b"~false", rpinky::interpreter::Type::Bool { value: true, line: 1 }, not_false);
e2e!(b"~~true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, double_not_true);
e2e!(b"~~false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, double_not_false);
e2e!(b"5 == 5", rpinky::interpreter::Type::Bool { value: true, line: 1 }, eq_numbers_true);
e2e!(b"5 == 3", rpinky::interpreter::Type::Bool { value: false, line: 1 }, eq_numbers_false);
e2e!(b"true == true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, eq_bools_true);
e2e!(b"true == false", rpinky::interpreter::Type::Bool { value: false, line: 1 }, eq_bools_false);
e2e!(b"'foo' == 'foo'", rpinky::interpreter::Type::Bool { value: true, line: 1 }, eq_strings_true);
e2e!(b"'foo' == 'bar'", rpinky::interpreter::Type::Bool { value: false, line: 1 }, eq_strings_false);
e2e!(b"5 ~= 3", rpinky::interpreter::Type::Bool { value: true, line: 1 }, neq_numbers_true);
e2e!(b"5 ~= 5", rpinky::interpreter::Type::Bool { value: false, line: 1 }, neq_numbers_false);
e2e!(b"'foo' ~= 'bar'", rpinky::interpreter::Type::Bool { value: true, line: 1 }, neq_strings_true);
e2e!(b"2 + 3 * 4", rpinky::interpreter::Type::Number { value: 14.0, line: 1 }, precedence_add_mul);
e2e!(b"2 * 3 + 4", rpinky::interpreter::Type::Number { value: 10.0, line: 1 }, precedence_mul_add);
e2e!(b"10 - 2 * 3", rpinky::interpreter::Type::Number { value: 4.0, line: 1 }, precedence_sub_mul);
e2e!(b"10 / 2 + 3", rpinky::interpreter::Type::Number { value: 8.0, line: 1 }, precedence_div_add);
e2e!(b"2 + 3 == 5", rpinky::interpreter::Type::Bool { value: true, line: 1 }, precedence_arith_eq);
e2e!(b"1 < 2 and 3 < 4", rpinky::interpreter::Type::Bool { value: true, line: 1 }, precedence_cmp_and);
e2e!(b"1 > 2 or 3 < 4", rpinky::interpreter::Type::Bool { value: true, line: 1 }, precedence_cmp_or);
e2e!(b"-0", rpinky::interpreter::Type::Number { value: 0.0, line: 1 }, unary_minus_zero);
e2e!(b"true", rpinky::interpreter::Type::Bool { value: true, line: 1 }, true_primary);
e2e!(b"0", rpinky::interpreter::Type::Number { value: 0.0, line: 1 }, zero_primary);
e2e!(
    b"''",
    rpinky::interpreter::Type::String {
        value: String::from(""),
        line: 1
    },
    empty_string_primary
);
e2e!(
    b"'test'",
    rpinky::interpreter::Type::String {
        value: String::from("test"),
        line: 1
    },
    string_primary
);
e2e!(
    b"'hello' + ' ' + 'world'",
    rpinky::interpreter::Type::String {
        value: String::from("hello world"),
        line: 1
    },
    string_concatenation
);
e2e!(
    b"(33 + 36) + '' + 21 * 20",
    rpinky::interpreter::Type::String {
        value: String::from("69420"),
        line: 1
    },
    string_number_concatenation
);
e2e!(b"~1", rpinky::interpreter::Type::Bool { value: false, line: 1 }, not_positive_num);
e2e!(b"~(-1)", rpinky::interpreter::Type::Bool { value: false, line: 1 }, not_negative_num);
e2e!(b"~'hello'", rpinky::interpreter::Type::Bool { value: false, line: 1 }, not_nonempty_string);
e2e!(
    b"(1 + 2) * (3 + 4) / (5 - 3)",
    rpinky::interpreter::Type::Number { value: 10.5, line: 1 },
    complex_grouped_arithmetic
);
e2e!(
    b"~(1 > 2) and (3 < 4)",
    rpinky::interpreter::Type::Bool { value: true, line: 1 },
    complex_boolean
);
e2e!(
    b"true and false or true",
    rpinky::interpreter::Type::Bool { value: true, line: 1 },
    and_or_precedence
);
e2e!(
    b"false or true and false",
    rpinky::interpreter::Type::Bool { value: false, line: 1 },
    or_and_precedence
);
e2e!(
    b"'value: ' + 42",
    rpinky::interpreter::Type::String {
        value: String::from("value: 42"),
        line: 1
    },
    string_concat_number
);
e2e!(
    b"100 + ' percent'",
    rpinky::interpreter::Type::String {
        value: String::from("100 percent"),
        line: 1
    },
    number_concat_string
);
e2e!(
    b"'is ' + true",
    rpinky::interpreter::Type::String {
        value: String::from("is true"),
        line: 1
    },
    string_concat_bool
);
e2e!(
    b"false + ' alarm'",
    rpinky::interpreter::Type::String {
        value: String::from("false alarm"),
        line: 1
    },
    bool_concat_string
);

e2e_runtime_error!(b"1 / 0", rpinky::errors::RuntimeError::new("division by zero".into(), 1), division_by_zero);
e2e_runtime_error!(b"1 % 0", rpinky::errors::RuntimeError::new("modulo by zero".into(), 1), modulo_by_zero);
e2e_runtime_error!(
    b"-true",
    rpinky::errors::RuntimeError::new("bad operand type for unary -: bool".into(), 1),
    bool_negation
);
e2e_runtime_error!(
    b"-''",
    rpinky::errors::RuntimeError::new("bad operand type for unary -: string".into(), 1),
    string_negation
);
e2e_runtime_error!(
    b"'hello' / 'world'",
    rpinky::errors::RuntimeError::new("division is not implemented for string".into(), 1),
    string_division
);
e2e_runtime_error!(
    b"'hello' % 'world'",
    rpinky::errors::RuntimeError::new("modulo is not implemented for string".into(), 1),
    string_modulo
);
e2e_runtime_error!(
    b"'hello' - 'world'",
    rpinky::errors::RuntimeError::new("subtraction is not implemented for string".into(), 1),
    string_subtraction
);
e2e_runtime_error!(
    b"'hello' * 'world'",
    rpinky::errors::RuntimeError::new("multiplication is not implemented for string".into(), 1),
    string_multiplication
);
e2e_runtime_error!(
    b"'hello' ^ 2",
    rpinky::errors::RuntimeError::new("exponentiation is not implemented for string".into(), 1),
    string_exponentiation
);
