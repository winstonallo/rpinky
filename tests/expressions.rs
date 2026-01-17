use cpinky;

macro_rules! e2e {
    ($src:literal, $exp:expr, $name:ident) => {
        #[test]
        fn $name() {
            let src = $src;
            let exp = $exp;
            let mut lexer = cpinky::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().unwrap();
            let ast = cpinky::parser::Parser::new(tokens).parse().unwrap();
            let result = cpinky::interpreter::interpret(&ast).unwrap();
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
            let mut lexer = cpinky::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().unwrap();
            let ast = cpinky::parser::Parser::new(tokens).parse().unwrap();
            let result = cpinky::interpreter::interpret(&ast);
            assert_eq!(result, Err(exp));
        }
    };
}

e2e!(b"7.7", cpinky::interpreter::Type::Number { value: 7.7, line: 1 }, number_primary);
e2e!(b"false", cpinky::interpreter::Type::Bool { value: false, line: 1 }, bool_primary);
e2e!(b"2 + 2", cpinky::interpreter::Type::Number { value: 4.0, line: 1 }, add);
e2e!(b"2 * 9", cpinky::interpreter::Type::Number { value: 18.0, line: 1 }, mul);
e2e!(b"9 / 2", cpinky::interpreter::Type::Number { value: 4.5, line: 1 }, div);
e2e!(b"2 * 9 + 13", cpinky::interpreter::Type::Number { value: 31.0, line: 1 }, precedence);
e2e!(b"2 * 9 + - -5", cpinky::interpreter::Type::Number { value: 23.0, line: 1 }, unary_minus);
e2e!(b"2 ^ 3 ^ 3 - 1", cpinky::interpreter::Type::Number { value: 134217727.0, line: 1 }, exponent);
e2e!(b"(2 ^ 3 ^ 3 - 1) % 2", cpinky::interpreter::Type::Number { value: 1.0, line: 1 }, rem);
e2e!(b"2 * (9 + 13) / 2", cpinky::interpreter::Type::Number { value: 22.0, line: 1 }, paren_1);
e2e!(
    b"2 * (9 + 13) + 2 ^ 2 + (((3 * 3) - 3) + 3.324) / 2.1",
    cpinky::interpreter::Type::Number { value: 52.44, line: 1 },
    paren_2
);
e2e!(
    b"14 / (12 / 2) / 2",
    cpinky::interpreter::Type::Number {
        value: 1.1666666666666667,
        line: 1
    },
    paren_3
);
e2e!(
    b"(44 >= 2) or false and 1 > 0",
    cpinky::interpreter::Type::Bool { value: true, line: 1 },
    bool_or
);
e2e!(b"~(44 >= 2)", cpinky::interpreter::Type::Bool { value: false, line: 1 }, bool_not);
e2e!(b"~(3 ~= 2)", cpinky::interpreter::Type::Bool { value: false, line: 1 }, noteq);
e2e!(b"(3 == 2 + 1)", cpinky::interpreter::Type::Bool { value: true, line: 1 }, eqeq);
e2e!(b"-2 ^ 3", cpinky::interpreter::Type::Number { value: -8.0, line: 1 }, exponent_unary_minus);
e2e!(
    b"'hello' + ' ' + 'world'",
    cpinky::interpreter::Type::String {
        value: String::from("hello world"),
        line: 1
    },
    string_concatenation
);

e2e_runtime_error!(b"1 / 0", cpinky::errors::RuntimeError::new("division by zero".into(), 1), division_by_zero);
e2e_runtime_error!(b"1 % 0", cpinky::errors::RuntimeError::new("modulo by zero".into(), 1), modulo_by_zero);
