use proc_macro_tests::test;

#[test]
fn test() {
    test! {
        #[allow(clippy::approx_constant)]
        const F64_WITH_ATTRIB: f64 = 1.570796327;
        const MINUS_FLOAT: f64 = -1.0;

        const LARGE_USIZE: usize = 16_000_000;
        const MINUS_INT: i8 = -1;

        const INITIAL_VALUE: isize = 5;
        let value = math::new(INITIAL_VALUE);

        // 15
        const ADD: isize = value.add(10);
        // -5
        const SUB: isize = value.sub(20);
        // 25
        const SUB_SUB: isize = value.sub(-30);
        // 50
        const ADD_NEW_OBJECT: isize = value.add(math::new(25));

        let other_math = math::new(10);
        other_math.add(10);
        // 70
        const ADD_OBJECT: isize = value.add(other_math);

        let one = 1;
        let two = 2;
        let three = 3;

        const ARRAY: [usize; _] = [one, 2, three];

        const STR_ARRAY_SZ: usize = _;
        let temp = "A static string";
        const STR: [&str; STR_ARRAY_SZ] = [temp, "array"];

        let four_math = math::new(10);
        four_math.add_four(1, 3, 6, 9);
        const FOUR_MATH: i128 = four_math.get();
    };

    #[allow(clippy::approx_constant)]
    {
        assert_eq!(F64_WITH_ATTRIB, 1.570796327);
    }
    assert_eq!(MINUS_FLOAT, -1.0);
    assert_eq!(LARGE_USIZE as u128, 16_000_000_u128);
    assert_eq!(MINUS_INT as i128, -1_i128);
    // test that 'math' local wasn't exposed outside the proc macro
    let math = 321;
    assert_eq!(math, 321);
    assert_eq!(INITIAL_VALUE, 5);
    assert_eq!(ADD, 15);
    assert_eq!(SUB, -5);
    assert_eq!(SUB_SUB, 25);
    assert_eq!(ADD_NEW_OBJECT, 50);
    assert_eq!(ADD_OBJECT, 70);
    assert_eq!(ARRAY, [1, 2, 3]);
    assert_eq!(STR, ["A static string", "array"]);
    assert_eq!(STR_ARRAY_SZ, 2);
    assert_eq!(FOUR_MATH, 29);
}

#[test]
fn test_multiple_borrow() {
    test! {
        let hundred = math::new(100);
        let one = math::new(1);
        const RESULT: u16 = hundred.add(hundred.add(hundred.get()));
        one.add_to(hundred);
        const RESULT_2: u16 = hundred.get();
    }

    assert_eq!(RESULT, 400);
    assert_eq!(RESULT_2, 401);
}

#[test]
fn test_custom_expr() {
    const TEST: &str = "Blah";
    const ANSWER: i32 = 42;

    fn test() -> i32 {
        ANSWER
    }

    struct Test {
        value1: &'static str,
        value2: fn() -> i32,
    }

    test! {
        const TEST_STRUCT: Test = expr::custom("Test {value1: TEST, value2: test}");
    }

    assert_eq!(TEST_STRUCT.value1, TEST);
    assert_eq!((TEST_STRUCT.value2)(), ANSWER);
}

#[test]
fn test_string_concat() {
    test! {
        let first = "First";
        let second = "Second";
        const STRING: &str = string::concat(first, second);
    }

    assert_eq!(STRING, "FirstSecond");
}

#[test]
fn test_string_object_concat() {
    test! {
        let prefix = string::new("A ");
        let variable = "Variable";
        const VARIABLE: &str = prefix.concat(variable);
        const LITERAL: &str = prefix.concat("Literal");
    }

    assert_eq!(VARIABLE, "A Variable");
    assert_eq!(LITERAL, "A Literal");
}
