# Goals
The goal of this library is to assist in the creation of proc macros, where the purpose of the proc macro is to create constants. In addition, it sets out to achieve the following:
- the contents of the proc macro will use a subset of Rust syntax (for the purpose of readability);
- the proc macro will allow compile time functionality that might not be available to the target platform (for example: the standard library or floating point arithmatic); and
- users of the proc macro will be given helpful complie time errors

# Examples
One use case for proc macros is to provide additional compile time functionality for the creation of constants.

A simple (although redundant) example of this would be to concatenate two literal strings together (in the same vein as the `concat!` macro_rule).

This library provides an API that makes the creation of such a proc macro trivial. For example, the following code would be a complete proc macro providing said functionality:
```rust
#[proc_macro]
pub fn string_concat(tokens: TokenStream) -> TokenStream {
    let mut string_path = Path::new();
    string_path.add_function(
        "concat",
        &(&|first: String, second: String| -> String {
            first + &second
        } as &dyn Fn(String, String) -> String),
   );

   let mut env = ProcMacroEnv::new();
   env.add_path("string", string_path);
   env.process(tokens)
}
```

This code creates a proc macro “environment” with a single Path named “string”. Under this Path is a single function named “concat” that accepts two Strings and returns a String (which is the concatenated result).

Here is an example of code that makes use of this proc macro:
```rust
string_concat! {
    let first = "First";
    let second = "Second";
    const STRING: &str = string::concat(first, second);
}

assert_eq!(STRING, "FirstSecond");
```

Note that the two variables ‘first’ and ‘second’ are dropped from scope at the end of the macro; only const values are preserved.

It is also possible to create and use objects. For example, a “String” object with a “concat” method can be created with the following code:
```rust
#[proc_macro]
pub fn string_concat(tokens: TokenStream) -> TokenStream {
    let mut string_type = ObjectType::new();
    string_type.add_method(
        "concat",
        &(&|first: &String, second: String| -> String { first.to_owned() + &second }
            as &dyn Fn(&String, String) -> String),
    );
    // sealing the ObjectType means it is no longer mutable and can now instantiate objects
    let string_type = string_type.seal();

    let mut string_path = Path::new();
    let string_new =
        &|first: String| -> Object { string_type.new_instance(first) } as &dyn Fn(String) -> Object;
    string_path.add_function("new", &string_new);

    let mut env = ProcMacroEnv::new();
    env.add_path("string", string_path);
    env.process(tokens)
}
```

And used in the following manner:
```rust
string_concat! {
    let prefix = string::new("A ");
    let variable = "Variable";
    const VARIABLE: &str = prefix.concat(variable);
    const LITERAL: &str = prefix.concat("Literal");
}

assert_eq!(VARIABLE, "A Variable");
assert_eq!(LITERAL, "A Literal");
```

Whilst these two examples are contrived, they serve to demonstrate the functionality of the library. A more practical example would be to add functionality at compile time that is not available on the target platform.

For example, say the target platform is the microprocessor of an Arduino Uno (the atmega328p). Code for this is usually compiled without the standard library and without Floating Point arithmetic. If you wanted to make use (at compile time) of a library that uses both floating points and the standard library, you can easily glue that functionality to a proc macro using this library; and then use that proc macro to create a constant.

A more concrete example is a constant array representing points of a sine wave at fixed intervals. The calculations within the proc macro can make use of the standard library and floating points, but the resulting constant array can be just u8s.
