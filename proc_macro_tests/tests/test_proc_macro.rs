#![cfg(feature = "proc_macro_expand")]

use macro_const::macro_const;
use proc_macro_tests::test;

#[test]
fn test_proc_macro_expand() {
    macro_const! {
        const CLOCK_HZ: usize = 16_000_000;
    }

    test! {
        const ANOTHER: usize = CLOCK_HZ!();
    }

    assert_eq!(CLOCK_HZ, ANOTHER);
    assert_eq!(ANOTHER, 16_000_000);
}
