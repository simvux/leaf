#[allow(unused)]
#[macro_export]
macro_rules! debug {
    ($category:expr, $($arg:tt)*) => (
        #[allow(unused_imports)]
        use colored::*;

        if $category.len() != 0 {
            #[cfg(debug_assertions)]
            print!(" {} {} ", $category.purple(), "->".yellow());
        }
        #[cfg(debug_assertions)]
        println!($($arg)*);
    )
}
