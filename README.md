![rust workflow](https://github.com/ndtoan96/cmdexpand/actions/workflows/rust.yml/badge.svg)

# Introduction

This library performs batch-like expansions in strings, that is, to expand `%VAR%` into their values inside some context. It's like a Windows version of [shellexpand](https://crates.io/crates/shellexpand).

Aside from context variables, it can also expands number arguments (e.g `%1`, `%*`) syntax with a list of arguments provided by user.

Unlike `shellexpand`, variables that do not exist will expand to empty string.

# Usage
See example below on how to define a context. The library also provids a default `cmdexpand::env_context` which can expand environment variable.
```rust
use cmdexpand::Expander;

fn mycontext(s: &str) -> Option<String> {
   match s {
       "NAME" => Some(String::from("Alice")),
       "HOBBY" => Some(String::from("coding")),
       _ => None,
   }
}

assert_eq!(Expander::new(r#"echo "%NAME%'s hobby is %HOBBY%""#)
              .add_context(&mycontext)
              .expand()
              .unwrap(),
           r#"echo "Alice's hobby is coding""#);
```

You can provide a list of arguments too.
```rust
use cmdexpand::Expander;

assert_eq!(Expander::new("del %*")
            .add_args(&["a.txt", "b.txt"])
            .expand()
            .unwrap(), "del a.txt b.txt");
```
