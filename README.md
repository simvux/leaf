# Lumina - A High-Level Compiled Functional Programming Language

## Introduction

Lumina is a work-in-progress programming language inspired by Haskell and Rust focused on efficiency of writing, error readability, simplicity and zero-cost FP abstractions.

## Status

The project is not yet usable and the code in many areas are left undocumented

## Example

```haskell
// traits work similarly to Rust except they support HKT but don't support associated types (yet?)
trait Functor
  fn map (self a, (a -> b) -> self b)

enum option a
  just a
  none

// syntax partially uses indentation however the rules are a lot more loose compared to "other" languages
// this is allowed by the fact that indentation is only syntax on top-level stuff
impl Functor for option a
  fn map opt f (option a, (a -> b) -> option b)
    match opt
      | just a -> just (f a)
      | none   -> none

// type annotations are optional since we have top-level type inference
fn main => is-just (just number)
  where let number = 4

fn is-just opt (option a -> bool)
  match opt
    | just _ -> true
    | none   -> false
```

## Architecture

Most stuff are handwritten. Including the parser, ast, typesystem, ir, inference etc. 

The two big exceptions are the tokenizer (logos) and the compiler backend (probably cranelift)
