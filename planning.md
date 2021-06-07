# Checklist

 * [x] port `tokenizer` to logos
 * [x] (unsure) add syntax for flagging parameters with stuff like `mut` 
       currently it'd have to be done with attributes, which looks ugly. 
 * [x] port `lumina-parser` and `lumina-compiler` to work with spans
 * [x] add a lot of comments
 * [ ] clean up current modules and freeze them without high-level features
 * [x] make sure our inference works
 * [x] swap out the custom tokenization process with Logos
 * [x] add support for tuples in lumina_typesystem
 * [x] add support for tuples in the parser
 * [x] add syntax for raw pointers and function pointers
 * [ ] make implementations be able to hold generic bounds for it's implementation
 * [x] fix so the tests compile
 * [x] implement pattern matching for `match` expressions
 * [-] (unsure) implement infallible pattern matching on parameters
 * [ ] bitwise operations
       we will probably implement them as `builtins` and then have operators in prelude
 * [ ] add support all throughout the compiler for extern functions
 * [x] change `and` keyword to `next` for `first..then`
 * [ ] add support for primitive/pointer casting

// TODO: Decide whether to allow specifying the struct type in a pattern
// because; we'd solve a lot of our parser code issues by just making the user only match the fields
// and not include the type name. 

# HIR -> MIR conversion planning

We're doing this a bit differently this time. 

I want to apply type-checking on the HIR. As things are being type checked; I want them to be converted to the infallible MIR. 

During the type-checking stage; I'll also be removing basically all sugars. 

Afterwards I'll have an MIR that isn't static dispatched and is infallible. I don't even have to do recursive descent for that conversion. 

So um, what do we call the pass? I suppose `Combiner` certainly wouldn't fit in. 
(there's lots of optimizations we want to do *before* static dispatch for obvious reasons)

`Checker` I suppose? 

## Pattern Wildcard Tracking

Currently we have `Pattern::Parent(Pattern::Wildcard(x))` and such in the `hir`. However; it's possible to completely abstract away the existence of pattern wildcard lookups by turning each branch into a lambda. 

Because this is done AST -> HIR; all the identifier linking will work out fine! 

# Casting

## Motivation

Being able to cast between primitive integer type is essential when working with `ffi`

The syntax can be reused for pointer casting as well. 

I propose the `as` keyword. 

`0 as uint`
`4 as c_int`
`ptr as *int`

# Raw Pointers

## Syntax

We should take advantage of the fact that we will only have heap pointers 

So; maybe we can just have a `box` keyword to heap-allocate stuff? 
Actually; this is a very low-level feature it shouldn't have such an high-level syntax. 

```
// Allocating

fn alloc lo (layout -> *nothing)
  builtin:alloc size.lo align.lo

type layout
  size  usize
  align usize

// Casting

let ptr = alloc { layout . size 8, align 8 }
 in ptr as *u64

// Derefing

fn ptr:deref ptr (*a -> a)
  // we can use the type of `ptr` to figure out what to deref into i guess
  builtin:deref ptr

// // this would use the existing type inference algorithm to correctly infer into the correct type
std:unsafe:ptr:deref ptr
```

## Implementation

A shit ton of magic

## Data Pointers

## Function Pointers

### Syntax

`*(a -> b)` however I feel like that can conflict for if you want to store raw pointers to a bunch of trait objects. 

Instead; we're gonna do `fn(a -> b)`. That way; you can also create raw pointers to raw function pointers like `*fn(a -> b)`

# Tuples

## Implementation

I'm unsure whether to just make this as structs that we generate on-the-fly
Or if we should add it as a builtin feature in the lumina_typesystem. 

I think I might add it as a builtin in the lumina_typesystem since it's a very common type
that's used extensively. 

In general; we're only gonna implement the Backend for them now

# Dynamic dispatch

## Motivation

There's currently two huge issues we've overlooked

This function is mono-polymorphic but implicitly
just adding an extra type-parameter is a really shitty strategy

```
type Applicator a b
  fun (a -> b)
  val a
```

These two types are identical to the users, but this won't type-check and it'll fail because 
the given function will be a different static type. 

```
fn combine ((int -> int) -> [(int -> int), (int -> int)])
  [#f, \v -> v + 1]
```

## Syntax

Syntax option A: 
`fn wrap v (Into int -> int) Into:into v`

Syntax option B:
`fn wrap v (dyn (Into int) -> int) Into:into v`

I think we're gonna go with `A`


## Rules

For dynamic dispatch to be valid; these conditions must hold true

 * The return value of all methods must be statically known


## In the context of higher-order-functions

```
fn apply f x ((a -> b), a -> b) f x
  ->
fn apply f x (Closure {a} b, a -> b) f x
```

Actually; can we do this without having to make changes to the `lumina_typesystem`? 

```
impl show for show_trait_object
  fn show obj (self -> string)
    (ptr:deref show.vtable.v) (ptr:deref data.v)
```
^ I don't think derefs would be performed here

I think we can! 
How does this work when there's multiple implementations for different variants of a trait? I don't think it should matter. 

We should verify that this would work properly with generic traits. 

## Virtual Method Tables

### Dependencies

 * Function Pointers (not closures)
 * Jump Tables
 * Read-Only Memory Segment (statics?)
 * Raw Pointer Operations
   * `creating` raw pointers
   * `derefing` raw pointers
   * `offsetting` raw pointers 
     maybe just cast em to `usize` behind the scenes and back from a function?

### Implementation

Each trait will have a `vtable` in read-only memory. 
Since the `vtable` is taken by pointer, the `vtable` can be a different one for each type. 
That's where the polymorphism comes into play. 
Data also needs pointer because, 'ya know, size differences and indirection requirements. 

```
trait show
  fn show (self -> string)

type show_trait_object
  data *nothing
  vtable *show_vtable

type show_vtable
  show (fn *nothing -> string)

fn use_dyn v (show_trait_object -> string)
  show.vtable.v data.v
```

Oh but hang on here, this relies on us being able to easily construct these pointers. And in this example implementation; the pointers are raw. 

But; is that viable in the real world? Maybe we'd need a `Box` instead of raw pointers so it's properly dropped when needed? 

## Limitations

There's some things that cannot be dynamic dispatched, for example; `[From int]`. 
Because; the return value of them are mixed so we can't manage the data. 

Rust (I think) restricts this through a `Sized` bound. However; I think we're gonna make it a bit more 
magic and just detect based of the parameters whether it can be done or not.

See `Rules` for this detection.

# Flagging parameters (or anything?)

## Motivation

Instead of just using attributes at an top-level, we're gonna want to be able to flag individual parameters and stuff. 

A big use for this would be `mut`

Another use case could be 1st-class debugging support. 

## Syntax

```
attributes [mut 0]
fn low-level v ([a] -> [b])

// these two look pretty good, but, they wouldn't support *multiple* flags
fn low-level v ([a] :: mut -> [b])
fn low-level v ([a] as mut -> [b])
// such as this, which of course does *not* look good
fn low-level v ([a] as [mut, dbg])
// maybe we actually *do* want to place them seperate from the parameters? 

// ye ok let's just go with this. It would make it so that we can't name a type the same thing as these modifiers, but that's alright. 

fn low-level v (mut [a] -> [b])
fn low-level v (debug [a] -> [b])
fn low-level v (result (dbg a) e -> a)

Hm, actually; i think I'm gonna go for `t :: [flags]`
with support for the sugar `t :: singular_flag`
```

# Syntactic sugar for error handling

We've had the `try` idea before. But; it hasn't really fit in well with the declarative syntax. 

However; what about if we combine it with let binds? 
That'd also be *much* easier to bind and it'd prevent confusing early-returns. 

```
try let x = gives_result 30
  in x + x

//

match gives_result
  | ok x -> x + x
  | err e -> e
```

But how would the trait API look? 
We'd have to make it magic...
Unless we use `into_result` like rust, but dunno. 
But then Option won't be able to implement it, so rust does that through magic? oh it does use that api but converts the option to result... feels hacky. 

# Bugs
