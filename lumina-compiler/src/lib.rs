#![feature(or_patterns)]
#![feature(box_patterns)]
#![feature(trait_alias)]

pub type AstType = lumina_parser::Type;

pub mod collector;
pub use collector::Collector;

pub mod hir;
pub mod mir;

pub mod verifier;
pub use verifier::Verifier;

mod linkedstore;
pub use linkedstore::{LinkedStore, LocationLinkedStore};

pub use lumina_parser::Attr;

// mod combiner;
// pub use combiner::Combiner;

use lumina_util::Typing;

pub type Span = std::ops::Range<usize>;

mod callstack;
pub use callstack::CallStack;
