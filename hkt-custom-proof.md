trait Functor
  fn map (self a, (a -> b) -> self b)

// invalid
impl Functor for option int
  fn map (self int, (int -> b) -> self b)
fn takes f (f a -> f string) when f Functor
  Functor:map (f 0.0) #to_string

impl Functor for option a
when a Show
  fn map opt f (self a, (a -> b) -> self b)
    match opt
      some n -> 
        first print n
        then some f.n 
      none -> none
fn takes f (f a -> f string) when f Functor
  Functor:map (f 0.0) #to_string
struct Test !Show
fn main
  first takes some.{ Test . } // option Test doesn't satisfy Functor
  then  takes some.0 // valid

trait IntFunctor
  fn map (self int, (int -> b) -> self b)
impl IntFunctor for option int
  fn map opt f (self int, (int -> b) -> self b)
    Functor:map opt #f
fn takes f (f int -> f string) when f IntFunctor
  IntFunctor:map f #int:to_string
fn main
  first takes some.(0.0) // invalid: because `option float` not `IntFunctor`
  then  takes some.0 // valid: because `option int` is `IntFunctor`

// new proposal
trait Functor
  fn map (self a, (a -> b), self? b)
impl Functor for option a
  fn map (self a, (a -> b), self? b)
trait From a
  fn from (a -> self)
fn From (option a) for option b
when b From a
  fn from opt (option a -> option b)
    Functor:map opt #From:from
trait IntFunctor
  fn map (self int, (int -> b) -> self? b)
impl IntFunctor for option int
  fn map (option int, (int -> b) -> option b)
  fn map (self int, (int -> b) -> self? b)
  fn map (self int, (int -> b) -> self b) // TODO: Also allow this? technically don't need it on both trait and impl

// syntastic issues
//
// Should we explicitely set the requirement, or explicitely loosen the requirement? 
