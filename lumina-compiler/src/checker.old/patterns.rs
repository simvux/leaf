use super::*;

// wildcards here will get kinda weird. Because; pattern stuff will be the *only* place where we
// still have non-parameter identifiers.
//
// So; I'm thinking that we might want to make every single branch a function instead. This way it
// becomes more similar to a jump table.
//
// ```
// fn f a =>
//   match {4, 2}
//     {1, x} if x > 1 -> a + 2
//     {x, y} -> a + y
//
//  fn f a =>
//    match {4, 2}
//      {1, x} if x > 1 -> (\{x, y} -> a + 2) {1, x}
//      {x, y}          -> (\{x, y} -> x + y) {x, y}
// ```
//
// If we do it this way then the concept of an wildcard (or non-param local) is nonexistant in the
// MIR. Which would be really comfy.
//
// Of course a lot of DCE would be needed later to sort out the mess that this causes. But we can
// rely on that being managable.
//
// Although; how will we compile the existing parent-wildcard stuff in the hir::Entity?
// One really shitty hacky way would be to just map all the Pattern with Pattern::Parent.
//
// Ye; I think I'm literally just gonna do that.
//
// We also need to verify total fallibility and stuff
//
// Actually; even with the lambda approach we still would need an mir::Wildcard just to apply the
// lambda.
//
// So; perhaps we should have a wrapper struct over mir::Entity which applies the lambda with a
// pattern?
//
// This way we still need the to_lambda_compatible_body though.
//
// I think we might want to make it so that the match expression is in a singular value so we can
// destruct it more easily.
//
// Alternatively of just mapping hir::Entity directly; we could just have more manual control over
// the hir -> mir transaction.
//
// Wait a second. We're doing the lifting on this pass. There should be a way to skip all the
// annoying stuff with Lookup::Pattern.
//
// Actually; maybe we just make run_lambda return what we need to capture. Because; I think we've
// currently completely forgetting that aspect of lambdas in checker/mod.rs
//
// Perhaps we'll make this expansion in the Verifier after all.
// Would solve a lot of issues. Because then we can ignore the fact that pattern wildcard calls
// exist at this stage. We only need to focus on creating them.
//
// So basically; creating the `applicator` concept at the Verifier instead.
// And then never having to lookup a pattern wildcard
//
// I think this should theoretically work
//
// In theory it'd look like
//
// ```
// let a = 2 in
//  match {2, 4}
//    | {1, y} if y > 2 -> a + y
//    | {x, y} -> a + x
//
// -- lowers -- //
//
// let a = 2 in
//  match {2, 4}
//    | {1, y} if y > 2 -> (\{_, y} -> a + y) {1, y}
//    | {x, y} -> (\{x, y} -> a + x) {x, y}
// ```

#[derive(Clone, Debug)]
pub struct MatchExpr {
    expr: Box<mir::Entity>,

    branches: Vec<(Pattern, PatternApplication)>,
}

#[derive(Clone, Debug)]
struct PatternApplication {
    body: mir::FID,

    // maps wildcards to parameters
    applicator: Applicator,
}

pub type Applicator = Vec<usize>;

#[derive(Clone, Debug)]
pub enum Pattern {}

impl<'hir, 'held> CheckerPass<'hir, 'held> {
    pub fn run_match(
        &mut self,
        kind: hir::function::MatchKind,
        of: &hir::Entity,
        table: &hir::PatternTable,
        branches: &[(hir::function::Header, Tr<hir::Entity>, Vec<Tr<String>>)],
    ) -> Result<Expression, Error> {
        let expr = self.run(of)?;

        // TODO: Lift the lambdas using dynfuncs
        //
        // Do the Applicator stuff
        for (pat, (eheader, eentity, epnames)) in table.patterns.iter().zip(branches.iter()) {
            // let (p, a, t) =
        }

        todo!();
    }

    fn run_pattern(
        &mut self,
        pattern: &hir::Pattern,
        exp: &Tp<hir::Type>,
    ) -> Result<(Pattern, Applicator), Error> {
        todo!();
    }
}

// we need to compile down match to decision trees on the same pass as type checking.
//
// Before we do this; we should fix the more low-level stuff so we can use that in our lowering
// here.
/*
pub enum Completion {
    List(ListCompletion),
    Tuple(),
}

enum ListCompletion {
    First(usize),
    Complete,
}
*/
