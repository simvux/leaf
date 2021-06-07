use lumina_typesystem::{Tp, Type};

pub type TRID = usize;
pub type MID = usize;
pub type FID = usize;
pub type DYNFID = usize;
pub type PID = usize;

#[derive(Clone, Debug)]
pub enum Entity {
    Function(FID, Vec<Entity>),
    Method(TRID, MID, Vec<Entity>),
    DynFunction(DYNFID, Vec<Entity>),
    Parameter(PID),
    ParameterCall(PID, Vec<Entity>),

    Int(i64),
    Float(f64),
    FnPointer(FID),

    Structure(Tp<Type>, Vec<Self>),
    IfExpr(Vec<(Self, Self)>),
    // Match(patterns::MatchExpr),
    //
    // we just turn casting into builtins instead I think.
    // Cast(Tp<Type>),
    //
    // Probably want these:
    //
    // JumpTable,
}

pub struct Expression<M> {
    pub entity: Entity,
    pub meta: M,
}

impl<M> Expression<M> {
    pub fn new(entity: Entity, meta: M) -> Self {
        Self { entity, meta }
    }
}
