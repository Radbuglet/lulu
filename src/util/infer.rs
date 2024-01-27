use std::{mem::replace, num::NonZeroU32};

use aunty::{Entity, Obj};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use super::{diag::DiagnosticReporter, disjoint_set::DisjointForest, span::Span};

#[derive(Debug)]
pub struct InferEngine {
    var_gen: InferVar,
    children: FxHashMap<(InferVar, u32), InferVar>,
    unions: DisjointForest<InferUnion>,
    simple_conflicts: Vec<(Span, Entity, Entity)>,
}

#[derive(Debug, Default)]
struct InferUnion {
    concretizations: SmallVec<[(Span, Entity); 1]>,
}

impl Default for InferEngine {
    fn default() -> Self {
        Self {
            var_gen: InferVar(NonZeroU32::new(1).unwrap()),
            children: FxHashMap::default(),
            unions: DisjointForest::default(),
            simple_conflicts: Vec::new(),
        }
    }
}

impl InferEngine {
    fn new_var_inner(var_gen: &mut InferVar, unions: &mut DisjointForest<InferUnion>) -> InferVar {
        let id = replace(
            var_gen,
            InferVar(
                var_gen
                    .0
                    .checked_add(1)
                    .expect("created too many inference variables"),
            ),
        );

        assert_eq!(unions.push(InferUnion::default()), id.0.get() - 1);

        id
    }

    pub fn new_var(&mut self) -> InferVar {
        Self::new_var_inner(&mut self.var_gen, &mut self.unions)
    }

    pub fn var_child(&mut self, var: InferVar, idx: u32) -> InferVar {
        *self
            .children
            .entry((var, idx))
            .or_insert_with(|| InferEngine::new_var_inner(&mut self.var_gen, &mut self.unions))
    }

    pub fn unify_single(&mut self, span: Span, lhs: SingleInferTerm, rhs: SingleInferTerm) {
        use SingleInferTerm::*;

        match (lhs, rhs) {
            (Var(lhs), Var(rhs)) => {
                self.unions
                    .union(lhs.0.get() - 1, rhs.0.get() - 1, |mut lhs, rhs| {
                        lhs.concretizations.extend(rhs.concretizations);
                        lhs
                    });
            }
            (Var(var), Const(cst)) | (Const(cst), Var(var)) => self
                .unions
                .find_mut(var.0.get() - 1)
                .1
                .concretizations
                .push((span, cst)),
            (Const(lhs), Const(rhs)) => {
                if lhs != rhs {
                    self.simple_conflicts.push((span, lhs, rhs));
                }
            }
        }
    }

    pub fn unify(&mut self, span: Span, lhs: InferTerm<'_>, rhs: InferTerm<'_>) {
        use InferTerm::*;

        match (lhs, rhs) {
            (Var(lhs), Var(rhs)) => {
                self.unify_single(span, SingleInferTerm::Var(lhs), SingleInferTerm::Var(rhs));
            }
            (Var(var), Const(adt, params)) | (Const(adt, params), Var(var)) => {
                self.unify_single(span, SingleInferTerm::Var(var), SingleInferTerm::Const(adt));

                for (i, param) in params.iter().enumerate() {
                    let var = self.var_child(var, i as u32);
                    self.unify(span, InferTerm::Var(var), *param);
                }
            }
            (Const(lhs_adt, lhs_params), Const(rhs_adt, rhs_params)) => {
                self.unify_single(
                    span,
                    SingleInferTerm::Const(lhs_adt),
                    SingleInferTerm::Const(rhs_adt),
                );

                for (&a, &b) in lhs_params.iter().zip(rhs_params) {
                    self.unify(span, a, b);
                }
            }
        }
    }

    pub fn detect_errors(&mut self, _diag: &Obj<DiagnosticReporter>) -> bool {
        todo!();
    }

    pub fn resolve(&self, var: InferVar) -> Entity {
        // This code assumes that we've already validated the context
        self.unions.find(var.0.get() - 1).1.concretizations[0].1
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum SingleInferTerm {
    Var(InferVar),
    Const(Entity),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct InferVar(NonZeroU32);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum InferTerm<'a> {
    Var(InferVar),
    Const(Entity, &'a [InferTerm<'a>]),
}

impl From<InferVar> for InferTerm<'_> {
    fn from(value: InferVar) -> Self {
        Self::Var(value)
    }
}
