use crate::tokens::{TokenType, Token};

// generic expression trait
pub trait Expression<T> {
    pub fn evaluate(&self) -> T;
    pub fn display(&self);
}

// binary expression
pub struct Binary<L, R> {
    left: L,
    right: R,
    operator: TokenType,
}

impl Binary {
    pub fn new<L, R>(left: L, operator: TokenType, right: R) -> Binary<L, R>
        where L: Expression, R: Expression {
            Binary{left, operator, right}
    }
}

impl Expression<u64> for Binary<L, R> {
    fn evaluate(&self) ->  {

    }

    fn display(&self) {
        
    }
}