fn variable_declaration() {

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Literal;
    use crate::*;

    #[test]
    fn variable_tests() -> Result<(), ()> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Literal::NIL, crate::run("var a; a".to_string(), env.clone())?); // variable initializes to nil
        assert_eq!(Literal::NUMERIC(5.0), crate::run("var b;b=5;b".to_string(), env.clone())?); // var assignment
        assert_eq!(Literal::NUMERIC(5.0), crate::run("var c=5;c".to_string(), env.clone())?); // var declaration with value
        assert_eq!(Literal::NUMERIC(42.0), crate::run("c=42;c".to_string(), env.clone())?); // var reassignment
        assert!(crate::run("var c;".to_string(), env.clone()).is_err()); // var redefinition error
        Ok(())
    }

    #[test]
    fn expression_tests() -> Result<(), ()> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Literal::NUMERIC(5.0), crate::run("3+2".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(0.0), crate::run("5*0".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(5.0), crate::run("(3+2)*1".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(5.0), crate::run("10*(3+2) / 10".to_string(), env.clone())?);
        assert_eq!(Literal::BOOL(true), crate::run("42 == (32 + 10)".to_string(), env.clone())?);
        assert_eq!(Literal::BOOL(false), crate::run("42 != (32 + 10)".to_string(), env.clone())?);
        assert!(crate::run("(100 - 100)/(100 - 100)".to_string(), env.clone()).is_err()); // division by zero error
        Ok(())
    }

    #[test]
    fn scoping_tests() -> Result<(), ()> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Literal::NUMERIC(13.0), crate::run("var a = 21;{a = 13;}a".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(5.0), crate::run("var b = 21;{b = 13; {b=5;}}b".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(13.0), crate::run("var c = 21;{c = 13; {var c=5;}}c".to_string(), env.clone())?);
        assert!(crate::run("{var x = 15;}print x;".to_string(), env.clone()).is_err()); // undeclared variable error
        Ok(())
    }
}