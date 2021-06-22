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
        assert!(crate::run("d=5;".to_string(), env.clone()).is_err()); // uninitialized variable error
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
        assert_eq!(Literal::BOOL(true), crate::run("true or false".to_string(), env.clone())?);
        assert_eq!(Literal::BOOL(false), crate::run("true and false".to_string(), env.clone())?);
        assert_eq!(Literal::BOOL(false), crate::run("var x = true; !x".to_string(), env.clone())?);
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

    #[test]
    fn control_flow() -> Result<(), ()> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Literal::NUMERIC(42.0), crate::run("var a; if true {a=42;} a".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(42.0), crate::run("var b; if false {b=10;} else {b=42;} b".to_string(), env.clone())?);
        assert!(crate::run("if false {} else ".to_string(), env.clone()).is_err()); // expect '{}' after 'else'
        assert!(crate::run("if true ".to_string(), env.clone()).is_err()); // expect '{}' after 'if' cond
        Ok(())
    }

    #[test]
    fn loop_test() -> Result<(), ()> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Literal::NUMERIC(45.0), crate::run("var sum = 0; var i = 0; while i < 10 {sum = sum + i; i = i + 1;}sum".to_string(), env.clone())?);
        assert_eq!(Literal::NUMERIC(0.0), crate::run("var x = 0; while 12 < 10 {x = x + 1; }x".to_string(), env.clone())?);
        assert!(crate::run("var i = 0; while i < 10 i = i + 1; i".to_string(), env.clone()).is_err()); // expect '{}' after 'while' cond
        Ok(())
    }
}