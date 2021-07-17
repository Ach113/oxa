#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Type;
    use crate::*;

    #[test]
    fn variable_tests() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NIL, crate::run("var a; a".to_string(), env.clone())?); // variable initializes to nil
        assert_eq!(Type::NUMERIC(5.0), crate::run("var b;b=5;b".to_string(), env.clone())?); // var assignment
        assert_eq!(Type::NUMERIC(5.0), crate::run("var c=5;c".to_string(), env.clone())?); // var declaration with value
        assert_eq!(Type::NUMERIC(42.0), crate::run("c=42;c".to_string(), env.clone())?); // var reassignment
        assert!(crate::run("d=5;".to_string(), env.clone()).is_err()); // uninitialized variable error
        Ok(())
    }

    #[test]
    fn expression_tests() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(5.0), crate::run("3+2".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(0.0), crate::run("5*0".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(5.0), crate::run("(3+2)*1".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(5.0), crate::run("10*(3+2) / 10".to_string(), env.clone())?);
        assert_eq!(Type::BOOL(true), crate::run("42 == (32 + 10)".to_string(), env.clone())?);
        assert_eq!(Type::BOOL(false), crate::run("42 != (32 + 10)".to_string(), env.clone())?);
        assert_eq!(Type::BOOL(true), crate::run("true or false".to_string(), env.clone())?);
        assert_eq!(Type::BOOL(false), crate::run("true and false".to_string(), env.clone())?);
        assert_eq!(Type::BOOL(false), crate::run("var x = true; !x".to_string(), env.clone())?);
        assert!(crate::run("(100 - 100)/(100 - 100)".to_string(), env.clone()).is_err()); // division by zero error
        Ok(())
    }

    #[test]
    fn scoping_tests() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(13.0), crate::run("var a = 21;{a = 13;}a".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(5.0), crate::run("var b = 21;{b = 13; {b=5;}}b".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(13.0), crate::run("var c = 21;{c = 13; {var c=5;}}c".to_string(), env.clone())?);
        assert!(crate::run("{var x = 15;}print x;".to_string(), env.clone()).is_err()); // undeclared variable error
        Ok(())
    }

    #[test]
    fn control_flow() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(42.0), crate::run("var a; if true {a=42;} a".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(42.0), crate::run("var b; if false {b=10;} else {b=42;} b".to_string(), env.clone())?);
        assert!(crate::run("if false {} else ".to_string(), env.clone()).is_err()); // expect '{}' after 'else'
        assert!(crate::run("if true ".to_string(), env.clone()).is_err()); // expect '{}' after 'if' cond
        Ok(())
    }

    #[test]
    fn loop_test() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(45.0), crate::run("var sum = 0; var i = 0; while i < 10 {sum = sum + i; i = i + 1;}sum".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(0.0), crate::run("var x = 0; while false {x = x + 1; }x".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(0.0), crate::run("var x = 0; while true {break; x = x + 100;}x".to_string(), env.clone())?); // test for break stmt
        assert_eq!(Type::NUMERIC(25.0), crate::run("var x = 0; var mod_sum = 0; while x < 10 {x = x + 1; if x % 2 == 0 {continue;}mod_sum = mod_sum + x;}mod_sum;".to_string(), env.clone())?);
        assert!(crate::run("var i = 0; while i < 10 i = i + 1; i".to_string(), env.clone()).is_err()); // expect '{}' after 'while' cond
        assert!(crate::run("break;".to_string(), env.clone()).is_err()); // break outside loop
        assert!(crate::run("continue;".to_string(), env.clone()).is_err()); // continue outside loop
        Ok(())
    }

    #[test]
    fn function_test() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(42.0), crate::run("fun f() { 42 } f()".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(377.0), crate::run("fun fib(n) { if n < 2 { return n; } return fib(n-1) + fib(n-2); } fib(14)".to_string(), env.clone())?);
        assert!(crate::run("fun f() {} f(1)".to_string(), env.clone()).is_err()); // argc mismatch
        assert!(crate::run("fun f(x) {} f()".to_string(), env.clone()).is_err()); // argc mismatch
        assert!(crate::run("return 42;".to_string(), env.clone()).is_err()); // return outside of function
        assert_eq!(Type::NUMERIC(13.0), crate::run("fun foo() { fun bar() { return 13; } return bar(); } foo()".to_string(), env.clone())?);
        assert!(crate::run("fun foo() { fun bar() { return 13; } return bar(); } bar()".to_string(), env.clone()).is_err()); // undefined function
        Ok(())
    }

    #[test]
    fn class_test() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(6.0), crate::run("class Foo {
            fun init(self, a, b, c) {
              self.a = a;
              self.b = b;
              self.c = c;
            }
                  
            fun sum(self) {
              return self.a + self.b + self.c;
            }
         }
                  
        var foo = Foo();
        foo.init(1, 2, 3);
        foo.sum()".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(13.0), crate::run("class Foo {              
            fun foo_method(self) {
              class Bar {
                fun bar_method(self, x) {
                  self.x = x;
                }
              }
              var b = Bar();
              b.bar_method(13);
              self.x = b.x;
            }
          }
                            
          var foo = Foo();
          foo.foo_method();
          foo.x".to_string(), env.clone())?);
        Ok(())
    }
}