#[cfg(test)]
mod tests {
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
        // for loops
        assert_eq!(Type::NUMERIC(143.0), crate::run("fun fib(x) {
          if x == 1 or x == 0 {
            return x;
          }
          return fib(x - 1) + fib(x - 2);
        }
        var sum = 0;
        for i in list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10){
          sum = sum + fib(i);
        }
        sum".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(10.0), crate::run("var sum = 0;
        for i in list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10){
            if i == 5 {
            break;
          }
          sum = sum + i;
        }sum".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(21.0), crate::run("var sum = 0;
        for i in list(list(1, 2, 3), list(4, 5, 6)){
            for j in i {
            sum = sum + j;
          }
        }sum".to_string(), env.clone())?);
        assert!(crate::run("var x = 15; for i in x {}".to_string(), env.clone()).is_err()); // not iterable
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
        // regular class with constructor
        assert_eq!(Type::NUMERIC(6.0), crate::run("class Foo {
            fun init(self, a, b, c) {
              self.a = a;
              self.b = b;
              self.c = c;
              return self;
            }
                  
            fun sum(self) {
              return self.a + self.b + self.c;
            }
         }
                  
        var foo = Foo(1, 2, 3);
        foo.sum()".to_string(), env.clone())?);
        // nested classes
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
        // returning object
        assert_eq!(Type::NUMERIC(-1.0), crate::run("class Foo {
            fun method(self) {
              class Bar {
                fun bar_method(self, x) {
                  self.x = x;
                }
              }
              var b = Bar();
              b.bar_method(-1);
              return b;
            }
          }
          
          var foo = Foo();
          var bar = foo.method();bar.x".to_string(), env.clone())?);
        // chained getters
        assert_eq!(Type::NUMERIC(42.0), crate::run("class Foo {
            fun method(self) {
              class Bar {
                fun bar_method(self, x) {
                  self.x = x;
                }
              }
              var b = Bar();
              b.bar_method(42);
              self.b = b;
            }
          }
          
          var foo = Foo();
          foo.method();foo.b.x".to_string(), env.clone())?);
        Ok(())
    }

    #[test]
    fn list_test() -> Result<(), String> {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        assert_eq!(Type::NUMERIC(42.0), crate::run("var x = list(1, 2, 42); x[2] ".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(4.0), crate::run("var x = list(1, 2, 42); x.add(-1); x.len()".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(2.0), crate::run("var x = list(1, 2, 42); x.remove(2); x.len()".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(2.0), crate::run("var x = list(list(1, 2)); x[0][1]".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(-1.0), crate::run("var x = list(1, 2, 3); x[0] = -1; x[0]".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(-1.0), crate::run("var x = list(list(1, 2)); x[0][1] = -1; x[0][1]".to_string(), env.clone())?);
        assert_eq!(Type::NUMERIC(60.0), crate::run("class Foo {
          fun init(self, a, b, c) {
            self.a = a;
            self.b = b;
            self.c = c;
            return self;
          }
          fun sum(self) {
            return self.a + self.b + self.c;
          }
        }
        var max = 0;
        var objects = list(Foo(1, 2, 3), Foo(4, 5, 6), Foo(7, 8, 9));
        var vec = list(Foo(10, 20, 30));
        for obj in objects {
          if max < obj.sum() {
            max = obj.sum();
          }
        }
        objects[1] = vec[0];
        for obj in objects {
          if max < obj.sum() {
            max = obj.sum();
          }
        }max".to_string(), env.clone())?);
        assert!(crate::run("var x = list(1, 2, 42); x[4] ".to_string(), env.clone()).is_err());
        assert!(crate::run("var x = list(); x.remove(0) ".to_string(), env.clone()).is_err());
        Ok(())
    }

    #[test]
    fn inheritance_test() -> Result<(), String> {
      let env = Rc::new(RefCell::new(Environment::new(None)));
      assert_eq!(Type::NUMERIC(100.0), crate::run("class A {
        fun method() {
          return 100;
        }
      }
      class B < A {
        fun method() {
          return 10;
        }
        fun test(self) {
          super.method();
        }
      }
      class C < B {}
      C().test()".to_string(), env.clone())?);
      assert_eq!(Type::NUMERIC(10.0), crate::run("class A {
        fun method(self) {
          return 100;
        }
      }
      class B < A {
        fun method(self) {
          return 10;
        }
      }B().method()".to_string(), env.clone())?);
      assert_eq!(Type::NUMERIC(100.0), crate::run("class A {
        fun method(self) {
          return 100;
        }
      }
      class B < A {
        fun method(self) {
          return super.method(self);
        }
      }B().method()".to_string(), env.clone())?);
      assert!(crate::run("class B {
        fun method(self) {
          return super.method(self);
        }
      }".to_string(), env.clone()).is_err());
      assert!(crate::run("class B < D {
        fun method(self) {
          return super.method(self);
        }
      }".to_string(), env.clone()).is_err());
      Ok(())
  }
}