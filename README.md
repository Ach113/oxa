# oxa
interpreted, object-oriented programming language. Interpreter written 100% in rust

## Table of contents
* [Info](#info)
* [Examples](#examples)
* [Future Plans](#future-plans)

# Info
oxa is my first language. I took inspiration from [this book](https://craftinginterpreters.com/), which I definitely recommend to anyone interested in writing new languages (or better understanding how computer languages work). The book uses Java to implement an interpreter for a language named LOX, which I have used as a blueprint for writing oxa. 

I would not neccessarily call oxa a copy of LOX. Considering Rust and Java are very different languages, I had to change some things quite a bit to better suite the implementation language. Additionally, oxa also supports import statements (syntax being identical to Python), lists (along with bracket operator for indexing) and some native functions for dealing with user input, file I/O and time.

# Examples

Hello world program
```
print "Hello World"!;
```
fibonacci sequence
```
fun fib(x) {
  if x == 1 or x == 0 {
    return x;
  }
  return fib(x-1) + fib(x-2);
}

var x = input();
print fib(x);
```

a bit more complex example
```
class Triple {
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
var objects = list(Triple(1, 2, 3), Triple(4, 5, 6), Triple(7, 8, 9));
objects.add(Triple(10, 20, 30));

for obj in objects {
  if max < obj.sum() {
    max = obj.sum();
  }
}

print max; // prints 60
```

# Future Plans

Since oxa is my hobby project which I started to mainly learn Rust and improve my knowledge of programming languages, I want to continue adding new features to the language. But my next main goal would be turning the interpreter into a bytecode compiler. 
