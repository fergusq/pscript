PScript
=======

PScript is a small programming language based on a concept called _models_,
which are similar to type classes in Haskell or traits in Rust. To put it
simply, a model is like an interface in Java (a collection of method
signatures). The difference between interfaces and models is that the
implementations of the methods must not be defined when defining a type, since
they can be declared anywhere in the code. One can use models to extend
existing types to support new functionalities.

Two of the most striking features of the language are:
* No inheritance
* No classes with methods, only structures with fields, every method comes from
  a model

Below is an example of a model declaration.

```
model Squarable {
	$ square();
}

extend Int with Squarable {
	Int square() {
		return this*this;
	}
}
```

The dollar type `$` represents the type for which the method is implemented,
`Int` in this case.

See DOC.md for more information.

## Building

To build the compiler, you need the Happy parser generator and Cabal installed. The program itself has following dependencies:

* base
* containers
* directory
* mtl
* split

Commands:

    happy src/Parser.y
    cabal build

## Features / TODO

### Compiler

- [x] Successfully compiles all tests
- [x] All tests pass
- [x] Module search path
- [ ] Meaningful error messages (half-done)
- [ ] Validation of type parametrized declarations
- [ ] Compiling multiple modules to multiple filesS
- [ ] Disable the exponential growth of size of the resulting C-file when using intersection types

### Language

- Modules
  - [x] Module declarations
  - [x] Importing modules
  - [x] Importing external C-functions
  - [ ] Header files?
- Literals
  - [x] String literals
  - [x] Integer literals
  - [x] List literals
  - [ ] Floating point literals
  - [ ] Character literals
- Expressions
  - [x] Lambda functions
  - [x] Explicit type casts
  - [x] Function pointers
- Declarations
  - [x] Models
    - [x] Extensions
    - [x] Model prerequisites
    - [ ] Model inheritance
  - [x] Structs
  - [x] Enums
- Generics
  - [x] The dollar type
  - [x] Type parametrization of types
  - [x] Type parametrization of functions
- Type inference
  - [x] Variable type inference
  - [x] Type parameter inference
  - [x] List type inference
  - [ ] Lambda type inference
  - [ ] Inference using supertypes
  - [ ] Inference of object type using method parameters
