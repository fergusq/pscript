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
