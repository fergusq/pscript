PScript documentation
=====================

## Concepts

### Models

A model is a generalization of a Java-like interface. No type implements a model when declared, but instead a type can be extended to
implement any model at any time after both the model and the type have been declared. Models are similar to traits in Rust and type classes
in Haskell.

`extend with` declaration is used to extend a type to implement a model:

```
model Foo {
	Int bar();
}

extend Int with Foo {
	Int bar() {
		return this + 5;
	}
}
```

When extending a generic type, each type parameter must be specified. Pattern matching is not yet supported.

```
extend Array<@T> with Foo {
	Int bar() {
		return this.size() + 5;
	}
}

// Pattern matching is not yet supported:
// extend Pointer<Int> with Foo {
//	Int bar() {
//		return this~ + 5;
//	}
// }
```

### The dollar type

`$` is a special type which can only be used inside model declarations. It corresponds to the type that implements the model and may be
pronounced as "self". The dollar type can only be used in the return values of methods.

```
model Increment {
	$ plusone();
}
```

When extending a type with a model which uses dollar types in method signatures, each dollar should be substituted with the type that
we are extending:

```
extend Int with Increment {
	Int plusone() {
		return this + 1;
	}
}
```

The return value of a dollar-marked method depends on the type of the variable (or other expression) that contains the object.

```
Void test(Int i, Increment&String j, Increment k) {
	var i2 = i.plusone(); // i2 has type Int
	var j2 = j.plusone(); // j2 has type Increment&String
	var k2 = k.plusone(); // k2 has type Increment
	puts(i2.toString());
	puts(j2.toString());
	// there's not much we can do with k2: the only thing
	// we know about it is that it has method plusone()
}
```

### Intersection types

An intersection type is a type thats domain contains all types that are extended with specified models. For example,
both `Array` and `Str` values can be casted to `HasSize&String` values.

The current implementation of the intersection types is a bit problematic as the compiler must generate a lot of unnecessary
and long functions to make intersection types work. The length and number of the functions is exponentially proportional to the number
of models in the intersection type. It is not healthy to use more than three models per type. A type with six models can generate up to
three hundred thousand lines of C-code.

### Generic types

Both models and structures can be generic, that is, they may have one or more type parameters. Generic types are resolved compile-time
and two versions of the same generic type with different type arguments are different types at run-time. Generic types are neither
covariant nor contravariant.

Each type parameter must be preceded with an `@`-character:

```
struct Box<@T> {
	@T value;
}
```

### Static duck typing

Generic methods in PScript support a feature called _static duck typing_. Similar semantics can be found in eg. C++.

Duck typing means that a method accepts arguments not based on their type but on the method names they implement. The
term comes from the saying "if it looks like a duck, it is a duck".

PScript allows calling arbitrary methods on values that have a type parameter as their type. Lets look at an example. Here
we define an "adding printer", a printer that adds a value to the object to be printed before printing.

```
model AddingPrinter<@T> {
	Void print(@T value);
}

const struct AddingPrinterImpl<@T> { @T prefix; }

extend AddingPrinterImpl<@T> with AddingPrinter<@T> {
	Void print(@T value) {
		puts((this::prefix + value).toString());
	}
}
```

We can use `AddingPrinter` like this:

```
var intPrinter = new AddingPrinterImpl<Int> { 5 };
intPrinter.print(3); // prints 8

var strPrinter = new AddingPrinterImpl<String> { ":) " };
strPrinter.print("hello"); // prints :) hello
```

See how the printer uses either integer addition or string concatenation depending on the type argument?
The type `@T` is resolved at compile time to be either `Int` or `String` and an appropriate method call
is made.

This process is completely based on method _names_: the models that declare the methods are unimportant.
In this case the `Int` version of `operator +` is a built-in method and the `String` version is declared in
non-related `Summable` model.

What happens if we try to make an instance of `AddingPrinterImpl<Bool>`?

```
var boolPrinter = new AddingPrinterImpl<Bool> { true };
boolPrinter.print(true);
```

`Bool` does not have method `operator +`. The compiler will give the following error:

```
[in _AddingPrinterImpl1Bool_print] error: type Bool does not have method `op_add'
```

It looks a bit complicated due to mangled names (they will be demangled in a later version), but
it basically says that it can't compile method `AddingPrinterImpl<Bool>.print` because
`Bool` does not have the required operator.

## Type system reference

### Primitive datatypes

* `Bool`. Either `True` or `False`.
* `Char`. Internally a 8-bit integer. Currently does not support any operators.
  The only use of this type is to define `Str`.
* `Int`. A 32-bit integer.
* `Pointer<@T>`. A pointer to a list of values of type `@T`.
* `Str`. An alias for `Char*`.

### Standard structures

#### The `Array` structure

```
const struct Array<@T> {
	Int len;
	@T* ptr;
}
```

`Array` is a wrapper for the low-level `Pointer` type. Unlike `Pointer` it contains
the length of the list of elements and can be used to iterate over all items it contains.
It has a special syntax `[v_1, v_2, ..., v_n]` which can be used to initialize it.
It also supports `[]` and `[]=` -methods.

`T[]` is equilevelent to `Array<T>`.

#### The `ArrayList` structure

```
struct ArrayList<@T> {
	@T[] elements;
}
```

An array with mutable size. Supports `add` and `remove`-operations.

### Standard models

#### The `Appendable` model

```
model Appendable<@T> {
	$ append(@T element);
}
```

Implementing types: `Array`, `ArrayList`.

#### The `Container` model

```
model Container<@T> {
	Bool contains(@T value);
}
```

Implementing types: `Array`, `ArrayList`.

#### The `HasSize` model

```
model HasSize {
	Int size();
}
```

Things that have size implement this model.

Implementing types: `Array`, `ArrayList`, `Str`, `String`

#### The `List` model

```
model List<@T> : HasSize {
	@T operator [] (Int index);
	Void operator []= (Int index, @T value);
	Void add(@T value);
	@T remove(Int index);
}
```

Implementing typs: `ArrayList`

#### The `Summable` model

```
model Summable<@T> {
	$ operator +(@T t);
	@T sumIdentity();
}
```

Although the type system does not yet support this feature, it should be noted that `@T` shall be same as `$`, eg. the implementing type.
Notably `Int` is not extended with this model due to some problems the author is having with the type system.

Implementing types: `ArrayList`, `Str`, `String`

#### The `String` model

```
model String {
	Str toString();
}
```

All types that can be converted to `Str` should be implement this model.
`String` can generally be used in place of `Str` to enable implicit type conversion.

Implementing types: `Array`, `ArrayList`, `Int`, `Str`
