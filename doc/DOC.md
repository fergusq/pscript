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

A model can have prerequisites, which means that all types that are extended with the model must also be extended with the
prerequisite. A model doesn't however need to implement its own prerequisites.

```
model Stack<@T> : Pushable<@T> {
	@T pop();
}
```

That means that in order to be a Stack, the type must first be a Pushable (i.e. support `push` operation) and then implement
the `pop` method.

Most models contain only one method. This allows great flexibility, but comes with a price:
the standard library becomes very complicated (see picture at the middle of this page).

The most feature-rich type, ArrayList, implements currently 12 different models.

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

_The example below does not work in the current version of PScript, as the string concatenation operation has been changed_
_from_ `+` _to_ `++`_._

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
[in AddingPrinterImpl<Bool>.print] error: type Bool does not have method `op_add'
```

## Type system reference

![A graph of the standard types](https://github.com/fergusq/pscript/raw/master/doc/types.svg "The standard types")

PScript's type system is rather different than in more notable languages C++ or Java. All currently implemented types and
their relations are presented in the graph above. Types in boxes are primitive built-in types. Types in hexagons are
structures and types in ovals are models, all types of these king are declared in the standard library.

A solid black arrow means that type implements a model. The source of the arrow is said to
be the _supertype_ and the destination of the arrow is the _subtype_. That is not inheritance, there is no
inheritance in PScript. It is possible and common that a type implements a model but its subtypes do not, eg. String implements
HasSize but Int does not.

There are also dotted arrows. The source of the arrow is said to be a _prerequisite_ of the destination of the arrow.
All subtypes of a type should and must implement all prerequisites of it. The type itself that has the prerequisite, however,
does not need to implement it. This is very common, in fact Iterator is the only type that implements its prerequisite StreamSource.
A type must also implement the prerequisites of the prerequisites, so for example ArrayList implements Container although it is 
not a direct prerequisite of VariableSizeList.

Only the solid arrows that can not be inferred from prerequisites are drawn.

### Primitive datatypes

* `Bool`. Either `true` or `false`.
* `Char`. Internally a 8-bit integer. Currently does not support any operators.
  The only use of this type is to define `Str`.
* `Func<@R, @P...>`. Type of anonymous functions.
  Has syntax sugar form `@P->@R` (for one parameter) or `(@P, @Q, ...) -> @R` (for many or none parameters).
* `Int`. A 32-bit integer.
* `Pointer<@T>`. A pointer to a list of values of type `@T`. Has syntax sugar form `@T*`.
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

#### The `ArrayIterator` structure

```
struct ArrayIterator<@T> {
	@T[] elements;
	Int location;
}
```

A basic implementation of `Iterator`.

#### The `ArrayList` structure

```
struct ArrayList<@T> {
	@T[] elements;
}
```

An array with mutable size. Supports `add` and `remove`-operations.

#### The `Fold` structure

```
struct Fold<@T, @U> {
	@U collection;
	(@U, @T)->@U collector;
}
```

#### The `Forall` structure

```
const struct Forall<@T> {
	@T->Void callback;
}
```

### Standard enumerations

#### The `Maybe` enumeration

```
enum Maybe<@T> {
	Just(@T),
	Nothing
}
```

Represents a value that may or may not be present. Implements both `Container` and `Eq`.

### Standard models

#### The `Appendable` model

```
model Appendable<@T> : CopyCollector<@T> {
	$ append(@T element);
}
```

Implementing types: `Array`, `ArrayList`.

#### The `Collector` model

```
model Collector<@T> {
	StreamOperation<@T, $> collect();
}
```

Implementing types: `ArrayList`, `CopyCollector`

#### The `Container` model

```
model Container<@T> : StreamSource<@T> {
	Bool contains(@T value);
}
```

All collections that have finite size should implement this model.

Implementing types: `Array`, `ArrayList`, `Maybe`.

#### The `CopyCollector` model

```
model CopyCollector<@T> {
	StreamOperation<@T, $> collectToCopy();
}
```

Implementing types: `Array`, `ArrayList`

#### The `HasSize` model

```
model HasSize {
	Int size();
}
```

All collections that have finite but variable size should implement this model.

Implementing types: `Array`, `ArrayList`, `Str`, `String`

#### The `Iterator` model

```
model Iterator<@T> {
	@T next();
	Bool hasNext();
}
```

Implementing types: `ArrayIterator`

#### The `List` model

```
model List<@T> : HasSize, Container<@T> {
	@T operator [](Int index);
	Void operator []=(Int index, @T value);
}
```

Collections that appear as ordered sequences of finite size should implement this model.

Implementing types: `Array`, `ArrayList`

#### The `Optional` model

```
model Optional<@T> : Container<@T> {
	@T orElse(@T value);
	@T orElseGet(()->@T getter);
	Bool isPresent();
}
```

Implementing types: `Maybe`

#### The `Pushable` model

```
model Pushable<@T> : Collector<@T> {
	Void push(@T element);
}
```

Implementing types: `ArrayList`

#### The `Stack` model

```
model Stack<@T> : Pushable<@T>{
	@T pop();
}
```

Implementing types: `ArrayList`

#### The `StreamSource` model

```
model StreamSource<@T> {
	Iterator<@T> iterator();
}
```

All collections that are iterable should implement this model. Provides a starting point for pipeline calculations.

Implementing types: `Array`, `ArrayList`, `ArrayIterator`, `Iterator`, `Maybe`

#### The `StreamOperation` model

```
model StreamOperation<@T, @U> {
	@U operator |(StreamSource<@T> source);
}
```

Takes a stream of `@T` and returns `@U`, which can be either a new `StreamSource` or the final result of pipeline.

`operator |` is an unique operator, as it is the right operand thats method is actually called.

Implementing types: `Fold`, `Forall`, `Func`

#### The `Summable` model

```
model Summable<@T> {
	$ operator ++(@T t);
	@T sumIdentity();
}
```

All types of collections that can be appended to other collections of the same type should implement this model.

Although the type system does not yet support this feature, it should be noted that `@T` shall be same as `$`, eg. the implementing type.

Implementing types: `Array`, `ArrayList`, `Str`, `String`

#### The `String` model

```
model String {
	Str toString();
}
```

All types that can be converted to `Str` should be implement this model.
`String` can generally be used in place of `Str` to enable implicit type conversion.

Implementing types: `Array`, `ArrayList`, `Bool`, `Int`, `Str`

#### The `VariableSizedList` model

```
model VariableSizeList<@T> : List<@T>, Stack<@T> {
	Void add(Int index, @T value);
	@T remove(Int index);
}
```

Collections that are lists and have variable size should implement this model.

Implementing types: `ArrayList`
