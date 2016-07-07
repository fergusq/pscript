PScript documentation
=====================

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

#### The `List` structure

```
struct List<@T> {
	@T[] elements;
}
```

A mutable array.

### Standard models

#### The `HasSize` model

```
model HasSize {
	Int size();
}
```

Things that have size implement this model.

Implementing types: `Array`, `List`, `Str`, `String`

#### The `Summable` model

```
model Summable<@T> {
	$ operator +(@T t);
	@T sumIdentity();
}
```

Althought the type system does not yet support this feature, it should be noted that `@T` shall be same as `$`, eg. the implementing type.
Notably `Int` is not extended with this model due to some problems the author is having with the type system.

Implementing types: `List`, `Str`, `String`

#### The `String` model

```
model String {
	Str toString();
}
```

All types that can be converted to `Str` should be implement this model.
`String` can generally be used in place of `Str` to enable implicite type conversion.

Implementing types: `Array`, `Int`, `List`, `Str`
