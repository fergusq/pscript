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

Althought the type system does not yet support this feature, it should be noted that `@T` shall be same as `$`, eg. the implementing type.
Notably `Int` is not extended with this model due to some problems the author is having with the type system.

Implementing types: `ArrayList`, `Str`, `String`

#### The `String` model

```
model String {
	Str toString();
}
```

All types that can be converted to `Str` should be implement this model.
`String` can generally be used in place of `Str` to enable implicite type conversion.

Implementing types: `Array`, `ArrayList`, `Int`, `Str`
