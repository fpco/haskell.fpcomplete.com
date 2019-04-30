---
title: Crash Course to Applicative Syntax
---

The `Applicative` typeclass is ubiquitous in the Haskell world. It
sits between `Functor` and `Monad` in the typeclass hierarchy, and
allows us to represent a number of things elegantly in ways that
monads do not:

* Context-free parsing
* Concurrency
* Streaming, parallel consumption
* Validation with multiple errors

(Just to name a few.) When it comes to `Monad`s, we have the `>>=`
operator. But since it's a bit tedious to work with, Haskell provides
`do`-notation as syntactic sugar. There is a language extension called
`ApplicativeDo`, which allows us to (ab)use `do`-notation for
`Applicative` as well. However:

* Subjectively, it doesn't seem to be a very commonly used extension
* Even if you choose to use this extension in your own code, you'll
  ultimately still need to read other people's code, which will be
  using the `Applicative` functions and operators directly

This tutorial covers `Applicative` in a syntactic way. We're _not_
going to be learning the intuition behind `Applicative`, much less
`Functor` or `Monad`. For some of that motivation, check out
[Functors, Applicatives, and
Monads](https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads),
or read the appropriate section of [Haskell Programming from First
Principles](http://haskellbook.com).

## Functions and operators

You should be aware of the following:


* `pure`- wraps up a pure value into some kind of
  `Applicative`. Nowadays, this is the same as `return`, and is
  typically preferred over the latter
* `liftA2`- applies a pure function to the values inside two `Applicative`-wrapped values
* `<$>`- operator version of `fmap`. This is really part of `Functor`,
  but it's used regularly with `Applicative` syntax
* `<*>`- apply a wrapped function to a wrapped value
* `*>`- perform the effects of the thing on the left, throw away its
  result, then perform the effects of the thing on the right and wrap
  its result. (Same as `>>` in `Monad`.)
* `<*`- perform the effects of the thing on the left, then perform the
  effects of the thing on the right, throw away the _right_ result
  value, and keep the left result value. **Note bene** This is _not_
  the same as `flip (*>)`, we'll see more below.

In addition to `liftA2`, there are other functions like `liftA3`,
`liftA4`, etc. In my experience, most people use the `<$>` and `<*>`
operators instead of the `liftA*` functions. Your mileage may vary.

## Most common use cases

You'll often see code that looks like this when parsing JSON values
using the `aeson` library:

```haskell
Person
  <$> parseString "name" o
  <*> parseInt "age" o
  <*> parseTelephone "telephone" o
```

This will parse `name`, then parse `age`, then parse `telephone`. It
would take those three values, and apply the `Person` data constructor
to them. If your `Applicative` is also a `Monad` (or you turn on
`ApplicativeDo`), you can get away with `do`-notation:

```haskell
name <- parseString "name o
age <- parseInt "age" o
telephone <- parseTelephone "telephone" o
pure $ Person name age telephone
```

This second version is more "pointful": you have to define extra
variable names.

You can also write the code above as:

```haskell
liftA3 Person
  (parseString "name" o)
  (parseInt "age" o)
  (parseTelephone "telephone" o)
```

Or:

```haskell
pure Person
  <*> parseString "name" o
  <*> parseInt "age" o
  <*> parseTelephone "telephone" o
```

Assuming the `Applicative` and `Monad` laws have all been obeyed,
these are guaranteed to behave identically.

### Parsers

Parsing JSON is nice, because you don't typically have to worry about
the order in which you parse the fields from an object. Lets say
you're parsing something textual. You might use a monadic interface
with `do`-notation.

```haskell
parsePerson :: Parser Person
parsePerson = do
  string "Name: "
  name <- takeWhile (/= '\n')
  endOfLine
  string "Age: "
  age <- decimal
  endOfLine
  pure $ Person name age
```

Let's try to do this with `Applicative` from what we've seen so
far. Let's say the `string` and `endOfLine` parser combinators each
return a `()` value that we want to ignore. We can do this parser with
a helper function:

```haskell
helper :: () -> Text -> () -> () -> Int -> () -> Person
helper () name () () age () = Person name age

parsePerson :: Parser Person
parsePerson = helper
  <$> string "Name: "
  <*> takeWhile (/= '\n')
  <*> endOfLine
  <*> string "Age: "
  <*> decimal
  <*> endOfLine
```

That works, but it's tedious. We can improve the situation with `*>`
and `<*`.

```haskell
parsePerson :: Parser Person
parsePerson = Person
  <$> (string "Name: " *> takeWhile (/= '\n') <* endOfLine)
  <*> (string "Age: " *> decimal <* endOfLine)
```

Notice that we used `*>` to say "ignore the value on the left" and
`<*` to say "ignore the value on the right." However, the effects
still flow from left to right, meaning that our parsing will work as
expected.

Let's say that we redefine our `Person` datatype to take `age` as its
first field, not `name`. You may be tempted to rewrite the parser
above as:

```haskell
parsePerson :: Parser Person
parsePerson = Person
  <$> (string "Age: " *> decimal <* endOfLine)
  <*> (string "Name: " *> takeWhile (/= '\n') <* endOfLine)
```

However, this is now a different parser! It's expecting `Age` to come
before `Name` in the text it's parser. Instead, in this case, you'd
need to do something like:

```haskell
parsePerson :: Parser Person
parsePerson = (\name age -> Person age name)
  <$> (string "Name: " *> takeWhile (/= '\n') <* endOfLine)
  <*> (string "Age: " *> decimal <* endOfLine)
```

Or more code-golfy:

```haskell
parsePerson :: Parser Person
parsePerson = (flip Person)
  <$> (string "Name: " *> takeWhile (/= '\n') <* endOfLine)
  <*> (string "Age: " *> decimal <* endOfLine)
```

## More examples

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Conduit
import UnliftIO

main :: IO ()
main = do
  write2Files
  runConduitRes $
    (sourceFile "file1.txt" *> sourceFile "file2.txt") .|
    sink

write2Files = runConcurrently $
      Concurrently (writeFile "file1.txt" "this is file 1")
   *> Concurrently (writeFile "file2.txt" "this is file 2")

sink = getZipSink $
      ZipSink (sinkFile "output1.txt")
   *> ZipSink (sinkFile "output2.txt")
```
