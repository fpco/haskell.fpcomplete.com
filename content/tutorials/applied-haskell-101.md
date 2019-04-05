---
title: Applied Haskell 101
---

This page is the beginning of the Applied Haskell course. For more
information, see [the syllabus](/syllabus). The goal here is to
provide some basic information on:

* What "Applied Haskell" means
* The tools you'll be using
* How to get more help

## What is Applied Haskell?

Applied Haskell is a course developed over the years by the FP
Complete team. It focuses on bridging the gap between Haskell basics
to the ability to write production-grade Haskell applications. In our
experience, there are a few important pieces necessary to make this
happen:

* Understand the tooling
* Knowledge of the basic libraries
* Understand how best to structure an application
* Understand how to get more help and information

This course will _not_ teach you every detail of every library. But
hopefully by the end you'll be able to find any library you need, read
its documentation, and use it.

## About Haskell

Haskell is in many ways a revolutionary language. It was innovative
when it first came on the scene decades ago, and remains innovative
today. Haskell continues to add cutting-edge features, especially at
the type level, to an industrial-strength language.

We don't care about that here. In Applied Haskell, we're going to
treat Haskell as a "getting things done" language. To borrow phrase,
we're going to be "brutally pragmatic." Haskell delivers a huge amount
of value with a subset of its features. We're going to focus on those
features and the tools and libraries necessary to take advantage of
them. We'll also be covering Haskell idioms and patterns so you can
pick up common code more quickly, and discuss some runtime system
idiosyncrasies.

You should know the basics of Haskell to follow along properly. You do
not need to know advanced language theory, category theory, or any
other surprising topics.

## Tooling

This course will assume that you'll be using the [Stack build
tool](https://haskellstack.org). Working with other tooling like
[Cabal](https://www.haskell.org/cabal/) or
[Nix](https://github.com/haskell-nix) will work as well, but may
require more fiddling. For following this course, it's recommended to
use Stack to avoid wasting time.

### Stack

Stack is a build tool for Haskell. It can also install your compiler
toolchain (and does so by default). It focuses on reproducible build
plans. As mentioned, there are other tools, but we'll be using
Stack. It comes first in this list as it's the first one you should
install, by following the [get started instructions](/get-started).

If you already have Stack on your machine, you can usually upgrade to
the latest version by running `stack upgrade`. Now that you have
Stack, we can move on to...

### GHC

GHC is the de facto standard Haskell compiler. You can install a
compiler version by running a command line:

```
$ stack setup ghc-8.4.4
```

Typically you won't have to do this explicitly. When Stack notices
you're missing a GHC version, it will install it for you.

GHC can both compile code and run it interpreted, as well as provide
you with an interactive REPL. You can do those directly with,
respectively, the `ghc`, `runghc`, and `ghci` executables. Stack does
not place these executables on the user PATH, so if you'd like to run
them, you'll typically use `stack ghc`, `stack runghc`, and `stack
ghci`.

If you'd like more information on interacting in these three ways,
check out:

* [How to Build](/tutorial/stack-build)
* [How to Script](/tutorial/stack-script)
* [How to Play](/tutorial/stack-play)

The lessons in this course mostly stick to using the script approach.

### Cabal build system

Apologies in advance, the terminology is about to get a bit complicated.

Stack is built on top of the Cabal build system. When you install a
package with Stack (e.g., by running `stack build conduit`), you're
installing a Cabal package. Cabal refers to a few different things:

* A file format for `.cabal` files, which provide metadata on a
  package
* A design of a build system
* A library for performing these builds, called `Cabal`
* A command line executable. The executable is called `cabal`, and the
  package it comes from is called `cabal-install`.

When using Stack, you'll use the `stack` executable instead of the
`cabal`/`cabal-install` executable. Other than that, you're using the
same Cabal components. Both tools (and Nix, for that matter) can
install the same packages, so there's high overlap.

The one final complication is that there's another file format called
hpack. This file format is YAML based (with files called
`package.yaml`), and is used to generate `.cabal` files. Stack
supports these out of the box, and many Stack projects will provide
`package.yaml` files, and have auto-generated `.cabal` files.

More information on all of this at:

* [stack.yaml vs cabal package files](https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/)
* [Stack architecture document](https://docs.haskellstack.org/en/stable/architecture/#parsing-cabal-files)

Note that these are more advanced issues which you probably won't run
into immediately. It's good to know where more information is when you
do hit a roadblock.

### Stackage

There are thousands of Haskell packages available online on
[Hackage](https://hackage.haskell.org/), the central open source
package repository. In order to make it easier to find a working build
plan, and provide consistent behavior for multiple users, the
[Stackage project](https://www.stackage.org/) provides curated
snapshots of packages which are tested to work together. There are
two different sets of Stackage snapshots:

* Long Term Support (LTS) Haskell comes out with snapshots about once
  per week. They are named lts-x.y (e.g.,
  [lts-12.21](https://www.stackage.org/lts-12.21)). Within a major
  version number, we mostly include non-breaking changes to
  packages. This makes it relatively easy to upgrade to more recent
  LTS minor versions.
* Stackage Nightly builds are daily and provide no compatibility
  guarantees. They are named nightly-YYYY-MM-DD (e.g.,
  [nightly-2019-03-10](https://www.stackage.org/nightly-2019-03-10/)).

stackage.org provides a few resources. Some notable ones:

* You can perform a Hoogle search, which allows you to search an
  entire snapshot for names and types. For example, you can [search
  for `map`](https://www.stackage.org/lts/hoogle?q=map).
* You can browse API documentation for a specific package, e.g. [the
  docs for conduit](https://www.stackage.org/package/conduit).

Note that Hackage also provides API documentation. I recommend using
Stackage's in general for two reasons:

1. When you use Stackage's docs, you're getting docs for a specific
   Stackage snapshot. This means that any links to dependencies is the
   same dependency used in that snapshot.
2. Sometimes the doc builder can have trouble building docs on
   Hackage, such as due to not finding a working build plan. With few
   exceptions, if a package is in a Stackage snapshot, it means that a
   working build plan was found.

### hlint

The `hlint` tool is a great linter. It not only provides ways to
improve your code, but in the process can teach you about better ways
of writing code you weren't aware of. Install `hlint` with:

```
$ stack install --resolver lts hlint
```

To see an example of `hlint`'s power, save the following to `Main.hs`
and then run `hlint Main.hs`:

```haskell
main :: IO ()
main = do
  mapM putStrLn ["Hello", "World"]
  pure ()
```

### Code formatting

Unfortunately in the Haskell world, code formatting isn't quite at the
level of rigour as, say, Go. There are different coding styles that
are commonly used, and (at least to my knowledge at time of writing)
none of the tools can be guaranteed to perfectly format every valid
Haskell program. Two commonly used tools are:

* [stylish-haskell](https://www.stackage.org/package/stylish-haskell),
  which cleans up things like import statements
* [hindent](https://www.stackage.org/package/hindent), which does a
  more traditional code formatting.

### Editor integration/IDEs

There are lots of different pieces of integration for lots of editors,
with various IDE support. This is a topic that's often in flux,
depending on which tools have upgraded to the most recent release of
GHC. For learning, I recommend: stick to simple syntax
highlighting. If you find integrations that work well for you, great!
The unfortunate reality is that what works for one person doesn't
reliably work for others.

One tool which I will mention is
[ghcid](https://www.stackage.org/package/ghcid). It is fully usable
from the command line (in fact, that's its intended use case), and
provides fast auto-rebuild support using the GHC
interpreter. Basically:

1. Run `stack install --resolver lts ghcid`
2. Go into a directory with a Stack project
3. Run `ghcid`
4. Fix all the bugs that appear on your screen
5. ...
6. Profit!

### Web resources

There are many resources available for Haskell across the web. In
addition to some of the links above:

* [The learn page on this site](/learn)
* [The community page on this site](/community)
* [haskell.org](https://www.haskell.org/)
* [The Yesod Web Framework](https://www.yesodweb.com/)
* [School of Haskell](https://www.schoolofhaskell.com)

## Nomenclature

Before diving into the rest of this course, a quick recap of some
nomenclature to be familiar with:

### Data types

* Type synonyms: `type String = [Char]`
* Newtypes: `newtype Age = Age Int` or `newtype Age = Age { unAge :: Int }`
* Data declaration
    * Product type: `data Person = Person Name Age`
    * Product w/record: `data Person = Person { personName :: Name, personAge :: Age }`
    * Enum: `data Fruit = Apple | Banana | Pear`
    * Mix them (proper sums): `data Age = UnknownAge | KnownAge Int`

Terms:

* `Fruit` is a _type constructor_
* `Fruit` is also a _type_
* `Apple`, `Banana`, and `Pear` are _data constructors_
* `Person` is both a type and data constructor
    * They live in separate namespaces, that's fine

Type variables:

* `data Maybe a = Nothing | Just a`
* `a` is a type variable
* `Maybe` is a type constructor
* However, `Maybe` is _not_ a type!
* `Maybe` has kind `Type -> Type`, aka `* -> *`
* `Maybe Int` is a type

More on data types in next section.

### Partial/total functions

__Bottom value__: when evaluated, throws a runtime exception or loops
infinitely

```haskell
bottom1 = error "I'm bottom!"
bottom2 = undefined
bottom3 = _ -- probably fails at compile time
bottom4 = let x = x in x -- infinite loop, runtime may detect it
```

__Total function__ produces a non-bottom output for all non-bottom
input.

__Partial function__ may produce a bottom output for some non-bottom
input.

Partiality can sneak in:

* Partial pattern matches
* Lambdas on sum types
* Infinite loops
* Turn on `-Wall`!

### Language extensions

50% of all production Haskell code is language extension and import
lines

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
```

Added at the top of your file. Basic structure:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- Above two lines for scripts, we'll cover that below
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ... #-}
{-# OPTIONS_GHC -Wall #-} -- better in package.yaml file, below
module Main (main) where

import Control.Monad (when)
-- import ...

main :: IO ()
main = putStrLn "Finally, some actual code!"
```

See: https://github.com/commercialhaskell/rio#language-extensions

## Syntactic sugar everywhere

Function calls:

```haskell
foo x y = ...
foo = \x y -> ...
foo = \x -> \y -> ...
```

All the same, allow for partial function application (not the same as
partial functions!).

Pattern matching:

```haskell
fromMaybe def Nothing = def
fromMaybe _def (Just x) = x

fromMaybe def mx =
  case mx of
    Nothing -> def
    Just x -> x

{-# LANGUAGE LambdaCase #-}
fromMaybe def = \case
  Nothing -> def
  Just x -> x
```

Also:

```haskell
if x then y else z

case x of
  True -> y
  False -> z
```
