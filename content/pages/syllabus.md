---
title: Applied Haskell Syllabus
---

Applied Haskell is a commercial training programming focusing on
teaching _intermediate Haskell_. The goal is to help someone move from
knowing Haskell basics to being able to write commercial software,
with enough knowledge to pick up any new skills needed on demand.

**If you're new to Haskell**, please check out our [learning
page](/learn) for introductory material.

The content below is freely available. If you're interested in
participating in a class teaching this material, please [check out our
training page](https://www.fpcomplete.com/training).

## Course format

If you're not participating in a instructor-led course, feel free to
skip this section.

This course is typically taught over a two day period by an FP
Complete Haskell engineer. There is more material available here than
can be taught in two days, so some content is typically skipped in the
classroom, with student interest guiding what we focus on. This course
also has a one day subset.

In order to maximize the value of this course, we strongly recommend
that you:

* Ensure you meet the prerequisites mentioned below
* Set up your system before attending the class following the instructions below
* At least skim through the material below before the course
* Perform the exercises during the course
* Take time after the course to review the material again and take
  another crack at any exercises you were not successful at

## Prerequisites

You should already be comfortable with Haskell syntax, common control
structures, and typeclasses. For the most part: if you understand how
to use monads, you're ready for this course. Though we'll cover it in
more detail during the course, you should also read [all about
strictness](/tutorial/all-about-strictness), as experience has shown
this to be a topic that trips people up often.

As a self test, we recommend ensuring you're able to do the following:

* Define `fmap` in terms of `>>=` and `return`
* Define `fmap` in terms of `<*>` and `pure`
* Define `>>=` in terms of `Applicative` instance and `join`
* Define `join` in terms of `>>=`
* Explain the intuition behind: what can you do with `Monad` and not `Applicative`?

If you're looking to read up on this, our recommendations are:

* [Haskell Programming from First Principles](http://haskellbook.com/)
  for a thorough coverage of all relevant topics
* For a quick crash course on `Functor`/`Applicative`/`Monad`, see
  [Michael's blog
  post](https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads)

Let the course material begin!

## System setup

We will be using the Stack build tool extensively throughout this
course. To get your system set up:

1. Download and install Stack following [our Getting Started
   guide](/get-started). If you already have Stack installed, run
   `stack upgrade` to ensure you have the latest version.
2. We're going to be using LTS Haskell version 12.21. (We'll explain
   what LTS Haskell is in the material below.) You may as well install
   an unnecessarily broad number of packages right off the bat:

   ```
   $ stack build --resolver lts-12.21 classy-prelude-yesod lens rio yesod-test foldl microlens-platform wai-conduit hspec`
   ```

     * You may also find it convenient to run `stack config set resolver lts-12.21`
       from outside of a project to set your global resolver to match.

3. Make sure you can run the script below successfully. Save it to a
   file ending with `.hs` and then run `stack filename.hs`. On
   non-Windows systems, you can also do `chmod +x filename.hs &&
   ./filename.hs`

   ```haskell
   #!/usr/bin/env stack
   -- stack --resolver lts-12.21 script
   main = putStrLn "Hello World!"
   ```

Note that the comment on line 2 above is necessary!

## Applied Haskell 101

We'll start off with a high level overview of the content we're going
to cover, our approach to Haskell, and tooling to be aware of.

[Applied Haskell 101](/tutorial/applied-haskell-101)

## The RIO approach

The `rio` library codifies much of our recommended best practices. It
includes an approach to structuring applications, a standard library,
a Prelude replacement, and more.

[The `rio` library](/library/rio)

## Mutability and concurrency

Haskell is immutable-by-default, but that default can be
overridden. And this mutability oftens pops up in the context of
concurrency. This is one of Haskell's greatest strengths, and we'll
cover it now.

* [Mutable variables](/tutorial/mutable-variables)
* [The async library](/library/async)

## Strictness, laziness, and evaluation

One of the hallmarks of Haskell is lazy evaluation. Understanding how
this works, when to use strictness, and how to avoid space leaks are
vital to production quality Haskell code.

* [Data types](/tutorial/data-types)
* [All About Strictness](/tutorial/all-about-strictness)

## Data structures

Lists are a common data structure in Haskell, but they are often
overused. It's vital to understand other common data structures. This
section intentionally comes after the strictness section, as the
former is a prerequisite for this material.

* [Data Structures](/tutorial/data-structures)
* [String Types](/tutorial/string-types)
* [Containers](/library/containers)
* [Vector](/library/vector)
* Builders and difference lists
* Let's revisit that data structure quiz...

## Exception handling

Exceptions are built into the Haskell runtime. Proper handling is
essential.

[Safe exception handling](/tutorial/exceptions)

That tutorial references two other deeper dives on the topic:

* [Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
* [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell)

## Testing

You **must test your code**. Haskell strong types help immensely, but
they are _not_ a replacement for testing. Fortunately, Haskell has
great testing libraries and tools.

* [The hspec test framework](/library/hspec)
* QuickCheck <!-- FIXME -->

## Serialization

Serialization to external binary and text-based formats.

* Binary serialization <!-- FIXME -->
* The [aeson](/library/aeson) library for JSON
* [yaml](/library/yaml)
* xml-conduit/html-conduit

## Standard programming needs

* [typed-process](/library/typed-process) for launching and interacting with subprocesses
* [HTTP client library](/library/http-client)
* [Web services](/tutorial/web-services)
* [Command line argument parsing optparse-applicative](/library/optparse-applicative)
* cryptonite
* time
* Random number generation

## Performance

Deeper understanding of Haskell performance, and how to improve it.

* [Primitive Haskell](/tutorial/primitive-haskell)
* [Profiling](/tutorial/profiling)
* [Evaluation order and state tokens](https://wiki.haskell.org/Evaluation_order_and_state_tokens)

## Streaming data

* [Conduit tutorial](/library/conduit)

## General patterns

This section demonstrates some common Haskell coding patterns, how
they work, when they're useful, and possible pitfalls.

* [Covariance, contravariance, and positive and negative position](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)
* Continuation Passing Style
* [Constraint trick for instances](http://chrisdone.com/posts/haskell-constraint-trick); perhaps we can have a section for common type patterns (newtypes an obvious one, and also for example using Rank-N types to store a generic function in a data structure, versus existential types to store generic data).
