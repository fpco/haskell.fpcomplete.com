# Haskell learning syllabus

**NOTE** This page is slightly out of date, and will be updated in the
near future. Most of the content is still accurate.

Over the years, FP Complete has put together a syllabus of Haskell
concepts we believe make up a solid level of productivity. Much of
this is captured in our [Applied
Haskell](https://github.com/fpco/applied-haskell) training course.

This outline provides a wide array of content, focused on practical
lessons towards writing real-world applications. It presumes a basic
knowledge of Haskell. If you're new to Haskell, we recommend the book
[Haskell Programming from First Principles](http://haskellbook.com/).

For more general purpose learning information, check out our [Learn
Haskell](/learn) page.

## Core information

You understand the basics of Haskell syntax and some common library
functions. This section should get you up to speed with many commonly
used features of Haskell, to provide a foundation for understanding
code in general, and to follow the rest of this outline in particular.

* [Get Started with Stack](/get-started)
* [Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell)
    * [Asynchronous Exception Handling in Haskell](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell) (deeper dive)
* [Operator Glossary](/tutorial/operators)
* [Synonyms in base](/tutorial/synonyms)
* [Common language extensions](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions)
* [Common typeclasses](https://wiki.haskell.org/Typeclassopedia)
* [All About Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness)

## Data structures

Covers some of the most commonly used data structures in Haskell, and
the libraries providing them.

* [String types (text and bytestring)](/tutorial/string-types)
* [vector](https://haskell-lang.org/library/vector)
* [containers](https://haskell-lang.org/library/containers)

## General patterns

This section demonstrates some common Haskell coding patterns, how
they work, when they're useful, and possible pitfalls.

* [Monad Transformers](/library/transformers)
* unliftio
* [Covariance, contravariance, and positive and negative position](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)
* Continuation Passing Style
* Builders and difference lists

## Testing

* QuickCheck
* hspec, tasty, others?
* [Hspec/doctest with Cabal as test framework](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md)

## Serialization

* store
* binary/cereal
* blaze-builder/bytestring-builder
* blaze-html
* attoparsec
* [aeson](https://haskell-lang.org/library/aeson)
* yaml
* xml-conduit/html-conduit
* base16-bytestring/base64-bytestring

## Standard programming needs

* [HTTP client library](/library/http-client)
* [Command line argument parsing optparse-applicative](/library/optparse-applicative)
* cryptonite
* time
* Random number generation (mwc-random)
* Regular expressions with regex-applicative

## System programming

* [typed-process](/library/typed-process) for launching and interacting with subprocesses
* Network and Socket I/O
* Writing scripts (turtle, Shelly) (see [How to Script with Stack](/tutorial/stack-script))

## Streaming data (conduit)

* [Conduit tutorial](/library/conduit)

## Concurrency and parallelism

Simon Marlow's book [Parallel and Concurrent Programming in
Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)
is a highly recommended read on the subject. In addition, we have the
following topics:

* [The async package](/library/async)
* Common concurrency patterns (e.g., the auto-update package)
    * "Passing the baton" to control flow of execution between threads, using `MVar` for two threads, `TVar` for multiple threads
* Concurrency patterns: worker threads, signals, blocking on TVars
* STM: blocking semantics around mutable variables
* resource-pool
* handling errors (SlaveThread), restarting tasks, timeouts and other common patterns

## Web programming

Web programming is another topic with many different approaches. Like
streaming data, we need an overview of the different options, and then
a drilldown on individual approaches. For now:

* [Web Application Interface](http://www.yesodweb.com/book/web-application-interface)
* [Yesod Web Framework](http://www.yesodweb.com/book)

## rio

* [rio](https://github.com/commercialhaskell/rio#readme)

## Advanced topics

* [Primitive Haskell](/tutorial/primitive-haskell)
* [Evaluation order and state tokens](https://wiki.haskell.org/Evaluation_order_and_state_tokens)
* Cabal trickery for backwards compatibility: Cabal CPP macros. Paths module. Flags. How to test for windows. Defaulting macros for ghci. Flags to either use new library version or another package (bytestring-builder) and set a CPP variable.
* [Constraint trick for instances](http://chrisdone.com/posts/haskell-constraint-trick); perhaps we can have a section for common type patterns (newtypes an obvious one, and also for example using Rank-N types to store a generic function in a data structure, versus existential types to store generic data).

## Database Programming

* persistent
* esqueleto
* opaleye
* mysql-simple
* postgresql-simple
* [Haskell Relational Record](http://khibino.github.io/haskell-relational-record/)

## Debugging/optimizing

* hlint
* Debugging
* Profiling
* Finding space leaks
* Strictness annotations
* Pragmas (UNPACK, INLINE, ...)
* Heap profiling
* Looking at GHC core

## Code and project structuring

As a project grows, there are many "patterns" that might save
developer some time by just doing some restructuring work. Some tricks
might save development time, while others help to re-compile less.

* Common `Imports.hs` module
* Multiple executables depending on common library

## Application infrastructure and support

As part of "commercial haskell", I think it would be great to have
both, haskell-specific and non-specific description with examples in
haskell for how do you manage all the standard needs for your
application infrastructure and support. Some topics would include:

* Deployment & service management
* Monitoring and metrics (ekg, bosun)
* Log handling techniques and the `say` package
