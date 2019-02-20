---
title: Recommended Haskell libraries
---

We recommend using
[`rio`](https://github.com/commercialhaskell/rio#readme) as a standard
library for Haskell. This provides a coherent set of libraries
providing data structures and common practices, while removing some
warts like partial functions.

`rio` provides the following common needs out of the box by
reexporting existing best-practices libraries:

* String types: use `ByteString` for binary data and `Text` for
  textual data. For more information, see [the String Types
  tutorial](/tutorial/string-types)
* Packed memory representations via [the vector library](/library/vector)
* Map and Set data types via [the containers library](/library/containers)
* External process interaction via [the typed-process library](/library/typed-process)
* Concurrency via [Software Transactional Memory](/library/stm) and
  the [async library](/library/async)

## Outside of rio

`rio` intends to act as a standard library, but does not include many
common pieces of functionality to avoid bloating its footprint. Other
common libraries are:

* [hspec](https://www.stackage.org/package/hspec) for testing
* [conduit](/library/conduit) for streaming data
* [http-client](/library/http-client) for making HTTP requests
* [Yesod](https://www.yesodweb.com) for creating HTTP servers and web applications

## Beyond libraries

We recommend checking out our [Haskell best
practices](/tutorial/best-practices) for further information on how we
recommend writing Haskell code.
