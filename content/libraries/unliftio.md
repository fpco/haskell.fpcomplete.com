---
title: Lifting and unlifting IO actions
---

Many actions in common Haskell libraries are tied to the `IO` type. For example:

```haskell
readFile :: FilePath -> IO ByteString
```

There are times when we want to use these actions in [monad
transformer stacks](/library/transformers) that sit on top of
`IO`. For many cases, we can use the `MonadIO` class and its `liftIO`
method to make this possible:

```haskell
readFileLifted :: MonadIO m => FilePath -> m ByteString
readFileLifted fp = liftIO (readFile fp)
```

However, there are two problems with this:

1. There are some classes of functions for which `liftIO` is
   insufficient. For example, try to convert `timeout :: Int -> IO a
   -> IO (Maybe a)` into `timeout :: MonadIO m => Int -> m a -> m
   (Maybe a)`. It can't be done.
2. It adds a lot of tedium to our code to insert `liftIO` calls where
   needed

The `unliftio` library solves both of these problems, as well as a few
others we'll get to. It also serves as the basis for the [`rio`
standard library](/library/rio).

Let's start with a high-level approach to the first problem, explain
the solution to the second problem, and then give some more details.

## Unlifting

`MonadIO` works for `readFile`, but doesn't work for `timeout`. The
reason is that `readFile` has `IO` in only _positive position_,
whereas `timeout` has `IO` in both positive and _negative
position_. If those terms are unfamiliar and you'd like to learn more,
check out our blog post on [Covariance and
Contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance). In
short: `readFile` only has `IO` as output, while `timeout` takes `IO`
as input as well.

`liftIO` lets us convert an `IO a` to an `m a`. We need to do the
opposite: convert an `m a` into an `IO a`. And that's precisely what
the `MonadUnliftIO` class provides:

```haskell
timeoutUnlifted :: MonadUnliftIO m => Int -> m a -> m (Maybe a)
timeoutUnlifted duration action = withRunInIO $ \run -> timeout duration (run action)
```

For minimal dependency footprint, the `MonadUnliftIO` typeclass and a
few helper functions are defined in the
[`unliftio-core`](https://www.stackage.org/package/unliftio-core)
library. But we're going to be talking about the batteries-loaded
`unliftio` library, which is where we solve the second problem.

## Batteries loaded

Using `liftIO` and `withRunInIO` directly is possible, but
tedious. The goal of the `unliftio` library is to instead collect
together common functionality and lift/unlift it from `IO` to either
`MonadIO` or `MonadUnliftIO`.

If you check out the [module list for the `unliftio`
library](https://www.stackage.org/package/unliftio), you'll see a
collection of functionality available. The tutorials on this site use
these unlifted variants in place of the originals wherever
possible. Some related tutorials you may be interested in:

* [async](/library/async)
* [stm](/library/stm)
* [Exception handling](/tutorial/exceptions)

That last bullet brings us to our final caveat.

## Exception handling semantics

Unlike the other modules in this library, the `UnliftIO.Exception`
module not only applies the `MonadIO` and `MonadUnliftIO` typeclasses
to existing functions. It also changes the semantics of functions
available in `Control.Exception`. This is to simplify proper exception
handling, especially in the presence of asynchronous
exceptions. Please see the [exception handling
tutorial](/tutorial/exceptions) for more information.

## The `UnliftIO` module

As a convenience, the `UnliftIO` module reexports a lot of common
functionality from the other modules in this library. You may find it
convenient to add `import UnliftIO` to your code. Alternatively,
consider using [`rio`](/library/rio), which also reexports the
`UnliftIO` module.

## Limitations

This tutorial has intentionally avoided discussing the design
decisions in the `unliftio` library and the `MonadUnliftIO`
typeclass. There are limitations versus alternatives. If you'd like
more information, please read [the `unliftio`
README](https://github.com/fpco/unliftio#readme).
