---
title: Memory benchmarking with weigh
---

weigh is a library for doing memory benchmarking. This tool was
originally developed for one of the clients of
[FPComplete](https://www.fpcomplete.com/) for high frequency trading
sector.

## Get started

```
$ stack new mylib simple-hpack --resolver lts-13.8
$ cd mylib
```

Enable benchmarking by including the following lines in the
`package.yaml` file:

```
library:
  source-dirs: src

dependencies:
  - base >= 4.7 && < 5
  - deepseq
  - bytestring
  - text
  - conduit
  - foldl

benchmarks:
  weigh-bench:
    main: main.hs
    source-dirs: bench
    dependencies:
    - mylib
    - weigh
```

Make sure that the ghc-option for the benchmark isn't threaded. Using
the single threaded RTS is essential for deterministic results.

Create an `src/WeighCheck.hs` module which will contain various values
for which we want to do do benchmarking:

```
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module WeighCheck where

import Control.DeepSeq
import Data.ByteString
import Data.Text
import GHC.Generics

foo :: Int
foo = 3

data Foo = Foo
  { myFoo :: Int
  , myMoo :: Int
  } deriving (Generic, NFData)

fooRecord :: Foo
fooRecord = Foo 1 2

data StrictFoo = StrictFoo
  { strictFoo :: !Int
  , strictMoo :: !Int
  } deriving (Generic, NFData)

strictFooRecord :: StrictFoo
strictFooRecord = StrictFoo 3 4

data UnpackedFoo = UnpackedFoo
  { unpackedFoo :: {-# UNPACK #-}!Int
  , unpackedMoo :: {-# UNPACK #-}!Int
  } deriving (Generic, NFData)

unpackedFooRecord :: UnpackedFoo
unpackedFooRecord = UnpackedFoo 3 4

type Mytuple = (Int, Int)

myTuple :: Mytuple
myTuple = (3, 4)

fooString :: String
fooString = "hello world"

fooText :: Text
fooText = "hello world"

fooByteString :: ByteString
fooByteString = "hello world"
```

### Benchmarking values

Create `bench/main.hs`:

```
module Main where

import Weigh
import WeighCheck

main :: IO ()
main =
  mainWith $ do
    valueBenchmark

valueBenchmark :: Weigh ()
valueBenchmark =
  wgroup "Value Benchmark" $ do
    setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
    value "foo" foo
    value "FooRecord" fooRecord
    value "strictFooRecord" strictFooRecord
    value "unpackedFooRecord" unpackedFooRecord
    value "myTuple" myTuple
    value "fooString" fooString
    value "fooText" fooText
    value "fooByteString" fooByteString
    value "myList" ([1 .. 10000] :: [Int])
```

And now run in the terminal:

```
$ stack bench --file-watch --fast
Running 1 benchmarks...
Benchmark weigh-bench: RUNNING...

Value Benchmark

  Case               Allocated      Max     Live  GCs      MaxOS
  foo                        0      408      408    0          0
  FooRecord                736      608      608    0          0
  strictFooRecord          736      608      608    0          0
  unpackedFooRecord        736      608      608    0          0
  myTuple                   40      432      432    0          0
  fooString                752      616      616    0          0
  fooText                    0      408      408    0          0
  fooByteString              0      408      408    0          0
  myList               799,920  400,072  400,072    0          0
```

## Benchmarking pure functions

Let's try to benchmark `sum` function with various variants. Create
`src/WeighFunction.hs`:

```
{-# LANGUAGE ScopedTypeVariables #-}

module WeighFunction where

import Conduit
import qualified Control.Foldl as F
import Data.Conduit.List (sourceList)
import Data.List (foldl')

conduitSum :: [Int] -> Int
conduitSum xs = runConduitPure $ sourceList xs .| sumC

foldSum :: [Int] -> Int
foldSum = foldl' (\acc x -> acc + x) (0 :: Int)

ffoldSum :: [Int] -> Int
ffoldSum = F.fold F.sum
```

and add the following function in `bench/main.hs`:

```
functionBenchmark :: Weigh ()
functionBenchmark =
  wgroup "Function Weigh" $ do
    setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
    func' "base sum" sum ([1 .. 10000] :: [Int])
    func' "conduit sum" (conduitSum) ([1 .. 10000] :: [Int])
    func' "FoldL sum" (foldSum) ([1 .. 10000] :: [Int])
    func' "foldl' sum" (ffoldSum) ([1 .. 10000] :: [Int])
```

This will be the output you will get:

```
  Case         Allocated     Max    Live  GCs      MaxOS
  base sum       920,464  32,240  32,240    0  1,048,576
  conduit sum  2,720,496     424     424    2          0
  FoldL sum      160,000     424     424    0          0
  foldl' sum     880,152     456     456    0          0
```

It seems the base version of `sum` doesn't run in constant memory. All
the other implementation run in constant memory. Note that the conduit
version performs lots of allocations.

## Benchmarking IO actions

Let's try to read a huge file and benchmark them. First, let's create
a huge file:

``` shellsession
$ dd if=/dev/urandom of=/home/sibi/random.dat bs=1M count=2000
2000+0 records in
2000+0 records out
2097152000 bytes (2.1 GB, 2.0 GiB) copied, 11.1687 s, 188 MB/s
~/g/weigh-tutorial (master) $ ls -lh ~/random.dat
-rw-rw-r-- 1 sibi sibi 2.0G Jun 19 16:55 /home/sibi/random.dat
```

Now let's read the file. Create `src/WeighFile.hs`:

``` haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module WeighFile where

import Conduit
import qualified Data.ByteString as BS

strictBSLength :: FilePath -> IO Int
strictBSLength fname = do
  !bs <- BS.readFile fname
  return $ BS.length bs

conduitLength :: FilePath -> IO Int
conduitLength fname = runConduitRes $ sourceFile fname .| sumCo 0

sumCo :: Monad m => Int -> ConduitT BS.ByteString o m Int
sumCo !acc = do
  val :: Maybe BS.ByteString <- await
  case val of
    Just v -> sumCo (acc + (BS.length v))
    Nothing -> pure acc
```

and add the following function in `bench/main.hs`:

``` haskell
ioBenchmark :: Weigh ()
ioBenchmark =
  wgroup "IO Benchmark" $ do
    setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
    let fname = "/home/sibi/random.dat"
    io "strict bs read" (strictBSLength) fname
    io "conduit read" (conduitLength) fname
```

This will be the output you will get:

``` shellsession
IO Benchmark

  Case                Allocated     Max    Live    GCs          MaxOS
  strict bs read  2,097,174,728   9,888   9,888      1  2,098,200,576
  conduit read    2,239,654,104  10,208  10,208  2,001              0
```

You can see from the column `MaxOS` that reading data via strict
`ByteString` makes the entire data in the memory as opposed to the
conduit version. But the conduit version has to perform more garbage
collection because of it's streaming property.
