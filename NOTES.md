# Miscellaneous Notes

## `doctest`


> Setup code
> 
> You can put setup code in a named chunk with the name $setup. The setup code is run before each example group. If the setup code produces any errors/failures, all tests from that module are skipped.
> 
> Here is an example:

```
module Foo where

import Bar.Baz

-- $setup
-- >>> let x = 23 :: Int

-- |
-- >>> foo + x
-- 65

foo :: Int
foo = 42
```

> Note that you should not place setup code inbetween the module header (module ... where) and import declarations. GHC will not be able to parse it (issue #167). It is best to place setup code right after import declarations, but due to its declarative nature you can place it anywhere inbetween top level declarations as well.

## `hspec` Testing Framework

Hspec Manual: <http://hspec.github.io/>

## `tasty` Testing Framework


## `class` `Ix`

```haskell
class (Ord a) => Ix a where

    {-# MINIMAL range, (index | unsafeIndex), inRange #-}

    -- | The list of values in the subrange defined by a bounding pair.
    range               :: (a,a) -> [a]

    -- | The position of a subscript in the subrange.
    index               :: (a,a) -> a -> Int

    -- | Like 'index', but without checking that the value is in range.
    unsafeIndex         :: (a,a) -> a -> Int

    -- | Returns 'True' the given subscript lies in the range defined
    -- the bounding pair.
    inRange             :: (a,a) -> a -> Bool

    -- | The size of the subrange defined by a bounding pair.
    rangeSize           :: (a,a) -> Int
```


## 

## 

## 

## 

## 

## 

## 

## 

## 

## 

