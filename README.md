# enumerate

[![Hackage](https://img.shields.io/hackage/v/enumerate.svg)](https://hackage.haskell.org/package/enumerate)
[![Build Status](https://secure.travis-ci.org/sboosali/enumerate.svg)](http://travis-ci.org/sboosali/enumerate)

Enumerate all the values in a finite type (automatically). Provides:

1. a typeclass for enumerating all values in a finite type,
2. a generic instance for automatically deriving it, and

# example

```haskell
    {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
    import Data.Enumerate (Enumerable(..))
    import Data.Generics (Generics)

    data CrudOp = Add | Edit | Delete | View
     deriving (Eq,Ord,Enum,Bounded,Generic,Enumerable)
    data Route = Home | Person CrudOp | House CrudOp
     deriving (Eq,Ord,Generic,Enumerable)

    >>> enumerated :: [Route]
    [Home, Person Add, Person Edit, Person Delete, Person View, House Add, House Edit, House Delete, House View]
```

# (extensive) documentation:
https://hackage.haskell.org/package/enumerate/docs/Enumerate.html

http://sboosali.github.io/documentation/enumerate/Enumerate.html (when hackage won't build the haddocks)

# related:

To reify functions, partial or total, into a Map,
see [enumerate-function](https://github.com/sboosali/enumerate-function).
