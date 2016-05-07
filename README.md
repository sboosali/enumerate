# enumerate
enumerate all the values in a finite type (automatically)

provides (1) a typeclass for enumerating all values in a finite type,
(2) a generic instance for automatic deriving, and
(3) helpers that reify functions (partial or total, monadic or pure) into a Map.

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
https://hackage.haskell.org/package/enumerate
