{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, LambdaCase, TupleSections #-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Function.Map

  (

   -- * @Enumerate.Function.Map@
   -- $module

    module Enumerate.Function.Map

   -- * (doctest context)
   -- $setup

  ) where

--------------------------------------------------
-- Imports: (Internal) Project Libraries ---------
--------------------------------------------------

import Enumerate.Types

--------------------------------------------------

import Enumerate.Function.Extra
import Enumerate.Function.Types
import Enumerate.Function.Reify
import Enumerate.Function.Invert

--------------------------------------------------
-- Imports: (External) Dependency Libraries ------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..))

--------------------------------------------------
-- Imports: Standard Library ---------------------
--------------------------------------------------

import qualified Data.Map as Map
import           Data.Map (Map)

--------------------------------------------------

import           Control.Exception(PatternMatchFail(..))
import           Data.Maybe (fromJust)

--------------------------------------------------

-- import GHC.TypeLits (Nat, type (^))

--------------------------------------------------
-- Documentation: Module -------------------------
--------------------------------------------------

{- $module

Converting between (partial) functions and maps.

'Partial' (a.k.a. a safely-partial function) is isomorphic to @Map@:

@
'fromFunctionM' . 'toFunctionM' ≡ 'id'
'toFunctionM' . 'fromFunctionM' ≡ 'id'
@

modulo which error is thrown.

-}

--------------------------------------------------
-- Documentation: DocTest ------------------------
--------------------------------------------------

{- $setup

>>> :set +m
>>> :set -XLambdaCase
>>> import qualified Prelude
>>> :{
let myNotM :: Monad m => Bool -> m Bool
    myNotM False = return True
    myNotM True  = return False
:}

-}

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{- | Convert a map to a function, if the map is total.

>>> Just not_ <- return $ toFunction (Map.fromList [(False,True),(True,False)])
>>> not_ False
True
>>> not_ True
False

-}

toFunction
  :: (Enumerable a, Ord a)
  => Map a b -> Maybe (a -> b)

toFunction m =

  if   isMapTotal m
  then Just f
  else Nothing

  where

  f = unsafeToFunction m
  --
  -- the cast (unsafeToFunction) is safe.
  -- when the map is total (isMapTotal).

{-# INLINABLE toFunction #-}

--------------------------------------------------

{- | Convert a (safely-)partial function to a map.

Lookup failures are 'throwM'n as a 'PatternMatchFail'.

>>> id_partial = toFunctionM (Map.fromList [(True,True)])
>>> id_partial True
True
>>> id_partial False
*** Exception: toFunctionM

-}

toFunctionM
  :: (Enumerable a, Ord a)
  => Map a b -> (Partial a b)

toFunctionM m = f

  where

  f x = maybe (throwM (PatternMatchFail message))
              return
              (Map.lookup x m)
        where
        message = "toFunctionM"
        -- NOTE Enriching the message with « show x »
        --      would require a « Show a » constraint.

{-# INLINABLE toFunctionM #-}

--------------------------------------------------
--------------------------------------------------

{-| Refines a partial function, if total.

>>> Just myNot = isTotalM myNotM
>>> myNot False
True
>>> myNot True
False

>>> :{
let uppercasePartial :: (MonadThrow m) => Char -> m Char  -- :: Partial Char Char
    uppercasePartial = \case
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
in maybe False (const True) $ isTotalM uppercasePartial
:}
False

>>> maybe False (const True) $ isTotalM (return :: Partial Char Char)
True

(with @uppercasePartial@ defined above, and 'Partial' defined internally).

@
'isTotalM' ≡ 'toFunction' '.' 'fromFunctionM'
@

-}

isTotalM
  :: (Enumerable a, Ord a)
  => (Partial a b) -> Maybe (a -> b)

isTotalM f = toFunction (fromFunctionM f)

{-# INLINABLE isTotalM #-}

--------------------------------------------------
--------------------------------------------------

{-| Wrapper around 'Map.lookup'

-}

unsafeToFunction
  :: (Ord a)
  => Map a b -> (a -> b)

unsafeToFunction m x = fromJust (Map.lookup x m)

{-# INLINABLE unsafeToFunction #-}

--------------------------------------------------

{-| Wraps 'Map.lookup'.

>>> (unsafeFromList [(False,True),(True,False)]) False
True
>>> (unsafeFromList [(False,True),(True,False)]) True
False

@
'unsafeFromList' = 'unsafeToFunction' . 'Map.fromList'
@

-}

unsafeFromList
 :: (Ord a)
 => [(a,b)]
 -> (a -> b)

unsafeFromList
 = unsafeToFunction . Map.fromList

{-# INLINABLE unsafeFromList #-}

--------------------------------------------------
--------------------------------------------------

{-| __/All/__ functions from @a@ to @b@.

In particular, all mappings /modulo/ extensional-equality
(i.e. 'extensionallyEqualTo').

@
'functionEnumerated' ≡ 'mappingEnumeratedAt' 'enumerated' 'enumerated'

-- (modulo 'toFunction')
@

Used by the 'Enumerable' instance for functions 
(see "Enumerate.Orphans.Function").

-}

functionEnumerated
 :: (Enumerable a, Enumerable b, Ord a, Ord b)
 => [a -> b]

functionEnumerated = functions
  where

  functions = (unsafeToFunction . Map.fromList) <$> mappings
  mappings  = mappingEnumeratedAt enumerated enumerated

{-# INLINABLE functionEnumerated #-}

--------------------------------------------------

{-| The number of functions from @a@ to @b@.

In particular, the number of mappings /modulo/ extensional-equality
(i.e. 'extensionallyEqualTo').

Arithmetically:

@≡ |b| ^ |a|
@

Example:

>>> import Prelude
>>> import Data.Ord (Ordering)
>>> functionCardinality (Nothing :: Maybe (Ordering -> Bool))
8
>>> cardinality (Just True)
2
>>> cardinality (Just EQ)
3
>>> 2 ^ 3
8

Used by the 'Enumerable' instance for functions 
(see "Enumerate.Orphans.Function").

-}

functionCardinality
 :: forall a b proxy. (Enumerable a, Enumerable b)
 => proxy (a -> b)
 -> Natural

functionCardinality _ = b ^ a
  where

  a = cardinality (Proxy :: Proxy a)
  b = cardinality (Proxy :: Proxy b)

{-# INLINABLE functionCardinality #-}

--------------------------------------------------

{-| The number of functions from @a@ to @b@.

See 'functionCardinality'.

Example:

>>> import Prelude
>>> import Data.Ord (Ordering(..))
>>> functionCardinalityOf (==EQ)
8

Specializations:

* Unifying @proxy@ with the function-arrow @(->)@:

    @
    'functionCardinalityOf' :: (Enumerable a, Enumerable b) => (a -> b) -> Natural
    @

-}

functionCardinalityOf
 :: forall a b proxy. (Enumerable a, Enumerable b)
 => proxy a b
 -> Natural

functionCardinalityOf _ = b ^ a
  where

  a = cardinality (Proxy :: Proxy a)
  b = cardinality (Proxy :: Proxy b)

{-# INLINABLE functionCardinalityOf #-}

--------------------------------------------------
--------------------------------------------------

-- | Are all pairs of outputs the same for the same input? (short-ciruits).

extensionallyEqualTo
 :: (Enumerable a, Eq b)
 => (a -> b)
 -> (a -> b)
 -> Bool

extensionallyEqualTo f g
 = all ((==) <$> f <*> g) enumerated

{-# INLINABLE extensionallyEqualTo #-}

--------------------------------------------------

-- | Is any pair of outputs different for the same input? (short-ciruits).

extensionallyUnequalTo
 :: (Enumerable a, Eq b)
 => (a -> b)
 -> (a -> b)
 -> Bool

extensionallyUnequalTo f g
 = any ((/=) <$> f <*> g) enumerated

{-# INLINABLE extensionallyUnequalTo #-}

--------------------------------------------------
--------------------------------------------------

{- | Show all inputs and their outputs, as @'unsafeFromList' [...]@.

Useful for defining (orphan) 'Show' instances for functions.
(See "Enumerate.OrphansFunction").

-}
functionShowsPrec
 :: (Enumerable a, Show a, Show b)
 => Int
 -> (a -> b)
 -> ShowS
functionShowsPrec
 = showsPrecWith "unsafeFromList" reifyFunction

{-# INLINABLE functionShowsPrec #-}

--------------------------------------------------

{-| Display a function as a @case@ expression.

Show /all/ inputs and their outputs, as a @-XLambdaCase@ expression,
i.e. @\\case ...@.

>>> Prelude.putStrLn (displayFunction Prelude.not)
\case
 False -> True
 True -> False

>>> const_True = const True :: Bool -> Bool
>>> Prelude.putStrLn (displayFunction const_True)
\case
 False -> True
 True -> True

-}

displayFunction
  :: (Enumerable a, Show a, Show b)
  => (a -> b) -> String

displayFunction = reifyFunction
             >>> fmap displayCase
             >>> ("\\case" :)
             >>> intercalate "\n" --TODO or, optionally with parameter, a semicolon.

  where

  displayCase (x,y) = intercalate " " ["", show x, "->", show y]

--------------------------------------------------

-- displayPartialFunction
--  :: (Enumerable a, Show a, Show b)
--  => (Partial a b)
--  -> String

--------------------------------------------------

{-| Display a function, if injective, as an "@esac@" expression.

@esac@ is some ad-hoc (pseudo-Haskell) syntax for defining injective functions.
Naming: "esac" is "case" backwards.

>>> const_True = const True :: Bool -> Bool
>>> Nothing = displayInjective const_True

>>> Just f = displayInjective not
>>> putStrLn f
\esac
 True <- Just False
 False <- Just True
 _ <- Nothing

Calls 'isInjective'.

-}

displayInjective
 :: (Enumerable a, Ord a, Ord b, Show a, Show b)
 => (a -> b)
 -> Maybe String

displayInjective f =

  case isInjective f of
    Nothing -> Nothing
    Just{}  -> Just (go f)

  where

  go  = reifyFunction
    >>> fmap displayCase
    >>> (["\\esac"]++)
    >>> (++ [" _ <- Nothing"])
    >>> intercalate "\n"      --TODO or, optionally with parameter, a semicolon.

  displayCase (x,y) = intercalate " " ["", show y, "<-", show (Just x)]

  -- displayInjective f = go <$> isInjective f
  --
  --   where
  --   go   = reifyFunction
  --      >>> fmap displayCase
  --      >>> ("\\case":)
  --      >>> intercalate "\n"
  --   displayCase = \case
  --    (y, Nothing) ->
  --    (y, Just x)  -> intercalate " " ["", show y, " <- ", show x]

--------------------------------------------------
--------------------------------------------------

{-| Construct all mappings, with explicit domain and image.

NOTE:

* @[(a,b)]@ is a "mapping".
* @[[(a,b)]]@ is a list of mappings.

>>> orderingPredicates = mappingEnumeratedAt [LT,EQ,GT] [False,True]
>>> length orderingPredicates
8
>>> import Enumerate.Function.Extra (printMappings)
>>> printMappings orderingPredicates
<BLANKLINE>
(LT,False)
(EQ,False)
(GT,False)
<BLANKLINE>
(LT,False)
(EQ,False)
(GT,True)
<BLANKLINE>
(LT,False)
(EQ,True)
(GT,False)
<BLANKLINE>
(LT,False)
(EQ,True)
(GT,True)
<BLANKLINE>
(LT,True)
(EQ,False)
(GT,False)
<BLANKLINE>
(LT,True)
(EQ,False)
(GT,True)
<BLANKLINE>
(LT,True)
(EQ,True)
(GT,False)
<BLANKLINE>
(LT,True)
(EQ,True)
(GT,True)

where the (total) mapping:

@
[ (LT, False)
, (EQ, False)
, (GT, True)
]
@

is equivalent to the function:

@
\\case
 LT -> False
 EQ -> False
 GT -> True
@

(with 'printMappings' defined internally).

-}

mappingEnumeratedAt :: [a] -> [b] -> [[(a,b)]]           -- TODO diagonalize? performance?
mappingEnumeratedAt as bs = go (crossProduct as bs)
 where

 go []                     = []

 go [somePairs]            = do
  pair <- somePairs
  return$ [pair]

 go (somePairs:theProduct) = do
  pair <- somePairs
  theExponent <- go theProduct
  return$ pair : theExponent

--------------------------------------------------
--------------------------------------------------