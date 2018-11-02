--------------------------------------------------
--------------------------------------------------

{-| Enumerate all values of finite types.

An 'Enumerable' instance can be /automatically/ derived for any finite type,
even ones for which 'Enum' cannot:

* Nested sum types. e.g. @(Either Bool Bool)@.
* Polymorphic types. e.g. @('Enumerable' a, 'Enumerable' b) => 'Enumerable' (Either a b)@.
* Product types. e.g. @('Enumerable' a, 'Enumerable' b) => 'Enumerable' (a, b)@.


= Modules


== @module@ "Enumerate.Types":

Defines the core typeclass, 'Enumerable', the 'Generic' "hook",
dozens of instances thereof.

== @module@ "Enumerate.Cardinality":

Defines 'Finite', a subclass of 'Enumerable'
(... but not actually, for technical reasons).

== @module@ "Enumerate.Enum":

Utilities for defining instances of the following typeclases
(given an instance of 'Enumerate'):

* 'Enum'
* 'Bounded'
* 'Ix' (TODO)

== @module@ "Enumerate.Between":

Type-Safe bounded intervals.


= Usages

== Use #1: Deriving an @Enum@-like class (for non nullary-sum datatypes)

@
data These a b

  = This  a
  | That    b
  | These a b

  deriving (Show, Eq, Ord, ..., 'Generic', 'Enumerable')
@


== Use #2: Writing @Enum@, @Bounded@, and @Ix@ instances (for non nullary-sum datatypes)

@Enum@ instance:

@
instance ('Enumerable' a, 'Enumerable' b, Ord a, Ord b) => 'Enum' (These a b) where

 'toEnum'   = 'toEnum_enumerable'   'array_enumerable'
 'fromEnum' = 'fromEnum_enumerable' 'table_enumerable'
@

@Bounded@ instance:

@
instance ('Enumerable' a, 'Enumerable' b) => 'Bounded' (These a b) where

  'minBound' = 'minBound_enumerable''
  'maxBound' = 'maxBound_enumerable''
@

instance (Enumerable a, Enumerable b) => Bounded (These a b) where  minBound = minBound_enumerable;  maxBound = maxBound_enumerable

For example, 

@
λ> toEnum 0 :: These Bool Bool
This False

λ> maxBound :: These Bool Bool
These True True
@


== Use #3: Enumerating a type

@
λ> import Prelude
λ> import Data.Ord

λ> type ThisBooleanThatOrdering = These 'Bool' 'Ordering'

λ> proxy = (Nothing :: Maybe ThisBooleanThatOrdering) 
λ> 'cardinality' proxy
11

λ> 'enumerated' :: [ThisBooleanThatOrdering]
--
[ This False
, This True
--
, That LT
, That EQ
, That GT
--
, These False LT
, These False EQ
, These False GT
, These True LT
, These True EQ
, These True GT
]
--
@

== Use #4: Pre-computing (and\/or caching) functions whose domains are @Enumerable@

== Use #5: Reifying functions whose domains and\/or images are @Enumerable@

See @<http://hackage.haskell.org/package/enumerate-function enumerate-function>@


= Examples

== Example: VIM-like Text Objects/Actions

Given these types:

@
-- DeriveGeneric
-- DeriveAnyClass

data Edit = Edit Action Slice Region
 deriving (Show,Eq,Ord,Generic,'Enumerable')

data Action
 = Select
 | Copy
 | Delete
 deriving (Show,Eq,Ord,Enum,Bounded,Generic,'Enumerable')

data Slice
 = Whole
 | Backwards
 | Forwards
 deriving (Show,Eq,Ord,Enum,Bounded,Generic,'Enumerable')

data Region
 = Character
 | Token
 | Line
 deriving (Show,Eq,Ord,Enum,Bounded,Generic,'Enumerable')
@

We can enumerate every possible editing action:

@
> 'enumerated' :: [Edit]
Edit Select Whole Character
Edit Select Whole Token
Edit Select Whole Line
Edit Select Backwards Character
Edit Select Backwards Token
Edit Select Backwards Line
Edit Select Forwards Character
Edit Select Forwards Token
Edit Select Forwards Line
Edit Copy Whole Character
Edit Copy Whole Token
Edit Copy Whole Line
Edit Copy Backwards Character
Edit Copy Backwards Token
Edit Copy Backwards Line
Edit Copy Forwards Character
Edit Copy Forwards Token
Edit Copy Forwards Line
Edit Delete Whole Character
Edit Delete Whole Token
Edit Delete Whole Line
Edit Delete Backwards Character
Edit Delete Backwards Token
Edit Delete Backwards Line
Edit Delete Forwards Character
Edit Delete Forwards Token
Edit Delete Forwards Line
@

See "Enumerate.Types" for detailed documentation.

the modules "Enumerate.Large" and "Enumerate.Function" have
orphan instances for large types,
and aren't reexported by default.
this makes attempting to enumerate them a type error,
rather than runtime non-termination.

See the source of "Enumerate.Example" for an example.


== Related

Related packages include:

* @<http://hackage.haskell.org/package/generic-deriving generic-deriving>@:
No efficient 'cardinality', and fewer helpers.
However, @generic-deriving@ is widely-used,
and if you're already depending on @generic-deriving@,
I definitely recommend simply importing
@<http://hackage.haskell.org/package/generic-deriving-1.12.2/docs/Generics-Deriving-Enum.html
Generics.Deriving.Enum>@.

* @<http://hackage.haskell.org/package/enumerable enumerable>@:
No @Generic@ instance.

* @<http://hackage.haskell.org/package/universe universe>@:
No @Generic@ instance.

* @<http://hackage.haskell.org/package/prelude-safeenum-0.1.1.2/docs/Prelude-SafeEnum.html SafeEnum>@:
Only @Enum@s.

* @<http://hackage.haskell.org/package/emgm-0.4/docs/Generics-EMGM-Functions-Enum.html emgm>@:
  Allows infinite lists (by convention). Too heavyweight.

* @<https://hackage.haskell.org/package/testing-feat-0.4.0.2/docs/Test-Feat-Class.html#t:Enumerable testing-feat>@:
Too heavyweight (testing framework).

* @<https://hackage.haskell.org/package/smallcheck smallcheck>@:
Too heavyweight (testing framework).
@Series@ enumerates up to some depth and can enumerated infinitely-inhabited types.

* @<https://hackage.haskell.org/package/quickcheck quickcheck>@:
Too heavyweight (testing framework, randomness not necessary).

-}

module Enumerate --TODO rename to Enumerable

 ( module Enumerate.Types
 , module Enumerate.Cardinality
 , module Enumerate.Enum
 -- , module Enumerate.Domain
 ) where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Types
import Enumerate.Cardinality
import Enumerate.Enum

-- import Enumerate.Domain

--------------------------------------------------
--------------------------------------------------