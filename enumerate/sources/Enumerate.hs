{-| enumerate all values in a finite type.

e.g.

>>> :set -XDeriveGeneric
>>> :set -XDeriveAnyClass

given:

@
-- an 'Enumerable' can be automatically derived,
-- even though it's a nested sum type (and thus not an 'Enum').
data Edit = Edit Action Slice Region
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data Action
 = Select
 | Copy
 | Delete
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data Slice
 = Whole
 | Backwards
 | Forwards
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data Region
 = Character
 | Token
 | Line
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)
@

we can enumerate every possible editing action:

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

see "Enumerate.Types" for detailed documentation.

the modules "Enumerate.Large" and "Enumerate.Function" have
orphan instances for large types,
and aren't reexported by default.
this makes attempting to enumerate them a type error,
rather than runtime non-termination.

See the source of "Enumerate.Example" for an example.

-}
module Enumerate --TODO rename to Enumerable
 ( module Enumerate.Types
 , module Enumerate.Cardinality
 , module Enumerate.Enum

 -- , module Enumerate.Domain
 ) where
import Enumerate.Types
import Enumerate.Cardinality
import Enumerate.Enum

-- import Enumerate.Domain
