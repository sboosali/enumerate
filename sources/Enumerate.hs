{-| enumerate all values in a finite type.

e.g.

@
TODO
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
 , module Enumerate.Reify
 , module Enumerate.Map
 , module Enumerate.Enum

 -- , module Enumerate.Domain
 ) where
import Enumerate.Types
import Enumerate.Cardinality
import Enumerate.Reify
import Enumerate.Map
import Enumerate.Enum

-- import Enumerate.Domain
