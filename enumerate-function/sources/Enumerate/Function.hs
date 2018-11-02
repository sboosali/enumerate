
{-| 

== Modules

* "Enumerate.Function.Reify": 
* "Enumerate.Function.Map": 
* "Enumerate.Function.Invert": 

== Example

e.g.

1. given:

@
data Edit = Edit Action Slice Region
 deriving ('Show',Read,Eq,'Ord','Generic','Enumerable')

data Action
 = Transpose
 | Copy
 | Delete
 deriving ('Show',Read,Eq,'Ord',Enum,Bounded,'Generic','Enumerable')

data Slice
 = Whole
 | Backwards
 | Forwards
 deriving ('Show',Read,Eq,'Ord',Enum,Bounded,'Generic','Enumerable')

data Region
 = Character
 | Token
 | Line
 deriving ('Show',Read,Eq,'Ord',Enum,Bounded,'Generic','Enumerable')
@

we can enumerate every possible editing action:

@
> 'enumerated' :: [Edit]
@

2. given a mapping to keyboard shortcuts within emacs:

@
type KeyBinding = [String]
emacsEdit :: Edit -> KeyBinding
@

the `enumerate-function` package can:

* verify that @emacsEdit@ doesn't map different editing actions
to the same keybindings, which would mean that one would shadow the other
i.e. it has no collisions; i.e. it's is injective.
* TODO verify that @emacsEdit@ maps every editing action to some keybinding,
which asserts that the relevant application supports `Edit`ing in its entirety.
(e.g. `Emacs` can, `Chrome` can't); i.e. it's is surjective.
* detect whether @emacsEdit@ is actually total; i.e.
free of bottoms. Haskell's exhaustivity checker (enable `-Wall`) can verify the
totality of @emacsEdit@, assuming no partial functions.
* serialize @emacsEdit@ into a mapping,
from which `elisp` source can be extracted.

(also see the source of "Enumerate.Function.Example")

-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Function

 ( module Enumerate.Function.Types
 , module Enumerate.Function.Reify
 , module Enumerate.Function.Map
 , module Enumerate.Function.Invert
 ) where

--------------------------------------------------

import Enumerate.Function.Types
import Enumerate.Function.Reify
import Enumerate.Function.Map
import Enumerate.Function.Invert

--------------------------------------------------
--------------------------------------------------
