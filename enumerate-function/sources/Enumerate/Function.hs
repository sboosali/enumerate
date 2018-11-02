
{-| Reify function into mappings.


= Modules

== @module@ "Enumerate.Function.Reify"

    Reify a function @(a -> b)@ into a mapping @[(a, b)]@.

    See:

    * 'reifyFunctionAtM'

== @module@ "Enumerate.Function.Map"

    Isomorphisms between @(->)@ and @Map@.

    See:

    * 'toFunctionM'
    * 'isTotalM'
    * 'extensionallyEqualTo'
    * 'displayFunction'

== @module@ "Enumerate.Function.Invert"

    Invert functions. Prove functions' "jectivity".

    See:

    * 'invertM'
    * 'isInjectiveM'
    * 'isSurjectiveM'
    * 'isBijectiveM'


= API

== API: Conversion

Convert functions between representations, and to derived functions:

* Get Inverses:

    * 'invertM'

* To\/From @Map@s:

    * 'fromFunctionM'

* To\/From @Set@s:

    * 'fromPredicateM'

== API: Properties

Ensure a function's properties, or discover them:

* Totality:

    * 'isTotalM'
    * 'getCoverageM'

* \"Jectivity\":

    * Check injectivity:

        * 'isInjectiveM'
        * 'isInjective'

    * Check surjectivity:

        * 'isSurjectiveM'
        * 'isSurjective'

    * Check bijectivity:

        * 'isBijectiveM'
        * 'isBijective'

    * Discover properites:

        * 'getJectivityM'

== API: Enumeration

Get all possible functions (modulo extensional equality) between two types:

    * 'functionEnumerated'

How many possible functions (modulo extensional equality) exist between two types (fast):

    * 'functionCardinality'

Render a function (as its mapping):

* For debugging: 'displayFunction'

* For efficient serialization: @TODO@.


= Examples

== Example: Emacs KeyBindings

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

The `enumerate-function` package can:

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