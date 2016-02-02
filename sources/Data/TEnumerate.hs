{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}

module Data.TEnumerate where 


{-| 

e.g. 

-- >>> type instance TEnumerated 'Bool = '[ 'False, 'True ]
-- >>> :kind! TEnumerated Bool
-- '[ 'False, 'True ]

law: injective.

law: the kind of the items is the input. 

the pseudo-Haskell: 

@
type family TEnumerated k :: [k]
@

errors with: 

@
    Kind variable also used as type variable: ‘k’
@


-}
type family TEnumerated k1 :: [k2]

type instance TEnumerated Bool = '[ 'False, 'True ]

