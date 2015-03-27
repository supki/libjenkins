module X where

{- | $setup
>>> import Test.QuickCheck
>>> :{
instance Arbitrary X where arbitrary = undefined
:}
-}

{- | >>> :{
let x = 1
    y = 2
  in x + y
:}
3
-}
data X
