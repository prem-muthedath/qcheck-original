module QuickCheck
    (
      -- running tests
      quickCheck
    , verboseCheck

      -- Arbitrary, Coarbitrary
    , Arbitrary (..)
    , Coarbitrary (..)
    , Tree (..)
    , arbTree
    , orderedList
    , vector

      -- `Gen` monad
    , Gen
    , choose
    , variant
    , promote
    , sized
    , elements
    , oneof
    , generate
    , frequency
    , listOf1

      -- Property
    , Property (..)
    , Testable (..)
    , Result (..)
    , nothing
    , result
    , evaluate
    , label
    , collect
    , classify
    , (==>)
    , forAll
    )
  where
--------------------------------------------------------------------------------
-- | imports

import QuickCheck.Test
import QuickCheck.Generator
import QuickCheck.Arbitrary
import QuickCheck.Property

--------------------------------------------------------------------------------

