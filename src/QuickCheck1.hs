-- | Original QuickCheck implementation, with code added on by Prem Muthedath.

-- purpose: study & understand QuickCheck. using original version is very apt,
-- as it is simple & short, yet has all the key features of QuickCheck today.

-- original implementation source: paper by claessen & hughes (see REF).
-- module, function comments: QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- REF: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

module QuickCheck1
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

import QuickCheck1.Test
import QuickCheck1.Generator
import QuickCheck1.Arbitrary
import QuickCheck1.Property

--------------------------------------------------------------------------------

