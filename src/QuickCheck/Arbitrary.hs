{-# LANGUAGE ScopedTypeVariables #-}

-- | Type classes for random generation of values.
-- code source: original QuickCheck implementation, Hughes, et al.
-- see hughes: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
-- module, function comments: QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- Prem Muthedath:
--    1. +, modified type signatures, pattern signatures;
--    2. + extra functions;
--    3. cabal packaging;
--    4. comments.

module QuickCheck.Arbitrary
  (
  -- Arbitrary, Coarbitrary values classes
    Arbitrary (..)
  , Coarbitrary (..)

  -- Tree type -- an instance of Arbitrary
  , Tree (..)

  -- helper functions for implementing arbitrary
  , arbTree       -- :: Arbitrary a => Int -> Gen (Tree a)

  -- generators that use arbitrary
  , orderedList   -- :: forall a. (Ord a, Arbitrary a) => Gen [a]
  , vector        -- :: forall a. Arbitrary a => Int -> Gen [a]
  )
where

import Control.Monad
  ( liftM
  , liftM2
  )

import Data.List
  ( sort
  )
import QuickCheck.Generator

--------------------------------------------------------------------------------
-- | class Arbitrary

-- random generation of values.
class Arbitrary a where
  -- | a generator for values of a given type.
  arbitrary :: Gen a

-- | instances
instance Arbitrary Bool where
  arbitrary = elements [True, False]

instance Arbitrary Int where
  arbitrary = sized (\n -> choose (-n, n))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = liftM2 (,) arbitrary arbitrary

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized (\n -> choose (0, n) >>= vector)

instance (Arbitrary a, Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)

-- | instance for Tree type.
-- Prem + this instance. hughes' paper has this code as an example.
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

-- | helper functions for implementing arbitrary.
-- | generator for Tree type.
arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = liftM Leaf arbitrary
arbTree n = frequency [
      (1, liftM Leaf arbitrary),
      (4, liftM2 Branch (arbTree (n `div` 2)) (arbTree (n `div` 2)))
    ]

--------------------------------------------------------------------------------
-- | class Coarbitrary.

-- used for random generation of functions.
class Coarbitrary a where
  -- | generates a function of type a -> b.
  coarbitrary :: a -> Gen b -> Gen b    -- Note: `b` can be anything.

-- | instances.
instance Coarbitrary Bool where
  coarbitrary b = variant (if b then 0 else 1)

instance Coarbitrary Int where
  coarbitrary n | n == 0    = variant 0
                | n < 0     = variant 2 . coarbitrary (-n)
                | otherwise = variant 1 . coarbitrary (n `div` 2)

instance (Coarbitrary a, Coarbitrary b) => Coarbitrary (a, b) where
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance Coarbitrary a => Coarbitrary [a] where
  coarbitrary []      = variant 0
  coarbitrary (a:as)  = variant 1 . coarbitrary a . coarbitrary as

-- | Prem + pattern type signatures; note that type `c` can be anything.
instance (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary (f :: a -> b) (gen :: Gen c) =
    (arbitrary :: Gen a)  >>= ((`coarbitrary` gen) . f)

--------------------------------------------------------------------------------
-- | generators that use arbitrary.

-- | generates an ordered list.
-- Prem + this function; hughes' paper mentions it but without implementation.
-- code source: QuickCheck @ hackage.
orderedList :: forall a. (Ord a, Arbitrary a) => Gen [a]
orderedList = sort <$> (arbitrary :: Gen [a])

-- | generates a list of given length.
vector :: forall a. Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary :: Gen a | _ <- [1..n] ]

--------------------------------------------------------------------------------

