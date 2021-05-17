{-# LANGUAGE ScopedTypeVariables #-}

-- | Test case generation.
-- code source: original QuickCheck implementation, Hughes, et al.
-- see hughes: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
-- module, function comments: QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- System.Random @ hackage: https://tinyurl.com/js4wzabf
-- Prem Muthedath:
--    1. +, modified type signatures, pattern signatures;
--    2. fixed compilation errors;
--    3. +, modified functions;
--    4. + error handling for empty [] arguments;
--    5. cabal packaging;
--    6. comments.

module QuickCheck.Generator where

import System.Random
  ( StdGen
  , Random
  , RandomGen
  , randomR
  , split
  , newStdGen
  )

import Control.Monad
  ( ap
  , replicateM
  , liftM
  )

--------------------------------------------------------------------------------
-- | generator type.

-- | a generator for value of type `a`.
newtype Gen a = Gen (Int -> StdGen -> a)

instance Functor Gen where
  fmap f (Gen x) = Gen (\n r -> f (x n r))

instance Applicative Gen where
  pure x = Gen (\_ _ -> x)
  (<*>)  = ap

instance Monad Gen where
  return a = Gen (\_ _ -> a)
  Gen m1 >>= k =
    Gen (\n r0 ->
      let (r1, r2) :: (StdGen, StdGen) = split r0
          Gen m2                       = k (m1 n r1)
      in m2 n r2)

--------------------------------------------------------------------------------
-- | primitive generator combinators.

-- | generates a random element in the given range.
choose :: Random a => (a, a) -> Gen a
choose bounds = Gen (\_ r -> fst (randomR bounds r))

-- | modifies a generator using an integer seed.
-- Prem + `rands` type signature. note that `rands` is infinitly recursive, but
-- being lazy, `rands` is called only till `v + 1` index, so the code works!
variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v + 1)))
  where rands :: forall t. RandomGen t => t -> [t]
        rands r0 = r1 : rands r2 where (r1, r2) :: (t, t) = split r0

-- | promotes a generator function to a generator of function. more generall
-- "promotes a monadic generator to a generator of monadic values."
promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)

-- | construct a generator that has a size constraint.
sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

-- | generates one of the given values; needs non-empty input list.
-- Prem + error handling; see QuickCheck @ hackage.
elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list."
elements xs = (xs !!) `liftM` choose (0, length xs - 1)

-- | randomly uses one of the given generators. needs non-empty input list.
-- Prem + error handling; see QuickCheck @ hackage.
oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list."
oneof gens = elements gens >>= id

-- run a generator. the size passed to the generator is always 30.
-- Prem + this function (not in hughes' paper); modeled after code in QuickCheck 
-- @ hackage, but hackage has a slightly different (& complex) implementation.
generate :: Gen a -> IO a
generate (Gen g) =
    do r :: StdGen <- newStdGen
       return (g 30 r)

--------------------------------------------------------------------------------
-- | common generator combinators.

-- | chooses one of the given generators, with a weighted random distribution.
-- input list must be non-empty.
-- Prem + error handling (see QuickCheck @ hackage), + `pick` type signature.
frequency :: forall a. [(Int, Gen a)] -> Gen a
frequency [] = error "QuickCheck.frequency used with empty list."
frequency xs | any (< 0) (map fst xs) =
               error "QuickCheck.frequency: negative weight."
             | all (== 0) (map fst xs) =
               error "QuickCheck.frequency: all weights zero."
frequency xs0 = choose (1, sum (map fst xs0)) >>= (`pick` xs0)
  where pick :: Int -> [(Int, Gen a)] -> Gen a
        pick _ [] = error "QuickCheck.pick used with empty list."
        pick n ((k, x):ys) | n <= k    = x
                           | otherwise = pick (n-k) ys

-- generates non-empty list of random length. size parameter governs max length.
-- Prem + this function (not in hughes' paper); modeled after code in QuickCheck 
-- @ hackage, but hackage one has a slightly different implementation.
listOf1 :: Gen a -> Gen [a]
listOf1 gen = sized $ \n ->
    do k :: Int <- choose (1, 1 `max` n)
       replicateM k gen

--------------------------------------------------------------------------------

