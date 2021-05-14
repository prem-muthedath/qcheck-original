{-# LANGUAGE ScopedTypeVariables #-}

-- | Original QuickCheck implementation (from Hughes' paper; see `REF` below).
-- Prem Muthedath:
--    1. +, modified type signatures, + some functions;
--    2. fixed compilation errors;
--    3. error handling for empty [] arguments;
--    4. cabal packaging;
--    5. docs.

-- Purpose: study & understand QuickCheck. this original version is very apt, as 
-- it is simple & short, yet has all the key features of QuickCheck today.

-- REF:
-- QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- System.Random @ hackage: https://tinyurl.com/js4wzabf
-- begriffs: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- hughes: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

module QuickCheck where

import System.Random
import Control.Monad
import qualified Data.Map as M
import Data.List (nub, findIndices, insert, sort)

------------------------------ Generators --------------------------------------
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

choose :: Random a => (a, a) -> Gen a
choose bounds = Gen (\_ r -> fst (randomR bounds r))

-- | NOTE: `rands` is infinitly recursive; but the code still works because 
-- haskell is lazy: after `v + 1` index, `rands` is not called anymore.
variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v + 1)))
  where rands :: forall t. RandomGen t => t -> [t]
        rands r0 = r1 : rands r2 where (r1, r2) :: (t, t) = split r0

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

-- | Prem + error handling.
elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list."
elements xs = (xs !!) `liftM` choose (0, length xs - 1)

vector :: forall a. Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary :: Gen a | _ <- [1..n] ]

-- | Prem + error handling.
oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list."
oneof gens = elements gens >>= id

-- | Prem + error handling; modeled after QuickCheck code @ hackage.
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

-- Prem + this function; modeled after one in latest QuickCheck @ hackage, but 
-- the hackage one has a slightly different implementation.
listOf1 :: Gen a -> Gen [a]
listOf1 gen = sized $ \n ->
    do k :: Int <- choose (1, 1 `max` n)
       replicateM k gen

-- Prem + this function; modeled after one in latest QuickCheck @ hackage, but 
-- the hackage one has a slightly different (& complex) implementation.
generate :: Gen a -> IO a
generate (Gen g) =
    do r :: StdGen <- newStdGen
       return (g 30 r)

------------------------------ Arbitrary ; Coarbitrary -------------------------
class Arbitrary a where
  arbitrary :: Gen a

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

class Coarbitrary a where
  coarbitrary :: a -> Gen b -> Gen b    -- Note: `b` can be anything.

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

------------------------------ Property ----------------------------------------
newtype Property = Prop (Gen Result)

data Result = Result
  {ok :: Maybe Bool, stamp :: [String], arguments :: [String]}

nothing :: Result
nothing = Result {ok = Nothing, stamp = [], arguments = []}

result :: Result -> Property
result res = Prop (return res)

class Testable a where
  property :: a -> Property

instance Testable Bool where
  property b = result (nothing {ok = Just b})

instance Testable Property where
  property prop = prop

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen :: Property = property a

forAll :: forall a b. (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
    do a :: a        <- gen
       res :: Result <- evaluate (body a)
       return (arg a res)
  where arg :: a -> Result -> Result
        arg a res = res {arguments = show a : arguments res}

(==>) :: Testable a => Bool -> a -> Property
True ==> a  = property a
False ==> _ = result nothing

label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
  where add :: Result -> Result
        add res = res {stamp = s : stamp res}

classify :: Testable a => Bool -> String -> a -> Property
classify True name    = label name
classify False _      = property

collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

------------------------------ Example -----------------------------------------
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = liftM Leaf arbitrary
arbTree n = frequency [
      (1, liftM Leaf arbitrary),
      (4, liftM2 Branch (arbTree (n `div` 2)) (arbTree (n `div` 2)))
    ]

------------------------------ Testing -----------------------------------------
histogram :: [Result] -> [(String, String)]
histogram res = let freq = M.toList $ testFreq res M.empty in
    map (\(k, v) ->
      let percent = (v * 100) `div` (length res)
          val     = show percent <> "%"
      in (k, val)) freq
  where testFreq :: [Result] -> M.Map String Int -> M.Map String Int
        testFreq xs mp = foldr (\x y -> testCaseFreq (stamp x) y) mp xs
        testCaseFreq :: [String] -> M.Map String Int -> M.Map String Int
        testCaseFreq ls mp = foldr (\x y -> M.insertWith (+) x 1 y) mp ls

check :: Testable a => a -> Bool -> IO ()
check prop verbose = do
    results :: [Result] <- generate . replicateM 100 . evaluate $ prop
    if any (\x -> ok x == Just False) results
      then printFail results
      else if any (\x -> ok x == Nothing) results
      then printGaveUp results
      else printPass results
  where printFail :: [Result] -> IO ()
        printFail xs =
          do let i = head $ findIndices (\x -> ok x == Just False) xs
             if verbose then printCases (zip [1..i+1] xs) else return ()
             putStrLn $ "*** Failed! Falsifiable after " <> show (i + 1) <> " tests:"
             printArgs $ arguments (xs !! i)
        printGaveUp :: [Result] -> IO ()
        printGaveUp xs = do
             let is = findIndices (\x -> ok x /= Nothing) xs
             putStrLn $ "*** Gave up! Passed only " <> show (length is) <> " tests."
             if verbose then printCases $ map (\i -> (i + 1, xs !! i)) is
             else return ()
        printPass :: [Result] -> IO ()
        printPass xs = do
             if verbose then printCases (zip [1..] xs) else return ()
             putStrLn $ "+++ OK: passed " <> show (length xs) <> " tests."
             mapM_ (\(k, v) -> putStrLn $ v <> " " <> k) $ histogram xs
        printCases :: [(Int, Result)] -> IO ()
        printCases = mapM_ (\(x, y) ->
          do putStrLn $ "+++ Test case " <> show x <> ":"
             printArgs $ arguments y)
        printArgs :: [String] -> IO ()
        printArgs = mapM_ (\x -> putStrLn $ "    " <> id x)  -- for `id` usage, see https://tinyurl.com/e9cmzc7c (so)

quickCheck :: Testable a => a -> IO ()
quickCheck prop = check prop False

verboseCheck :: Testable a => a -> IO ()
verboseCheck prop = check prop True

------------------------------ Properties --------------------------------------
prop_1 :: [Int] -> [Int] -> Property
prop_1 x y = collect (length x) $ x ++ y /= y ++ x

prop_2 :: [Int] -> Property
prop_2 x = classify (x==[]) "empty" $
           classify (length x > 10) "has > 5 elements" $
           classify (x /= nub x) "has duplicates" $
           reverse (reverse x) == x

prop_3 :: Int -> [Int] -> Property
prop_3 x xs = (ordered xs) ==> (ordered (insert x xs))
  where ordered y = (y == sort y)

