-- | Test QuickCheck by running it on a bunch of properties.
-- output visually (i.e., manually) verified to confirm if QuickCheck works!
-- author: Prem Muthedath.
--
-- NOTE: since this file has no explicit module name, GHC, by default, treats it 
-- as the `Main` module.  see https://tinyurl.com/3yw7tft7 (so)

import QuickCheck1

import Data.List
  ( nub
  , insert
  , sort
  )

--------------------------------------------------------------------------------
-- | tests for QuickCheck.

-- | enumeration of tests for QuickCheck.
data Test = Pass
            | Fail
            | Collect
            | Classify
            | Implication
            | GaveUp
            | ForAll
            deriving (Eq, Enum)

-- | `Show` instance,
instance Show Test where
  show Pass         = "*** QuickCheck pass test ***"
  show Fail         = "*** QuickCheck fail test ***"
  show Collect      = "*** QuickCheck collect test ***"
  show Classify     = "*** QuickCheck classify test ***"
  show Implication  = "*** QuickCheck implication (==>) test ***"
  show GaveUp       = "*** QuickCheck gave up! (==>) test ***"
  show ForAll       = "*** QuickCheck forAll test ***"

--------------------------------------------------------------------------------
-- | properties to test QuickCheck.

-- | QuickCheck `pass` test.
prop_pass :: Int -> Int -> Bool
prop_pass x y = x + y == y + x

-- | QuickCheck `fail' test.
prop_fail :: [Int] -> [Int] -> Property
prop_fail x y = collect (length x) $ x ++ y /= y ++ x

-- | QuickCheck `collect` test.
prop_collect :: [Int] -> Property
prop_collect xs = collect (length xs) $ xs == reverse (reverse xs)

-- | QuickCheck `classify` test.
prop_classify :: [Int] -> Property
prop_classify x =
    classify (x==[]) "empty" $
    classify (length x > 10) "has > 5 elements" $
    classify (x /= nub x) "has duplicates" $
    reverse (reverse x) == x

-- | QuickCheck implication (==>) test.
prop_impl :: Int -> Int -> Property
prop_impl x y = (x >= (-25)) ==> (x + y == y + x)

-- | QuickCheck 'gave up!' test.
prop_gave_up :: Int -> [Int] -> Property
prop_gave_up x xs = (ordered xs) ==> (ordered (insert x xs))
  where ordered y = (y == sort y)

-- QuickCheck `forAll` test.
prop_forAll :: Int -> Property
prop_forAll x = forAll orderedList $ \xs ->
    classify (xs==[]) "empty" $
    classify (length xs > 10) "has > 10 elements" $
    classify (xs /= nub xs) "has duplicates" $
    ordered (insert x xs)
  where ordered y = (y == sort y)

--------------------------------------------------------------------------------
-- | test QuickCheck.

-- NOTE: you've to visually verify the output to confirm if QuickCheck works.
-- to test verbose, replace `quickCheck` with `verboseCheck` in below code.

-- | main
main :: IO ()
main = mapM_ (\x ->
      do putStrLn $ "\n" <> show x
         case x of
             Pass         -> quickCheck prop_pass
             Fail         -> quickCheck prop_fail
             Collect      -> quickCheck prop_collect
             Classify     -> quickCheck prop_classify
             Implication  -> quickCheck prop_impl
             GaveUp       -> quickCheck prop_gave_up
             ForAll       -> quickCheck prop_forAll
      ) [toEnum 0 :: Test ..]

--------------------------------------------------------------------------------


