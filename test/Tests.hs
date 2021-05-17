-- | Test QuickCheck by running it on a bunch of properties.
-- author: Prem Muthedath.
--
-- NOTE: since this file has no explicit module name, GHC, by default, treats it 
-- as the `Main` module.  see https://tinyurl.com/3yw7tft7 (so)
import QuickCheck

import Data.List
  ( nub
  , insert
  , sort
  )

--------------------------------------------------------------------------------
-- | properties to be tested.

-- | integer addition -- commutative?
-- used to check if `Bool` works.
prop_add :: Int -> Int -> Bool
prop_add x y = x + y == y + x

-- | concatenation of 2 lists of `Int` -- commutative?
-- used to check `collect`, `Property` return
prop_concat :: [Int] -> [Int] -> Property
prop_concat x y = collect (length x) $ x ++ y /= y ++ x

-- | simple list reverse.
-- used to check `classify`.
prop_rev :: [Int] -> Property
prop_rev x =
    classify (x==[]) "empty" $
    classify (length x > 10) "has > 5 elements" $
    classify (x /= nub x) "has duplicates" $
    reverse (reverse x) == x

-- | inetger addition with implication -- commutative?
-- used to check `(==>)`.
prop_cond_add :: Int -> Int -> Property
prop_cond_add x y = (x >= (-25)) ==> (x + y == y + x)

-- | list insert with implication -- still ordered?
-- used to check `(==>)`.
prop_cond_ins :: Int -> [Int] -> Property
prop_cond_ins x xs = (ordered xs) ==> (ordered (insert x xs))
  where ordered y = (y == sort y)

-- list insert using `forAll` -- still ordered?
-- used to check `forAll`.
prop_ins :: Int -> Property
prop_ins x = forAll orderedList $ \xs ->
    classify (xs==[]) "empty" $
    classify (length xs > 10) "has > 10 elements" $
    classify (xs /= nub xs) "has duplicates" $
    ordered (insert x xs)
  where ordered y = (y == sort y)

--------------------------------------------------------------------------------
-- | main

-- test properties.
main :: IO ()
main = do
  putStrLn "--- add ---"
  quickCheck prop_add
  putStrLn "--- concat ---"
  quickCheck prop_concat
  putStrLn "--- rev ---"
  quickCheck prop_rev
  putStrLn "--- cond add ---"
  quickCheck prop_cond_add
  putStrLn "--- cond insert ---"
  quickCheck prop_cond_ins
  putStrLn "--- insert with forAll ---"
  quickCheck prop_ins

--------------------------------------------------------------------------------


