{-# LANGUAGE ScopedTypeVariables #-}

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
-- REF: for combinator classifications, see
-- https://lemonidas.github.io/pdf/Foundational.pdf

-- | enumeration of tests for QuickCheck.
data Test
    -- basic pass/fail
    = Pass
    | Fail

    -- labels
    | Collect
    | Classify

    -- checker combinators
    | Implication
    | GaveUp
    | ForAll

    -- high-level combinators
    | Frequency
    | OrderedList
    | Vector
    | ListOf1
    | RTree         -- randomly generated tree
    | Elements
    | Oneof

    -- low-level combinators
    | Sized
    | Choose
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
  show Frequency    = "*** QuickCheck frequency test ***"
  show OrderedList  = "*** QuickCheck orderedList test ***"
  show Vector       = "*** QuickCheck vector test ***"
  show ListOf1      = "*** QuickCheck listOf1 test ***"
  show RTree        = "*** QuickCheck tree test ***"
  show Elements     = "*** QuickCheck elements test ***"
  show Oneof        = "*** QuickCheck oneof test ***"
  show Sized        = "*** QuickCheck sized test ***"
  show Choose       = "*** QuickCheck choose test ***"

--------------------------------------------------------------------------------
-- | properties to test QuickCheck.

-- ** basic pass/fail.

-- | QuickCheck `pass`.
prop_pass :: Int -> Int -> Bool
prop_pass x y = x + y == y + x

-- | QuickCheck `fail'.
prop_fail :: [Int] -> [Int] -> Bool
prop_fail x y = x ++ y /= y ++ x

-- ** lables.

-- | QuickCheck `collect`.
prop_collect :: [Int] -> Property
prop_collect xs = collect (length xs) $ xs == reverse (reverse xs)

-- | QuickCheck `classify`.
prop_classify :: [Int] -> Property
prop_classify x =
    classify (x==[]) "empty" $
    classify (length x > 10) "has > 5 elements" $
    classify (x /= nub x) "has duplicates" $
    reverse (reverse x) == x

-- ** checker combinators.

-- | QuickCheck implication (==>).
prop_impl :: Int -> Int -> Property
prop_impl x y = (x >= (-25)) ==>
    (classify (x >= (-25)) "passed are >= -25" $ (x + y == y + x))

-- | QuickCheck 'gave up!'.
prop_gave_up :: Int -> [Int] -> Property
prop_gave_up x xs = (ordered xs) ==> (ordered (insert x xs))
  where ordered :: Ord a => [a] -> Bool
        ordered y = (y == sort y)

-- QuickCheck `forAll`.
prop_forAll :: Int -> Property
prop_forAll x = forAll (sort <$> (arbitrary :: Gen [Int])) $ \xs ->
    classify (ordered xs) "obeying forAll condition `sorted`" $
    classify (xs==[]) "empty" $
    classify (length xs > 10) "has > 10 elements" $
    classify (xs /= nub xs) "has duplicates" $
    ordered (insert x xs)
  where ordered :: Ord a => [a] -> Bool
        ordered y = (y == sort y)

-- ** high-level combinators.

-- | QuickCheck `frequency`.
-- REF: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
prop_freq :: Property
prop_freq = forAll myList $ \xs ->
    classify (xs==[]) "empty" $
    classify (length xs >= 1) "has >= 1 element" $
    classify (xs /= nub xs) "has duplicates" $
    reverse (reverse xs) == xs
  where myList :: Gen [Int]
        myList = frequency
          [ (1, return [])
          , (4, ((:) <$> (arbitrary :: Gen Int)) <*> myList)
          ]

-- | QuickCheck `orderedList`.
prop_ord_list :: Property
prop_ord_list = forAll (orderedList :: Gen [Int]) $ \xs ->
    classify (xs == sort (xs)) "ordered" $
    classify (xs==[]) "empty" $
    classify (length xs > 10) "has > 10 elements" $
    classify (xs /= nub xs) "has duplicates" $
    xs == sort xs

-- | QuickCheck `vector`.
prop_vector :: Property
prop_vector = forAll (vector 10 :: Gen [Int]) $ \xs ->
    classify (length xs < 10) "have size < 10" $
    classify (length xs == 10) "have size = 10" $
    classify (length xs > 10) "have size > 10" $
    length xs == 10

-- | QuickCheck `listOf1`.
prop_list1 :: Property
prop_list1 = forAll (listOf1 (arbitrary :: Gen Int)) $
    \xs -> collect (length xs) $
           classify (length xs > 0) "have >= 1 element" $
           length xs >= 1

-- | QuickCheck `tree`.
-- `depth` implementation: see /u/ duplode @ https://tinyurl.com/6hatvhuc (so)
prop_tree :: Property
prop_tree = forAll (arbitrary :: Gen (Tree Int)) $ \x ->
    classify (isValid x) "are valid trees" $
    classify (depth x == 1) "has depth 1" $
    classify (depth x == 2) "has depth 2" $
    classify (depth x == 3) "has depth 3" $
    classify (depth x > 3) "has depth > 3" $
    isValid x
  where depth :: Tree a -> Int
        depth (Leaf _) = 1
        depth (Branch l r) = 1 + max (depth l) (depth r)
        isValid :: Tree Int -> Bool
        isValid (Leaf n) = elem n [(-30) .. 30] -- `30` comes from `generator`.
        isValid (Branch l r) = (isValid l) && (isValid r)

-- | QuickCheck `elements`.
prop_elements :: Property
prop_elements = forAll (elements [1..10]) $
    \(x :: Int) -> classify (x >= 1 && x <= 10) "in range [1, 10]" $
                   elem x [1..10]

-- | QuickCheck `oneof`.
prop_oneof :: Property
prop_oneof = forAll (oneof [
        return []
      , return [1, 2, 5]
      , return [11, 15, 4, 0]
      ]
    ) $ \(xs :: [Int]) ->
          classify (elem xs [[]]) "is empty" $
          classify (elem xs [[1, 2, 5]]) "is [1, 2, 5]" $
          classify (elem xs [[11, 15, 4, 0]]) "is [11, 15, 4, 0]" $
          elem xs [ [], [1, 2, 5], [11, 15, 4, 0] ]

-- ** low-level combinators.

-- | QuickCheck `sized`.
prop_sized :: Property
prop_sized = forAll
    (sized (\(n :: Int) -> choose ((-n), n))) $
    \x -> classify (x > 0 && x <= 30) "in (0, 30]" $
          classify (x == 0) "zero" $
          classify (x < 0 && x >= (-30)) "in [-30, 0)" $
          x >= (-30) && x <= 30     -- comes from `n=30` set in `generator`.

-- | QuickCheck `choose`.
prop_choose :: Property
prop_choose = forAll (choose (0, 15)) $
    \(x :: Int) -> classify (x >= 0 && x <= 15) "in range [0, 15]" $
                   elem x [0..15]

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
             Frequency    -> quickCheck prop_freq
             OrderedList  -> quickCheck prop_ord_list
             Vector       -> quickCheck prop_vector
             ListOf1      -> quickCheck prop_list1
             RTree        -> quickCheck prop_tree
             Elements     -> quickCheck prop_elements
             Oneof        -> quickCheck prop_oneof
             Sized        -> quickCheck prop_sized
             Choose       -> quickCheck prop_choose
      ) [toEnum 0 :: Test ..]

--------------------------------------------------------------------------------


