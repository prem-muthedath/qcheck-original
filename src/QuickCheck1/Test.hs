{-# LANGUAGE ScopedTypeVariables #-}

-- | Run QuickCheck.
-- author: Prem Muthedath.

-- imported modules use hughes' original QuickCheck implementation (see `REF`).
-- additional source for code, ideas, comments: QuickCheck @ hackage.
-- Prem Muthedath:
--    1. + all code in this module (this code not in Hughes` paper);
--    2. cabal packaging;
--    3. docs (most pulled from QuickCheck @ hackage).

-- REF:
-- hughes: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
-- QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- begriffs: https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

module QuickCheck1.Test
  ( quickCheck
  , verboseCheck
  )
where

import Control.Monad
  ( replicateM
  )

import Data.List
  ( findIndices
  )

import qualified Data.Map as M

import QuickCheck1.Generator    -- code almost entirely from hughes' paper.
import QuickCheck1.Property     -- code almost entirely from hughes' paper.

--------------------------------------------------------------------------------
-- | Running tests.

-- | tests a property and prints the results to stdout.
quickCheck :: Testable a => a -> IO ()
quickCheck prop = check prop False

-- | tests a property & prints the results & all test cases generated to stdout.
verboseCheck :: Testable a => a -> IO ()
verboseCheck prop = check prop True

-- | tests a prpoperty & prints results to stdout. if `verbose`, then prints all 
-- generated test cases as well to stdout.
check :: Testable a => a -> Bool -> IO ()
check prop verbose = do
    results :: [Result] <- generate . replicateM 100 . evaluate $ prop
    print' results verbose

-- | prints results of tests to stdout. if `verbose`, prints appropriate 
-- generated test cases as well to stdout.
print' :: [Result] -> Bool -> IO ()
print' res verbose
    | any failed res =
      printFail
    | length res <= 10 * length (filter bad res) =
      -- limit bad test cases to < 10%.
      printGaveUp
    | otherwise = printPass
  where printFail :: IO ()
        printFail = do
             let i = head $ findIndices failed res
             if verbose
                then printCases (zip [1 .. i+1] res)
                else return ()
             putStrLn $
                "*** Failed! Falsifiable after "
                <> show (i + 1)
                <> " tests:"
             printArgs $ arguments (res !! i)
        printGaveUp :: IO ()
        printGaveUp = do
             let is = findIndices good res
             if verbose
                then printCases $ map (\i -> (i + 1, res !! i)) is
                else return ()
             putStrLn $
                "*** Gave up! Passed only "
                <> show (length is)
                <> " tests."
        printPass :: IO ()
        printPass = do
             let numbered     = zip [1..] res
                 pass         = filter passed res
                 passNumbered = filter (\(_, y) -> passed y) numbered
             if verbose
                then printCases passNumbered
                else return ()
             putStrLn $
                "+++ OK: passed "
                <> show (length pass)
                <> " tests."
             mapM_ (\(k, v) -> putStrLn $
                v
                <> " "
                <> k
              ) $ histogram pass
        printCases :: [(Int, Result)] -> IO ()
        printCases = mapM_ (\(x, y) ->
          do putStrLn $
                "+++ Test case "
                <> show x
                <> ":"
             printArgs $ arguments y)
        printArgs :: [String] -> IO ()
        -- for `id` usage, see https://tinyurl.com/e9cmzc7c (so)
        printArgs = mapM_ (\x -> putStrLn $ "    " <> id x)

--------------------------------------------------------------------------------
-- | helper functions to run tests.

-- | True if test case is a failure.
failed :: (Result -> Bool)
failed = \x -> ok x == Just False

-- | True if test case is invalid.
bad :: (Result -> Bool)
bad = \x -> ok x == Nothing

-- | True if test case is valid.
good :: (Result -> Bool)
good = \x -> ok x /= Nothing

-- | True if test case is a pass.
passed :: (Result -> Bool)
passed = \x -> ok x == Just True

--------------------------------------------------------------------------------
-- | test results analysis.

-- | histogram of generated test cases.
-- returns [(label, percent)], where `percent` is `label` frequency in %.
-- Prem + this function (not in hughes paper).
-- for `foldr` usage, see /u/ bolo @ https://tinyurl.com/bbyw4hc (so)
histogram :: [Result] -> [(String, String)]
histogram res = let freq = M.toList $ testFreq M.empty in
    map (\(k, v) ->
      let percent = (v * 100) `div` (length res)
          val     = show percent <> "%"
      in (k, val)) freq
  where testFreq :: M.Map String Int -> M.Map String Int
        testFreq mp = foldr (\x y -> tcFreq (stamp x) y) mp res
        tcFreq :: [String] -> M.Map String Int -> M.Map String Int
        tcFreq ls mp = foldr (\x y -> M.insertWith (+) x 1 y) mp ls

--------------------------------------------------------------------------------


