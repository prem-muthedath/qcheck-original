#### qcheck-original

Original QuickCheck implementation (Claessen & Hughes -- see REF), with code 
added on by Prem Muthedath.

Purpose: study & understand QuickCheck. Using original version is very apt, as 
it is simple & short, yet has all the key features of QuickCheck today.

REF:
  1. https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
  2. QuickCheck @ hackage: https://tinyurl.com/e98m55wc

Module & function docs, code (some modified), ideas sourced from QuickCheck @ 
hackage: https://tinyurl.com/e98m55wc

Prem Muthedath: all test code, code to run QuickCheck, cabal packaging, docs.

HOW TO RUN THE PROGRAM AND TESTS:
  1. `cd` to `qcheck-original`, the top directory containing this `README` file.
  2. to load QuickCheck library in `GHCi`, type below command & press `RETURN`:
        - `cabal v2-repl :qcheck-original`

     next, enable `ScopedTypeVariables` extension in `GHCi`, using the `:set` 
     command, as shown below.
        - `:set -XScopedTypeVariables`

     you can now use the library in `GHCi`, just as you would use any library; 
     see below for examples of commands you can enter at `GHCi` prompt:
        - `quickCheck (\(x :: Int) -> x + 1 == 1 + x)`
        - `verboseCheck (\(x :: Int) -> x + 1 == 1 + x)`
        - `do; y <- generate $ evaluate (\(x :: Int) -> x + 1 == 1 + x); print 
          (ok y)`
  3. to run all tests on QuickCheck on the commandline, type below command & 
     press `RETURN`:
        - `cabal v2-run :qcheck-original-test`
  4. to run all tests through `GHCi`, first type below command & press `ENTER`, 
     which will load the tests in `GHCi`:
        - `cabal v2-repl :qcheck-original-test`

      next, to run all tests in `GHCi`, type below command & press `ENTER`:
        - `main`
  5. to run individual tests in `GHCi`, first type below command & press 
     `ENTER`, which will load the tests in `GHCi`:
        - `cabal v2-repl :qcheck-original-test`

     you can then run any test you wish in `GHCi`; some examples:
        - `quickCheck prop_pass`
        - `quickCheck prop_fail`
        - `quickCheck prop_choose`


