{-# LANGUAGE ScopedTypeVariables #-}

-- | Combinators for constructing properties.
-- code source: original QuickCheck implementation, Hughes, et al.
-- see hughes: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
-- module, function comments: QuickCheck @ hackage: https://tinyurl.com/e98m55wc
-- Prem Muthedath:
--    1. +, modified type signatures, pattern signatures;
--    2. cabal packaging;
--    3. comments.

module Property where

import Generator
import Arbitrary
  ( Arbitrary (..)
  )

--------------------------------------------------------------------------------
-- | Property.

-- | type of properties.
newtype Property = Prop (Gen Result)

-- | class of properties, i.e., types QuickCheck knows how to test. typically, a 
-- property will be a function returning `bool` or `Property`.
class Testable a where
  -- | convert the thing to a property.
  property :: a -> Property

-- | instances.
instance Testable Bool where
  property b = result (nothing {ok = Just b})

instance Testable Property where
  property prop = prop

-- | Prem + `forall` to type signature, + `arbitrary` type signature.
instance forall a b. (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll (arbitrary :: Gen a) f

--------------------------------------------------------------------------------
-- | Result.

-- | Result type -- the result of a single test.
data Result = Result
  { ok :: Maybe Bool
  -- result of a test case.
  , stamp :: [String]
  -- test case labels (or stamp, as named here).
  , arguments :: [String]
  -- generated test case.
  }

-- | no result -- i.e., test neither passed nor failed.
nothing :: Result
nothing = Result {ok = Nothing, stamp = [], arguments = []}

-- | creates a property from a result.
result :: Result -> Property
result res = Prop (return res)

evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen :: Property = property a

--------------------------------------------------------------------------------
-- | Property combinators.

-- attaches a label to a test case. used for reporting test case distribution.
label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
  where add :: Result -> Result
        add res = res {stamp = s : stamp res}

-- | attaches a label to a test case. used for reporting test case distribution.
collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

-- | reports how many test cases satisfy a given condition.
classify :: Testable a => Bool -> String -> a -> Property
classify True name    = label name
classify False _      = property

-- implication for properties.
(==>) :: Testable a => Bool -> a -> Property
True ==> a  = property a
False ==> _ = result nothing

-- | explicit universal quantification -- uses an explcitly supplied generator.
-- Prem + `forall` to type signature, + pattern type signatures.
forAll :: forall a b. (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
    do a :: a        <- gen
       res :: Result <- evaluate (body a)
       return (arg a res)
  where arg :: a -> Result -> Result
        arg a res = res {arguments = show a : arguments res}

--------------------------------------------------------------------------------


