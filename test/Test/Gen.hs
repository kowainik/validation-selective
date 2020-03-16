{- | Generators for test data types.
-}

module Test.Gen
    ( Property
    , genValidation
    , genValidationList
    , genFunction
    , genFunction2
    , genInt
    , genSmallInt
    , genSmallText
    , genSmallList
    , genEither
    ) where

import Data.Text (Text)
import Hedgehog (Gen, MonadGen, PropertyT)
import Validation (Validation (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Helper alias for tests.
type Property = PropertyT IO ()

-- | Generate a simple unary function from the list.
genFunction :: Gen (Int -> Int)
genFunction = genInt >>= \n -> Gen.element
    [ id
    , (+ n)
    , (* n)
    , const n
    , (n -)
    , subtract n
    ]

-- | Generate a simple binary function from the list.
genFunction2 :: Gen (Int -> Int -> Int)
genFunction2 = Gen.element
    [ const
    , (+)
    , (*)
    , (-)
    , subtract
    ]

-- | Generate an 'Int'.
genInt :: Gen Int
genInt = Gen.enumBounded

-- | Generate a positive 'Int' within the range of @1-6@.
genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear 1 6)

-- | Generate a 'Text' of length @0-10@.
genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 0 10) Gen.unicode

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)

-- | Generate a 'Validation'.
genValidation :: Gen a -> Gen (Validation [Text] a)
genValidation gen = Gen.choice
    [ Success <$> gen
    , Failure <$> genSmallList genSmallText
    ]

-- | Generate 'Either' with more frequent 'Right's.
genEither :: MonadGen m => m e -> m a -> m (Either e a)
genEither genE genA = Gen.sized $ \n -> Gen.frequency
    [ (2, Left <$> genE)
    , (1 + fromIntegral n, Right <$> genA)
    ]

-- | Generate a list of 'Validation's.
genValidationList :: Gen a -> Gen [Validation [Text] a]
genValidationList = Gen.list (Range.linear 0 200) . genValidation
