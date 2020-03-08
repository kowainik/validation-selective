{- | Generators for test data types.
-}

module Test.Gen
    ( genValidation
    , genFunction
    , genInt
    , genSmallInt
    , genSmallText
    , genSmallList
    , genEither
    ) where

import Data.Text (Text)
import Hedgehog (Gen, MonadGen)
import Validation (Validation (..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- | Generate a simple function from the list.
genFunction :: Gen (Int -> Int)
genFunction = genInt >>= \n -> Gen.element
    [ id
    , (+ n)
    , (* n)
    , const n
    , (n -)
    , subtract n
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
