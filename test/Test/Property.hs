{- HLINT ignore "Alternative law, right identity" -}
{- HLINT ignore "Alternative law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Use <$>" -}

{-
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Test.Property
    ( validationLawsSpec
    ) where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Data.Text (Text)
import Validation (Validation (..))

import Hedgehog (Gen, PropertyT, forAll, forAllWith, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


validationLawsSpec :: Spec
validationLawsSpec = describe "Validation Property Tests" $ do
    describe "Semigroup instance for Validation" $
        it "associativity: a <> (b <> c) ≡ (a <> b) <> c"
            semigroupAssociativity
    describe "Monoid instance for Validation" $ do
        it "right identity: x <> mempty ≡ x" monoidRightIdentity
        it "left identity:  mempty <> x ≡ x" monoidLeftIdentity
    describe "Applicative instance for Validation" $ do
        it "identity: pure id <*> x ≡ x" applicativeIdentity
        it "composition: pure (.) <*> f <*> g <*> x ≡ f <*> (g <*> x)"
            applicativeComposition
        it "homomorphism: pure f <*> pure x ≡ pure (f x)"
            applicativeHomomorphism
        it "interchange: f <*> pure x ≡ pure ($ x) <*> f"
            applicativeInterchange
        it "apply right: u *> v ≡ (id <$ u) <*> v"  applicativeApplyRight
        it "apply left:  u <* v ≡ liftA2 const u v" applicativeApplyLeft
    describe "Alternative instance for Validation" $ do
        it "associativity: a <|> (b <|> c) ≡ (a <|> b) <|> c"
            alternativeAssociativity
        it "right identity: x <|> empty ≡ x" alternativeRightIdentity
        it "left identity:  empty <|> x ≡ x" alternativeLeftIdentity

-- | Helper alias for tests.
type Property = PropertyT IO ()

----------------------------------------------------------------------------
-- Semigroup instance properties
----------------------------------------------------------------------------

semigroupAssociativity :: Property
semigroupAssociativity = checkAssotiativityFor (genValidation genSmallText) (<>)

----------------------------------------------------------------------------
-- Monoid instance properties
----------------------------------------------------------------------------

monoidRightIdentity :: Property
monoidRightIdentity = hedgehog $ do
    x <- forAll $ genValidation genSmallText
    x <> mempty === x

monoidLeftIdentity :: Property
monoidLeftIdentity = hedgehog $ do
    x <- forAll $ genValidation genSmallText
    mempty <> x === x

----------------------------------------------------------------------------
-- Applicative instance properties
----------------------------------------------------------------------------

applicativeIdentity :: Property
applicativeIdentity = hedgehog $ do
    vx <- forAll $ genValidation genSmallText
    (pure id <*> vx) === vx

applicativeComposition :: Property
applicativeComposition = hedgehog $ do
    vf <- forAllWith (const "f") $ genValidation genFunction
    vg <- forAllWith (const "g") $ genValidation genFunction
    vx <- forAll $ genValidation genSmallInt
    (pure (.) <*> vf <*> vg <*> vx) === (vf <*> (vg <*> vx))

applicativeHomomorphism :: Property
applicativeHomomorphism = hedgehog $ do
    f <- forAllWith (const "f") genFunction
    x <- forAll genSmallInt
    (pure f <*> pure x) === pure @(Validation [Text]) (f x)

applicativeInterchange :: Property
applicativeInterchange = hedgehog $ do
    vf <- forAllWith (const "f") $ genValidation genFunction
    x <- forAll genSmallInt
    (vf <*> pure x) === (pure ($ x) <*> vf)

applicativeApplyRight :: Property
applicativeApplyRight = hedgehog $ do
    vy <- forAll $ genValidation genSmallInt
    vx <- forAll $ genValidation genSmallInt
    (vy *> vx) === ((id <$ vy) <*> vx)

applicativeApplyLeft :: Property
applicativeApplyLeft = hedgehog $ do
    vy <- forAll $ genValidation genSmallInt
    vx <- forAll $ genValidation genSmallInt
    (vy <* vx) === liftA2 const vy vx

----------------------------------------------------------------------------
-- Alternative instance properties
----------------------------------------------------------------------------

alternativeAssociativity :: Property
alternativeAssociativity = checkAssotiativityFor (genValidation genSmallText) (<|>)

alternativeRightIdentity :: Property
alternativeRightIdentity = hedgehog $ do
    x <- forAll $ genValidation genSmallText
    (x <|> empty) === x

alternativeLeftIdentity :: Property
alternativeLeftIdentity = hedgehog $ do
    x <- forAll $ genValidation genSmallText
    (empty <|> x) === x

----------------------------------------------------------------------------
-- Property helpers
----------------------------------------------------------------------------

{- | Property test for the associativity law:

@
a ⊗ (b ⊗ c) ≡ (a ⊗ b) ⊗ c
@
-}
checkAssotiativityFor
    :: (Show a, Eq a)
    => Gen a
    -> (a -> a -> a)
    -> Property
checkAssotiativityFor gen op = hedgehog $ do
    a <- forAll gen
    b <- forAll gen
    c <- forAll gen
    a `op` (b `op` c) === (a `op` b) `op` c

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- | Generate a simple function from the list.
genFunction :: Gen (Int -> Int)
genFunction = Gen.element [(+), (*), const] <*> genSmallInt

-- | Generate an 'Int' within the range of @(-10, 10)@.
genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear (-10) 10)

-- | Generate a 'Text' of length @3-7@.
genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 3 10) Gen.unicode

-- | Generate a 'Validation'.
genValidation :: Gen a -> Gen (Validation [Text] a)
genValidation gen = Gen.choice
    [ Success <$> gen
    , Failure <$> Gen.list (Range.linear 1 5) genSmallText
    ]
