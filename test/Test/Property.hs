{- HLINT ignore "Alternative law, right identity" -}
{- HLINT ignore "Alternative law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Functor law" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use mconcat" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Reduce duplication" -}

{-
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Test.Property
    ( validationLawsSpec
    ) where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (sconcat, stimes)
import Data.Text (Text)
import Validation (Validation (..))

import Hedgehog (Gen, PropertyT, forAll, forAllWith, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


validationLawsSpec :: Spec
validationLawsSpec = describe "Validation Property Tests" $ do
    describe "Semigroup instance for Validation" $ do
        it "Associativity: a <> (b <> c) ≡ (a <> b) <> c"
            semigroupAssociativity
        it "Concatenation: sconcat ≡ foldr1 (<>)"
            semigroupConcatenation
        it "Times: stimes n a ≡ foldr1 (<>) (replicate n a)"
            semigroupTimes
    describe "Monoid instance for Validation" $ do
        it "Right Identity: x <> mempty ≡ x" monoidRightIdentity
        it "Left Identity:  mempty <> x ≡ x" monoidLeftIdentity
        it "Associativity: mappend a (mappend b c) ≡ mappend (mappend a b) c"
            monoidAssociativity
        it "Concatenation: mconcat ≡ foldr mappend mempty"
            monoidConcatenation
    describe "Functor instance for Validation" $ do
        it "Identity: fmap id ≡ id"
            functorIdentity
        it "Composition: map f . fmap g ≡ fmap (f . g)"
            functorComposition
        it "Const: fmap (const x) ≡ x <$"
            functorConst
    describe "Applicative instance for Validation" $ do
        it "Identity: pure id <*> x ≡ x"
            applicativeIdentity
        it "Composition: pure (.) <*> f <*> g <*> x ≡ f <*> (g <*> x)"
            applicativeComposition
        it "Homomorphism: pure f <*> pure x ≡ pure (f x)"
            applicativeHomomorphism
        it "Interchange: f <*> pure x ≡ pure ($ x) <*> f"
            applicativeInterchange
        it "Apply Right: u *> v ≡ (id <$ u) <*> v"  applicativeApplyRight
        it "Apply Left:  u <* v ≡ liftA2 const u v" applicativeApplyLeft
    describe "Alternative instance for Validation" $ do
        it "Associativity: a <|> (b <|> c) ≡ (a <|> b) <|> c"
            alternativeAssociativity
        it "Right Identity: x <|> empty ≡ x" alternativeRightIdentity
        it "Left Identity:  empty <|> x ≡ x" alternativeLeftIdentity

-- | Helper alias for tests.
type Property = PropertyT IO ()

----------------------------------------------------------------------------
-- Semigroup instance properties
----------------------------------------------------------------------------

semigroupAssociativity :: Property
semigroupAssociativity = checkAssotiativityFor (genValidation genSmallText) (<>)

semigroupConcatenation :: Property
semigroupConcatenation = do
    let gen = genValidation genSmallText
    a <- forAll gen
    as <- forAll $ genSmallList gen
    let ne = a :| as
    sconcat ne === foldr1 (<>) ne

semigroupTimes :: Property
semigroupTimes = do
    a <- forAll $ genValidation genSmallText
    n <- forAll (Gen.int (Range.linear 2 5))
    stimes n a === foldr1 (<>) (replicate n a)

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

monoidAssociativity :: Property
monoidAssociativity = checkAssotiativityFor (genValidation genSmallText) mappend

monoidConcatenation :: Property
monoidConcatenation = hedgehog $ do
    as <- forAll $ genSmallList $ genValidation genSmallText
    mconcat as === foldr mappend mempty as

----------------------------------------------------------------------------
-- Functor instance laws
----------------------------------------------------------------------------

functorIdentity :: Property
functorIdentity = hedgehog $ do
    a <- forAll $ genValidation genSmallText
    fmap id a === id a

functorComposition :: Property
functorComposition = hedgehog $ do
    a <- forAll $ genValidation genInt
    f <- forAllWith (const "f") genFunction
    g <- forAllWith (const "g") genFunction
    fmap f (fmap g a) === fmap (f . g) a

functorConst :: Property
functorConst = hedgehog $ do
    a <- forAll $ genValidation genSmallText
    let x = 'X'
    fmap (const x) a === (x <$ a)

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
    vx <- forAll $ genValidation genInt
    (pure (.) <*> vf <*> vg <*> vx) === (vf <*> (vg <*> vx))

applicativeHomomorphism :: Property
applicativeHomomorphism = hedgehog $ do
    f <- forAllWith (const "f") genFunction
    x <- forAll genInt
    (pure f <*> pure x) === pure @(Validation [Text]) (f x)

applicativeInterchange :: Property
applicativeInterchange = hedgehog $ do
    vf <- forAllWith (const "f") $ genValidation genFunction
    x <- forAll genInt
    (vf <*> pure x) === (pure ($ x) <*> vf)

applicativeApplyRight :: Property
applicativeApplyRight = hedgehog $ do
    let genVal = genValidation genInt
    vy <- forAll genVal
    vx <- forAll genVal
    (vy *> vx) === ((id <$ vy) <*> vx)

applicativeApplyLeft :: Property
applicativeApplyLeft = hedgehog $ do
    let genVal = genValidation genInt
    vy <- forAll genVal
    vx <- forAll genVal
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
genFunction = genInt >>= \n -> Gen.element
    [ id
    , (+ n)
    , (* n)
    , const n
    , (n -)
    , subtract n
    ]

-- | Generate an 'Int' within the range of @(-10, 10)@.
genInt :: Gen Int
genInt = Gen.enumBounded

-- | Generate a 'Text' of length @3-7@.
genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 0 10) Gen.unicode

-- | Generate a small list of the given generated elements.
genSmallList :: Gen a -> Gen [a]
genSmallList = Gen.list (Range.linear 0 6)

-- | Generate a 'Validation'.
genValidation :: Gen a -> Gen (Validation [Text] a)
genValidation gen = Gen.choice
    [ Success <$> gen
    , Failure <$> Gen.list (Range.linear 0 5) genSmallText
    ]
