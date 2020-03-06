{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2014 Chris Allen, Edward Kmett
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lightweight pure validation based on 'Applicative' and 'Selective' functors.

'Validation' is a monoidal sibling to 'Either' but 'Validation' doesn't have a
'Monad' instance. 'Validation' allows to accumulate all errors instead of
short-circuting on the first error so you can display all possible errors at
once.

Common use-cases include:

1. Validating each input of a form with multiple inputs.
2. Performing multiple validations of a single value.

Instances of different standard typeclasses provide various semantics:

1. 'Semigroup': accumulate both 'Failure' and 'Success' with '<>'.
2. 'Monoid': 'Success' that stores 'mempty'.
3. 'Functor': change the type inside 'Success'.
4. 'Bifunctor': change both 'Failure' and 'Success'.
5. 'Applicative': apply function to values inside 'Success' and accumulate
   errors inside 'Failure'.
6. 'Alternative': return the first 'Success' or accumulate all errors
   inside 'Failure'.
7. 'Selective': choose which validations to apply based on the value
   inside.
-}

module Validation
       ( -- * Type
         Validation (..)

         -- * How to use
         -- $use

         -- * Interface functions
       , validationToEither
       , eitherToValidation
       ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Selective (Selective (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (..), TypeError)

#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
#endif

-- >>> $setup
-- >>> import Control.Selective (ifS)

{- $use

To give you an example of usage of 'Validation' let's take a @Computer@  data type that needs to be validated:

>>> :{
data Computer = Computer
    { computerRam  :: !Int  -- ^ Ram in Gigabytes
    , computerCpus :: !Int
    } deriving stock (Eq, Show)
:}

You can validate that the computer has a minimum of 16GB of RAM:

>>> :{
validateRam :: Int -> Validation [Text] Int
validateRam ram
    | ram >= 16 = Success ram
    | otherwise = Failure ["Not enough RAM"]
:}

and that the processor has at least two CPUs:

>>> :{
validateCpus :: Int -> Validation [Text] Int
validateCpus cpus
    | cpus >= 2 = Success cpus
    | otherwise = Failure ["Not enough CPUs"]
:}

You can use these functions with the 'Applicative' instance of the 'Validation'
type to construct a validated @Computer@. You will get either (pun intended) a
valid @Computer@ or the errors that prevent it from being considered valid.

Like so:

>>> :{
mkComputer :: Int -> Int -> Validation [Text] Computer
mkComputer ram cpus = Computer
    <$> validateRam ram
    <*> validateCpus cpus
:}

Using @mkComputer@ we get a @Success Computer@ or a list with all possible errors:

>>> mkComputer 16 2
Success (Computer {computerRam = 16, computerCpus = 2})

>>> mkComputer 16 1
Failure ["Not enough CPUs"]

>>> mkComputer 15 2
Failure ["Not enough RAM"]

>>> mkComputer 15 1
Failure ["Not enough RAM","Not enough CPUs"]
-}

-- | 'Validation' is 'Either' with a 'Left' that is a 'Semigroup'.
data Validation e a
    = Failure e
    | Success a
    deriving stock (Eq, Ord, Show)

instance Functor (Validation e) where
    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)
    {-# INLINE fmap #-}

    (<$) :: a -> Validation e b -> Validation e a
    x <$ Success _ = Success x
    _ <$ Failure e = Failure e
    {-# INLINE (<$) #-}

{- | This instance covers the following cases:

1. Both 'Success': combine values inside 'Success' with '<>'.
2. Both 'Failure': combine values inside 'Failure' with '<>'.
3. One 'Success', one 'Failure': return 'Failure'.

__Examples__

>>> success1 = Success [42] :: Validation [Text] [Int]
>>> success2 = Success [69] :: Validation [Text] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [Text] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [Text] [Int]

>>> success1 <> success2
Success [42,69]

>>> failure1 <> failure2
Failure ["WRONG","FAIL"]

>>> success1 <> failure1
Failure ["WRONG"]

-}
instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
    (<>) :: Validation e a -> Validation e a -> Validation e a
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

{- | 'mempty' is @'Success' 'mempty'@.
-}
instance (Semigroup e, Semigroup a, Monoid a) => Monoid (Validation e a) where
    mempty :: Validation e a
    mempty = Success mempty
    {-# INLINE mempty #-}

    mappend :: Validation e a -> Validation e a -> Validation e a
    mappend = (<>)
    {-# INLINE mappend #-}

{- | This instance if the most important instance for the 'Validation' data
type. It's responsible for the many implementations. And it allows to accumulate
errors while performing validation or combining the results in the applicative
style.

__Examples__

>>> success1 = Success 42 :: Validation [Text] Int
>>> success2 = Success 69 :: Validation [Text] Int
>>> successF = Success (* 2) :: Validation [Text] (Int -> Int)
>>> failure1 = Failure ["WRONG"] :: Validation [Text] Int
>>> failure2 = Failure ["FAIL"]  :: Validation [Text] Int

>>> successF <*> success1
Success 84

>>> successF <*> failure1
Failure ["WRONG"]

>>> (+) <$> success1 <*> success2
Success 111

>>> (+) <$> failure1 <*> failure2
Failure ["WRONG","FAIL"]

>>> liftA2 (+) success1 failure1
Failure ["WRONG"]

>>> liftA3 (,,) failure1 success1 failure2
Failure ["WRONG","FAIL"]

Implementations of all functions are lazy and they correctly work if some
arguments are not fully evaluated.

>>> :{
isFailure :: Validation e a -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False
:}

>>> failure1 *> failure2
Failure ["WRONG","FAIL"]
>>> isFailure $ failure1 *> failure2
True
>>> epicFail = error "Impossible validation" :: Validation [Text] Int
>>> isFailure $ failure1 *> epicFail
True
-}
instance Semigroup e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success
    {-# INLINE pure #-}

    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    Failure e <*> b = Failure $ case b of
        Failure e' -> e <> e'
        Success _  -> e
    Success _ <*> Failure e  = Failure e
    Success f <*> Success a = Success (f a)
    {-# INLINE (<*>) #-}

    (*>) :: Validation e a -> Validation e b -> Validation e b
    Failure e *> b = Failure $ case b of
        Failure e' -> e <> e'
        Success _  -> e
    Success _ *> Failure e  = Failure e
    Success _ *> Success b  = Success b
    {-# INLINE (*>) #-}

    (<*) :: Validation e a -> Validation e b -> Validation e a
    Failure e <* b = Failure $ case b of
        Failure e' -> e <> e'
        Success _  -> e
    Success _ <* Failure e  = Failure e
    Success a <* Success _  = Success a
    {-# INLINE (<*) #-}

{- | 'Selective' functors from the [selective](https://hackage.haskell.org/package/selective)
package. This instance allows choosing which validations to apply
based on value inside. 'Validation' can't have a lawful 'Monad'
instance but it's highly desirable to have the monadic behavior in cases
when you want future checks depend on previous values. 'Selective'
allows to circumvent this limitation by providing the desired
behavior.

==== __Example__

The following usage example is taken from the [Selective Applicative Functors](https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf)
paper by Andrey Mokhov, Georgy Lukyanov, Simon Marlow, Jeremie Dimino.


Let's say we have a shape — a circle or a rectangle:


>>> :{
newtype Radius = Radius Int deriving newtype (Num, Show)
newtype Height = Height Int deriving newtype (Num, Show)
newtype Width  = Width  Int deriving newtype (Num, Show)
:}

>>> :{
data Shape
    = Circle Radius
    | Rectangle Width Height
    deriving stock (Show)
:}

We define a function that constructs a shape given a choice of the
shape @x@, and the shape’s parameters (@r@, @w@, and @h@) in an
arbitrary selective functor @f@. You can think of the inputs as
results of reading the corresponding fields from a web form, where @x@
is a checkbox, and all other fields are numeric textboxes, some of
which may be empty.

>>> :{
shape :: Selective f => f Bool -> f Radius -> f Width -> f Height -> f Shape
shape x r w h = ifS x (Circle <$> r) (Rectangle <$> w <*> h)
:}

We choose @f = 'Validation' [String]@ to report the errors that
occurred when reading values from the form. Let us see how this works.

>>> shape (Success True) (Success 1) (Failure ["width?"]) (Failure ["height?"])
Success (Circle 1)
>>> shape (Success False) (Failure ["radius?"]) (Success 2) (Success 3)
Success (Rectangle 2 3)
>>> shape (Success False) (Success 1) (Failure ["width?"]) (Failure ["height?"])
Failure ["width?", "height?"]
>>> shape (Failure ["choice?"]) (Failure ["radius?"]) (Success 2) (Failure ["height?"])
Failure ["choice?"]

In the last example, since the shape’s choice could not be read, we do
not report any subsequent errors. But it does not mean we are
short-circuiting the validation: we will continue accumulating errors
as soon as we get out of the failed conditional, as demonstrated
below.

>>> :{
twoShapes :: Applicative f => f Shape -> f Shape -> f (Shape, Shape)
twoShapes = liftA2 (,)
:}

>>> s1 = shape (Failure ["choice 1?"]) (Success 1) (Failure ["width 1?"]) (Success 3)
>>> s2 = shape (Success False) (Success 1) (Success 2) (Failure ["height 2?"])
>>> twoShapes s1 s2
Failure ["choice 1?","height 2?"]
-}
instance Semigroup e => Selective (Validation e) where
    select :: Validation e (Either a b) -> Validation e (a -> b) -> Validation e b
    select (Failure e)   _ = Failure e -- Skip effect after failed conditions
    select (Success eab) f = case eab of
        Left a  -> ($ a) <$> f  -- Apply second effect
        Right b -> Success b    -- Skip second effect
    {-# INLINE select #-}

{- | This instance implements the following behavior for the binary operator:

1. Both 'Failure': combine values inside 'Failure' using '<>'.
2. At least is 'Success': return the left 'Success' (the earliest 'Success').
3. 'empty' is @'Failure' 'mempty'@.

__Examples__

>>> success1 = Success [42] :: Validation [Text] [Int]
>>> success2 = Success [69] :: Validation [Text] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [Text] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [Text] [Int]

>>> success1 <|> success2
Success [42]
>>> failure1 <|> failure2
Failure ["WRONG","FAIL"]
>>> failure2 <|> success2
Success [69]
-}
instance (Semigroup e, Monoid e) => Alternative (Validation e) where
    empty :: Validation e a
    empty = Failure mempty
    {-# INLINE empty #-}

    (<|>) :: Validation e a -> Validation e a -> Validation e a
    s@Success{} <|> _ = s
    _ <|> s@Success{} = s
    Failure e <|> Failure e' = Failure (e <> e')
    {-# INLINE (<|>) #-}

instance Foldable (Validation e) where
    fold :: Monoid m => Validation e m -> m
    fold (Success a) = a
    fold (Failure _) = mempty
    {-# INLINE fold #-}

    foldMap :: Monoid m => (a -> m) -> Validation e a -> m
    foldMap _ (Failure _) = mempty
    foldMap f (Success a) = f a
    {-# INLINE foldMap #-}

    foldr :: (a -> b -> b) -> b -> Validation e a -> b
    foldr f x (Success a) = f a x
    foldr _ x (Failure _) = x
    {-# INLINE foldr #-}

instance Traversable (Validation e) where
    traverse :: Applicative f => (a -> f b) -> Validation e a -> f (Validation e b)
    traverse f (Success a) = Success <$> f a
    traverse _ (Failure e) = pure (Failure e)
    {-# INLINE traverse #-}

    sequenceA :: Applicative f => Validation e (f a) -> f (Validation e a)
    sequenceA = traverse id
    {-# INLINE sequenceA #-}

instance Bifunctor Validation where
    bimap :: (e -> d) -> (a -> b) -> Validation e a -> Validation d b
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Success a) = Success (g a)
    {-# INLINE bimap #-}

    first :: (e -> d) -> Validation e a -> Validation d a
    first f (Failure e) = Failure (f e)
    first _ (Success a) = Success a
    {-# INLINE first #-}

    second :: (a -> b) -> Validation e a -> Validation e b
    second _ (Failure e) = Failure e
    second g (Success a) = Success (g a)
    {-# INLINE second #-}

#if MIN_VERSION_base(4,10,0)
instance Bifoldable Validation where
--    bifoldMap :: (e -> m) -> (a -> m) -> Validation e a -> m
    bifoldMap f _ (Failure e) = f e
    bifoldMap _ g (Success a) = g a
    {-# INLINE bifoldMap #-}

instance Bitraversable Validation where
    bitraverse :: Applicative f
               => (e -> f d) -> (a -> f b) -> Validation e a -> f (Validation d b)
    bitraverse f _ (Failure e) = Failure <$> f e
    bitraverse _ g (Success a) = Success <$> g a
    {-# INLINE bitraverse #-}
#endif

{- | Transform a 'Validation' into an 'Either'.

>>> validationToEither (Success "whoop")
Right "whoop"

>>> validationToEither (Failure "nahh")
Left "nahh"
-}
validationToEither :: Validation e a -> Either e a
validationToEither = \case
    Failure e -> Left e
    Success a -> Right a
{-# INLINE validationToEither #-}

{- | Transform an 'Either' into a 'Validation'.

>>> eitherToValidation (Right "whoop")
Success "whoop"

>>> eitherToValidation (Left "nahh")
Failure "nahh"
-}
eitherToValidation :: Either e a -> Validation e a
eitherToValidation = \case
    Left e  -> Failure e
    Right a -> Success a
{-# INLINE eitherToValidation #-}

----------------------------------------------------------------------------
-- Custom errors
----------------------------------------------------------------------------

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

It's not possible to implement lawful 'Monad' instance for 'Validation'.

In case it is used by mistake, the user will see the following:

>>> Success 42 >>= \n -> if even n then Success n else Failure ["Not even"]
...
... Type 'Validation' doesn't have lawful 'Monad' instance
      which means that you can't use 'Monad' methods with 'Validation'.
...
-}
instance (NoValidationMonadError, Semigroup e) => Monad (Validation e) where
    return = error "Unreachable Validation instance of Monad"
    (>>=)  = error "Unreachable Validation instance of Monad"

-- | Helper type family to produce error messages
type family NoValidationMonadError :: Constraint where
    NoValidationMonadError = TypeError
        ( 'Text "Type 'Validation' doesn't have lawful 'Monad' instance"
        ':$$: 'Text "which means that you can't use 'Monad' methods with 'Validation'."
        )
