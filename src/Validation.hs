{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2014 Chris Allen, Edward Kmett
            (c) 2018-2023 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lightweight pure data validation based on 'Applicative' and 'Selective' functors.

'Validation' allows to accumulate all errors instead of
short-circuting on the first error so you can display all possible
errors at once.

Common use-cases include:

1. Validating each input of a form with multiple inputs.
2. Performing multiple validations of a single value.

'Validation' provides __modular__ and __composable__ interface which
means that you can implement validations for different pieces of your
data independently, and then combine smaller parts into the validation
of a bigger type. The below table illustrates main ways to combine two
'Validation's:

+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
|   Typeclass   | Operation ○ | 'Failure' e ○ 'Failure' d | 'Success' a ○ 'Success' b | 'Failure' e ○ 'Success' a | 'Success' a ○ 'Failure' e |
+===============+=============+===========================+===========================+===========================+===========================+
| 'Semigroup'   | '<>'        | 'Failure' (e '<>' d)      | 'Success' (a '<>' b)      | 'Failure' e               | 'Failure' e               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
| 'Applicative' | '<*>'       | 'Failure' (e '<>' d)      | 'Success' (a b)           | 'Failure' e               | 'Failure' e               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
| 'Alternative' | '<|>'       | 'Failure' (e '<>' d)      | 'Success' a               | 'Success' a               | 'Success' a               |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+
| 'Selective'   | '<*?'       | 'Failure' e               | 'Selective' choice        | 'Failure' e               | 'Selective' choice        |
+---------------+-------------+---------------------------+---------------------------+---------------------------+---------------------------+

In other words, instances of different standard typeclasses provide
various semantics which can be useful in different use-cases:

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
       , isFailure
       , isSuccess
       , validation
       , failures
       , successes
       , partitionValidations
       , fromFailure
       , fromSuccess

         -- ** 'NonEmpty' combinators
         -- $nonEmptyCombinators
       , failure
       , failureIf
       , failureUnless

         -- ** 'Either' conversion
         -- $either
       , validationToEither
       , eitherToValidation

         -- * Combinators
       , validateAll

         -- ** When* functions
       , whenSuccess
       , whenFailure
       , whenSuccess_
       , whenFailure_
       , whenSuccessM
       , whenFailureM
       , whenSuccessM_
       , whenFailureM_

         -- ** 'Maybe' conversion
       , failureToMaybe
       , successToMaybe
       , maybeToFailure
       , maybeToSuccess
       ) where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.DeepSeq (NFData, NFData1, NFData2 (..))
import Control.Selective (Selective (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Data (Data)
import Data.Foldable (Foldable (..))
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Validation.Combinators


-- $setup
-- >>> import Control.Applicative (liftA3)
-- >>> import Control.Selective (ifS)
-- >>> import Data.Char (isDigit)
-- >>> import Data.Maybe (listToMaybe)
-- >>> import Text.Read (readMaybe)

{- $use

This section contains the typical 'Validation' usage example. Let's say we
have a form with fields where you can input your login information.

>>> :{
data Form = Form
    { formUserName :: !String
    , formPassword :: !String
    }
:}


This @Form@ data type can represent values of some text fields on the
web page or inside the GUI application. Our goal is to create a value of
the custom @User@ data type from the @Form@ fields.

First, let's define our @User@ type and additional @newtype@s for more
type safety.

>>> :{
newtype UserName = UserName
    { unUserName :: String
    } deriving newtype (Show)
:}

>>> :{
newtype Password = Password
    { unPassword :: String
    } deriving newtype (Show)
:}

>>> :{
data User = User
    { userName     :: !UserName
    , userPassword :: !Password
    } deriving stock (Show)
:}

We can easily create a @User@ from the @Form@ in the /unsafe/ way by wrapping
each form field into the corresponding @newtype@:

>>> :{
unsafeUserFromForm :: Form -> User
unsafeUserFromForm Form{..} = User
    { userName     = UserName formUserName
    , userPassword = Password formPassword
    }
:}

However, this conversion is unsafe (as name suggests) since @Form@ can
contain /invalid/ data. So, before creating a @User@ we want to check
whether all @Form@ fields satisfy our preconditions. Specifically:

1. User name must not be empty.
2. Password should be at least 8 characters long.
3. Password should contain at least 1 digit.

'Validation' offers __modular__ and __composable__ way of defining and
outputting all validation failures which means:

1. __Modular__: define validation checks for different fields
independently.
2. __Composable__: combine smaller validations easily into a
validation of a bigger type.

Before implementing @Form@ validation, we need to introduce a type for
representing our validation errors. It is a good practice to define
all possible errors as a single sum type, so let's go ahead:

>>> :{
data FormValidationError
    = EmptyName
    | ShortPassword
    | NoDigitPassword
    deriving stock (Show)
:}

With 'Validation' we can define checks for individual fields
independently and compose them later. First, let's start with defining
validation for the name:

>>> :{
validateName :: String -> Validation (NonEmpty FormValidationError) UserName
validateName name = UserName name <$ failureIf (null name) EmptyName
:}

You can notice a few things about this function:

1. All errors are collected in 'NonEmpty', since we want to have
guarantees that in case of errors we have at least one failure.
2. It wraps the result into @UserName@ to tell that validation is
passed.

Let's see how this function works:

>>> validateName "John"
Success "John"
>>> validateName ""
Failure (EmptyName :| [])

Since 'Validation' provides __modular__ interface for defining checks,
we now can define all validation functions for the password
separately:

>>> :{
validateShortPassword :: String -> Validation (NonEmpty FormValidationError) Password
validateShortPassword password = Password password <$
    failureIf (length password < 8) ShortPassword
:}

>>> :{
validatePasswordDigit :: String -> Validation (NonEmpty FormValidationError) Password
validatePasswordDigit password = Password password <$
    failureUnless (any isDigit password) NoDigitPassword
:}

After we've implemented validations for different @Form@ fields, it's
time to combine them together! 'Validation' offers several ways to
compose different validations. These ways are provided via different
instances of common Haskell typeclasses, specifically:

* 'Semigroup'
* 'Alternative'
* 'Applicative'

'Semigroup' allows combining values inside both 'Failure' and
'Success' but this requires both values to implement the 'Semigroup'
instance. This doesn't fit our goal, since @Password@ can't have a
reasonble 'Semigroup' instance.

'Alternative' returns first 'Success' or combines all 'Failure's. We
can notice that 'Alternative' also doesn't work for us here.

In our case we are interested in collecting all possible errors and
returning 'Success' only when all checks are passed. Fortunately,
'Applicative' is exactly what we need here. So we can use the '*>'
operator to compose all checks for password:

>>> :{
validatePassword :: String -> Validation (NonEmpty FormValidationError) Password
validatePassword password =
    validateShortPassword password *> validatePasswordDigit password
:}


Let's see how it works:

>>> validatePassword "abcd"
Failure (ShortPassword :| [NoDigitPassword])
>>> validatePassword "abcd1"
Failure (ShortPassword :| [])
>>> validatePassword "abcd12345"
Success "abcd12345"

The @validation@ library provides several convenient combinators, so
you can write the password check in a shorter way:

@
validatePassword :: 'String' -> 'Validation' ('NonEmpty' FormValidationError) Password
validatePassword = 'fmap' Password . 'validateAll'
    [ (\`'failureIf'\`     ShortPassword)   . (< 8) . 'length'
    , (\`'failureUnless'\` NoDigitPassword) . 'any' isDigit
    ]
@

After we've implemented validations for all fields, we can compose
them together to produce validation for the whole @User@. As before,
we are going to use the 'Applicative' instance:

>>> :{
validateForm :: Form -> Validation (NonEmpty FormValidationError) User
validateForm Form{..} = User
    <$> validateName formUserName
    <*> validatePassword formPassword
:}

And it works like a charm:

>>> validateForm (Form "" "")
Failure (EmptyName :| [ShortPassword,NoDigitPassword])
>>> validateForm (Form "John" "abc")
Failure (ShortPassword :| [NoDigitPassword])
>>> validateForm (Form "Jonh" "qwertypassword")
Failure (NoDigitPassword :| [])
>>> validateForm (Form "Jonh" "qwertypassword123")
Success (User {userName = "Jonh", userPassword = "qwertypassword123"})
-}

{- | 'Validation' is a polymorphic sum type for storing either all
validation failures or validation success. Unlike 'Either', which
returns only the first error, 'Validation' accumulates all errors
using the 'Semigroup' typeclass.

Usually type variables in @'Validation' e a@ are used as follows:

* @e@: is a list or set of failure messages or values of some error data type.
* @a@: is some domain type denoting successful validation result.

Some typical use-cases:

* @'Validation' ['String'] User@

    * Either list of 'String' error messages or a validated value of a
      custom @User@ type.

* @'Validation' ('NonEmpty' UserValidationError) User@

    * Similar to previous example, but list of failures guaranteed to
      be non-empty in case of validation failure, and it stores values
      of some custom error type.
-}
data Validation e a
    = Failure e
    -- ^ Validation failure. The @e@ type is supposed to implement the 'Semigroup' instance.
    | Success a
    -- ^ Successful validation result of type @a@.
    deriving stock (Eq, Ord, Show, Generic, Generic1, Data)
    deriving anyclass (NFData, NFData1)

{- | Allows changing the value inside 'Success' with a given function.

__Examples__

>>> fmap (+1) (Success 9)
Success 10
>>> fmap (+1) (Failure ["wrong"])
Failure ["wrong"]
-}
instance Functor (Validation e) where
    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)
    {-# INLINE fmap #-}

    (<$) :: a -> Validation e b -> Validation e a
    x <$ Success _ = Success x
    _ <$ Failure e = Failure e
    {-# INLINE (<$) #-}

{- | 'Semigroup' allows merging multiple 'Validation's into single one
by combining values inside both 'Failure' and 'Success'. The '<>'
operator merges two 'Validation's following the below rules:

1. If both values are 'Failure's, returns a new 'Failure' with
accumulated errors.
2. If both values are 'Success'ful, returns a new 'Success' with
combined success using 'Semigroup' for values inside 'Success'.
3. If one value is 'Failure' and another one is 'Success', then
'Failure' is returned.

__Examples__

>>> success1 = Success [9] :: Validation [String] [Int]
>>> success2 = Success [15] :: Validation [String] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [String] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [String] [Int]

>>> success1 <> success2
Success [9,15]
>>> failure1 <> failure2
Failure ["WRONG","FAIL"]
>>> success1 <> failure1
Failure ["WRONG"]
>>> failure2 <> success1 <> success2 <> failure1
Failure ["FAIL","WRONG"]
-}
instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
    (<>) :: Validation e a -> Validation e a -> Validation e a
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

{- | @'mempty' :: 'Validation' e a@ is @Success@ which stores
@'mempty' :: a@ to be consistent with the 'Semigroup' instance.

__Examples__

>>> mempty :: Validation String [Bool]
Success []
-}
instance (Semigroup e, Monoid a) => Monoid (Validation e a) where
    mempty :: Validation e a
    mempty = Success mempty
    {-# INLINE mempty #-}

    mappend :: Validation e a -> Validation e a -> Validation e a
    mappend = (<>)
    {-# INLINE mappend #-}

{- | This instance is the most important instance for the 'Validation' data
type. It's responsible for the many implementations. And it allows to accumulate
errors while performing validation or combining the results in the applicative
style.

__Examples__

>>> success1 = Success 9 :: Validation [String] Int
>>> success2 = Success 15 :: Validation [String] Int
>>> successF = Success (* 2) :: Validation [String] (Int -> Int)
>>> failure1 = Failure ["WRONG"] :: Validation [String] Int
>>> failure2 = Failure ["FAIL"]  :: Validation [String] Int

>>> successF <*> success1
Success 18
>>> successF <*> failure1
Failure ["WRONG"]
>>> (+) <$> success1 <*> success2
Success 24
>>> (+) <$> failure1 <*> failure2
Failure ["WRONG","FAIL"]
>>> liftA2 (+) success1 failure1
Failure ["WRONG"]
>>> liftA3 (,,) failure1 success1 failure2
Failure ["WRONG","FAIL"]

Implementations of all functions are lazy and they correctly work if some
arguments are not fully evaluated.

>>> failure1 *> failure2
Failure ["WRONG","FAIL"]
>>> isFailure $ failure1 *> failure2
True
>>> epicFail = error "Impossible validation" :: Validation [String] Int
>>> isFailure $ failure1 *> epicFail
True
-}
instance Semigroup e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success
    {-# INLINE pure #-}

    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    Failure e1 <*> b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ <*> Failure e = Failure e
    Success f <*> Success a = Success (f a)
    {-# INLINE (<*>) #-}

    (*>) :: Validation e a -> Validation e b -> Validation e b
    Failure e1 *> b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ *> Failure e = Failure e
    Success _ *> Success b = Success b
    {-# INLINE (*>) #-}

    (<*) :: Validation e a -> Validation e b -> Validation e a
    Failure e1 <* b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    Success _ <* Failure e = Failure e
    Success a <* Success _ = Success a
    {-# INLINE (<*) #-}

    liftA2 :: (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
    liftA2 _ (Failure e1) b = Failure $ case b of
        Failure e2 -> e1 <> e2
        Success _  -> e1
    liftA2 _ (Success _) (Failure e) = Failure e
    liftA2 f (Success a) (Success b) = Success (f a b)
    {-# INLINE liftA2 #-}

{- | 'Selective' functors from the [selective](https://hackage.haskell.org/package/selective)
package. This instance allows choosing which validations to apply
based on value inside. 'Validation' can't have a lawful 'Monad'
instance but it's highly desirable to have the monadic behavior in cases
when you want future checks depend on previous values. 'Selective'
allows to circumvent this limitation by providing the desired
behavior.

==== __Examples__

To understand better, how 'Selective' can be helpful, let's consider a
typical usage example with validating passwords.

>>> :{
newtype Password = Password
    { unPassword :: String
    } deriving stock (Show)
:}

When user enters a password in some form, we want to check the
following conditions:

1. Password must not be empty.
2. Password must contain at least 8 characters.
3. Password must contain at least 1 digit.

As in the previous usage example with form validation, let's introduce
a custom data type to represent all possible errors.

>>> :{
data PasswordValidationError
    = EmptyPassword
    | ShortPassword
    | NoDigitPassword
    deriving stock (Show)
:}

And, again, we can implement independent functions to validate all these cases:

>>> type PasswordValidation = Validation (NonEmpty PasswordValidationError) Password

>>> :{
validateEmptyPassword :: String -> PasswordValidation
validateEmptyPassword password = Password password <$
    failureIf (null password) EmptyPassword
:}

>>> :{
validateShortPassword :: String -> PasswordValidation
validateShortPassword password = Password password <$
    failureIf (length password < 8) ShortPassword
:}

>>> :{
validatePasswordDigit :: String -> PasswordValidation
validatePasswordDigit password = Password password <$
    failureUnless (any isDigit password) NoDigitPassword
:}

And we can easily compose all these checks into single validation for
@Password@ using 'Applicative' instance:

>>> :{
validatePassword :: String -> PasswordValidation
validatePassword password =
    validateEmptyPassword password
    *> validateShortPassword password
    *> validatePasswordDigit password
:}

However, if we try using this function, we can notice a problem
immediately:

>>> validatePassword ""
Failure (EmptyPassword :| [ShortPassword,NoDigitPassword])

Due to the nature of the 'Applicative' instance for 'Validation', we
run all checks and combine all possible errors. But you can notice
that if password is empty, it doesn't make sense to run other
validations. The fact that the password is empty implies that password
is shorter than 8 characters.

You may say that check for empty password is redundant because empty
password is a special case of a short password. However, when using
'Validation', we want to display readable and friendly errors to
users, so they know how to fix errors and can act correspondingly.

This behaviour could be achieved easily if 'Validation' had the
'Monad' instance. But it can't have a lawful 'Monad'
instance. Fortunately, the 'Selective' instance for 'Validation' can
help with our problem. But to solve it, we need to write our password
validation in a slightly different way.

First, we need to write a function that checks whether the password is
empty:

>>> :{
checkEmptyPassword :: String -> Validation e Bool
checkEmptyPassword = Success . null
:}

Now we can use the @ifS@ function from the @selective@ package to
branch on the result of @checkEmptyPassword@:

>>> :{
validatePassword :: String -> PasswordValidation
validatePassword password = ifS
    (checkEmptyPassword password)
    (failure EmptyPassword)
    (validateShortPassword password *> validatePasswordDigit password)
:}

With this implementation we achieved our desired behavior:

>>> validatePassword ""
Failure (EmptyPassword :| [])
>>> validatePassword "abc"
Failure (ShortPassword :| [NoDigitPassword])
>>> validatePassword "abc123"
Failure (ShortPassword :| [])
>>> validatePassword "security567"
Success (Password {unPassword = "security567"})
-}
instance Semigroup e => Selective (Validation e) where
    select :: Validation e (Either a b) -> Validation e (a -> b) -> Validation e b
    select (Failure e)   _ = Failure e -- Skip effect after failed conditions
    select (Success eab) f = case eab of
        Left a  -> ($ a) <$> f  -- Apply second effect
        Right b -> Success b    -- Skip second effect
    {-# INLINE select #-}

{- | This instance implements the behaviour when the first 'Success'
is returned. Otherwise all 'Failure's are combined.

__Examples__

>>> success1 = Success [9] :: Validation [String] [Int]
>>> success2 = Success [15] :: Validation [String] [Int]
>>> failure1 = Failure ["WRONG"] :: Validation [String] [Int]
>>> failure2 = Failure ["FAIL"]  :: Validation [String] [Int]

>>> success1 <|> success2
Success [9]
>>> failure1 <|> failure2
Failure ["WRONG","FAIL"]
>>> failure2 <|> success2
Success [15]
-}
instance (Monoid e) => Alternative (Validation e) where
    empty :: Validation e a
    empty = Failure mempty
    {-# INLINE empty #-}

    (<|>) :: Validation e a -> Validation e a -> Validation e a
    s@Success{} <|> _        = s
    _ <|> s@Success{}        = s
    Failure e <|> Failure e' = Failure (e <> e')
    {-# INLINE (<|>) #-}

{- | 'Foldable' for 'Validation' allows folding values inside 'Success'.

__Examples__

>>> fold (Success [16])
[16]
>>> fold (Failure "WRONG!" :: Validation String [Int])
[]
-}
instance Foldable (Validation e) where
    fold :: Monoid m => Validation e m -> m
    fold = \case
        Failure _ -> mempty
        Success a -> a
    {-# INLINE fold #-}

    foldMap :: Monoid m => (a -> m) -> Validation e a -> m
    foldMap f = \case
        Failure _ -> mempty
        Success a -> f a
    {-# INLINE foldMap #-}

    foldr :: (a -> b -> b) -> b -> Validation e a -> b
    foldr f x = \case
        Failure _ -> x
        Success a -> f a x
    {-# INLINE foldr #-}

    foldr' :: (a -> b -> b) -> b -> Validation e a -> b
    foldr' = foldr
    {-# INLINE foldr' #-}

    foldl :: (b -> a -> b) -> b -> Validation e a -> b
    foldl f x = \case
        Failure _ -> x
        Success a -> f x a
    {-# INLINE foldl #-}

    foldl' :: (b -> a -> b) -> b -> Validation e a -> b
    foldl' = foldl
    {-# INLINE foldl' #-}

    toList :: Validation e a -> [a]
    toList = \case
        Failure _ -> []
        Success a -> [a]
    {-# INLINE toList #-}

    null :: Validation e a -> Bool
    null = \case
        Failure _ -> True
        Success _ -> False
    {-# INLINE null #-}

    length :: Validation e a -> Int
    length = \case
        Failure _ -> 0
        Success _ -> 1
    {-# INLINE length #-}

    elem :: Eq a => a -> Validation e a -> Bool
    elem x = \case
        Failure _ -> False
        Success a -> x == a
    {-# INLINE elem #-}

    sum :: Num a => Validation e a -> a
    sum = \case
        Failure _ -> 0
        Success a -> a
    {-# INLINE sum #-}

    product :: Num a => Validation e a -> a
    product = \case
        Failure _ -> 1
        Success a -> a
    {-# INLINE product #-}

    -- not-implemented because they are partial, so we're using the
    -- default implementations
    --
    -- foldr1  :: (a -> a -> a) -> Validation e a -> a
    -- foldl1  :: (a -> a -> a) -> Validation e a -> a
    -- maximum :: Ord a => Validation e a -> a
    -- minimum :: Ord a => Validation e a -> a

{- | Traverse values inside 'Success' with some effectful computation.

__Examples__

>>> parseInt = readMaybe :: String -> Maybe Int
>>> traverse parseInt (Success "42")
Just (Success 42)
>>> traverse parseInt (Success "int")
Nothing
>>> traverse parseInt (Failure ["42"])
Just (Failure ["42"])
-}
instance Traversable (Validation e) where
    traverse :: Applicative f => (a -> f b) -> Validation e a -> f (Validation e b)
    traverse f (Success a) = Success <$> f a
    traverse _ (Failure e) = pure (Failure e)
    {-# INLINE traverse #-}

    sequenceA :: Applicative f => Validation e (f a) -> f (Validation e a)
    sequenceA = \case
        Failure e -> pure (Failure e)
        Success f -> Success <$> f
    {-# INLINE sequenceA #-}

{- | Similar to 'Functor' but allows mapping of values inside both
'Failure' and 'Success'.

__Examples__

>>> bimap length show (Success 50)
Success "50"
>>> bimap length show (Failure ["15", "9"])
Failure 2
-}
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

{- | Similar to 'Foldable' but allows folding both 'Failure' and
'Success' to the same monoidal value according to given functions.

__Examples__

>>> one x = [x]
>>> bifoldMap id (one . show) (Success 15)
["15"]
>>> bifoldMap id (one . show) (Failure ["Wrong", "Fail"])
["Wrong","Fail"]
-}
instance Bifoldable Validation where
--    bifoldMap :: (e -> m) -> (a -> m) -> Validation e a -> m
    bifoldMap f _ (Failure e) = f e
    bifoldMap _ g (Success a) = g a
    {-# INLINE bifoldMap #-}

{- | Similar to 'Traversable' but traverses both 'Failure' and
'Success' with given effectful computations.

__Examples__

>>> parseInt = readMaybe :: String -> Maybe Int
>>> bitraverse listToMaybe parseInt (Success "42")
Just (Success 42)
>>> bitraverse listToMaybe parseInt (Success "int")
Nothing
>>> bitraverse listToMaybe parseInt (Failure [15])
Just (Failure 15)
>>> bitraverse listToMaybe parseInt (Failure [])
Nothing
-}
instance Bitraversable Validation where
    bitraverse
        :: Applicative f
        => (e -> f d)
        -> (a -> f b)
        -> Validation e a
        -> f (Validation d b)
    bitraverse f _ (Failure e) = Failure <$> f e
    bitraverse _ g (Success a) = Success <$> g a
    {-# INLINE bitraverse #-}

instance NFData2 Validation where
    liftRnf2 :: (e -> ()) -> (a -> ()) -> Validation e a -> ()
    liftRnf2 f _s (Failure x) = f x
    liftRnf2 _f s (Success y) = s y

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
    return = pure
    (>>=)  = error "Unreachable Validation instance of Monad"

-- | Helper type family to produce error messages
type family NoValidationMonadError :: Constraint where
    NoValidationMonadError = TypeError
        ( 'Text "Type 'Validation' doesn't have lawful 'Monad' instance"
        ' :$$: 'Text "which means that you can't use 'Monad' methods with 'Validation'."
        )

----------------------------------------------------------------------------
-- Either
----------------------------------------------------------------------------

{- $either
'Validation' is usually compared to the 'Either' data type due to the similarity
in structure, nature and use case. Here is a quick table you can relate to, in
order to see the main properties and differences between these two data types:

+------------------------+---------------------------+---------------------------+
|                        | 'Either'                  | 'Validation'              |
+========================+===========================+===========================+
| Error result           | 'Left'                    | 'Failure'                 |
+------------------------+---------------------------+---------------------------+
| Successful result      | 'Right'                   | 'Success'                 |
+------------------------+---------------------------+---------------------------+
| 'Applicative' instance | Stops on the first 'Left' | Aggregates all 'Failure's |
+------------------------+---------------------------+---------------------------+
| 'Monad' instance       | Lawful instance           | __Cannot__ exist          |
+------------------------+---------------------------+---------------------------+

== Comparison in example

For the sake of better illustration of the difference between 'Either' and
'Validation', let's go through the example of how parsing is done with the usage of
these types.

Our goal is to parse two given 'String's and return their sum in case if both of
them are valid 'Int's. If any of the inputs is failing to be parsed we should
return the @ParseError@ which we are introducing right now:

>>> :{
newtype ParseError = ParseError
    { nonParsedString :: String
    } deriving stock (Show)
:}

Let's first implement the parsing of single input in the 'Either' context:

>>> :{
parseEither :: String -> Either ParseError Int
parseEither input = case readMaybe @Int input of
    Just x  -> Right x
    Nothing -> Left $ ParseError input
:}

And the final function for 'Either' looks like this:

>>> :{
parseSumEither :: String -> String -> Either ParseError Int
parseSumEither str1 str2 = do
    let x = parseEither str1
    let y = parseEither str2
    liftA2 (+) x y
:}

Let's now test it in action.

>>> parseSumEither "1" "2"
Right 3
>>> parseSumEither "NaN" "42"
Left (ParseError {nonParsedString = "NaN"})
>>> parseSumEither "15" "Infinity"
Left (ParseError {nonParsedString = "Infinity"})
>>> parseSumEither "NaN" "infinity"
Left (ParseError {nonParsedString = "NaN"})

__Note__ how in the case of both failed parsing we got only the first @NaN@.

To finish our comparison, let's implement the same functionality using
'Validation' properties.

>>> :{
parseValidation :: String -> Validation (NonEmpty ParseError) Int
parseValidation input = case readMaybe @Int input of
    Just x  -> Success x
    Nothing -> failure $ ParseError input
:}

>>> :{
parseSumValidation :: String -> String -> Validation (NonEmpty ParseError) Int
parseSumValidation str1 str2 = do
    let x = parseValidation str1
    let y = parseValidation str2
    liftA2 (+) x y
:}

It looks almost completely identical except for the resulting type —
@'Validation' ('NonEmpty' ParseError) 'Int'@. But let's see if they behave the
same way:

>>> parseSumValidation "1" "2"
Success 3
>>> parseSumValidation "NaN" "42"
Failure (ParseError {nonParsedString = "NaN"} :| [])
>>> parseSumValidation "15" "infinity"
Failure (ParseError {nonParsedString = "infinity"} :| [])
>>> parseSumValidation "NaN" "infinity"
Failure (ParseError {nonParsedString = "NaN"} :| [ParseError {nonParsedString = "infinity"}])

As expected, with 'Validation' we got __all__ parse 'Failure's we received on
the way.

== Combinators

We are providing several functions for better integration with the 'Either'
related code in this section.
-}

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
-- Interface
----------------------------------------------------------------------------

{- | Predicate on if the given 'Validation' is 'Failure'.

>>> isFailure (Failure 'e')
True
>>> isFailure (Success 'a')
False
-}
isFailure :: Validation e a -> Bool
isFailure = \case
    Failure _ -> True
    Success _ -> False

{- | Predicate on if the given 'Validation' is 'Success'.

>>> isSuccess (Success 'a')
True
>>> isSuccess (Failure 'e')
False
-}
isSuccess :: Validation e a -> Bool
isSuccess = \case
    Success _ -> True
    Failure _ -> False

{- | Transforms the value of the given 'Validation' into @x@ using provided
functions that can transform 'Failure' and 'Success' value into the resulting
type respectively.

>>> let myValidation = validation (<> " world!") (show . (* 10))
>>> myValidation (Success 100)
"1000"
>>> myValidation (Failure "Hello")
"Hello world!"
-}
validation :: (e -> x) -> (a -> x) -> Validation e a -> x
validation fe fa = \case
    Success a -> fa a
    Failure e -> fe e

{- | Filters out all 'Failure' values into the new list of @e@s from the given
list of 'Validation's.

Note that the order is preserved.

>>> failures [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
["Hello","world","!"]
-}
failures :: [Validation e a] -> [e]
failures v = [e | Failure e <- v]
{-# INLINE failures #-}

{- | Filters out all 'Success' values into the new list of @a@s from the given
list of 'Validation's.

Note that the order is preserved.

>>> successes [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
[1,2]
-}
successes :: [Validation e a] -> [a]
successes v = [a | Success a <- v]
{-# INLINE successes #-}

{- | Redistributes the given list of 'Validation's into two lists of @e@s and
@e@s, where the first list contains all values of 'Failure's and the second
one — 'Success'es correspondingly.

Note that the order is preserved.

>>> partitionValidations [Failure "Hello", Success 1, Failure "world", Success 2, Failure "!" ]
(["Hello","world","!"],[1,2])
-}
partitionValidations :: [Validation e a] -> ([e], [a])
partitionValidations = go
  where
    go :: [Validation e a] -> ([e], [a])
    go []               = ([], [])
    go (Failure e:rest) = first  (e:) $ go rest
    go (Success a:rest) = second (a:) $ go rest

{- | Returns the contents of a 'Failure'-value or a default value otherwise.

>>> fromFailure "default" (Failure "failure")
"failure"
>>> fromFailure "default" (Success 1)
"default"
-}
fromFailure :: e -> Validation e a -> e
fromFailure _ (Failure e) = e
fromFailure e _           = e

{- | Returns the contents of a 'Success'-value or a default value otherwise.

>>> fromSuccess 42 (Success 1)
1
>>> fromSuccess 42 (Failure "failure")
42
-}
fromSuccess :: a -> Validation e a -> a
fromSuccess _ (Success a) = a
fromSuccess a _           = a

----------------------------------------------------------------------------
-- NonEmpty Combinators
----------------------------------------------------------------------------

{- $nonEmptyCombinators

When using 'Validation', we often work with the 'NonEmpty' list of errors, and
those lists will be concatenated later.

The following functions aim to help with writing more concise code.

For example, instead of (perfectly fine) code like:

>>> :{
validateNameVerbose :: String -> Validation (NonEmpty String) String
validateNameVerbose name
    | null name = Failure ("Empty Name" :| [])
    | otherwise = Success name
:}

one can write simply:

>>> :{
validateNameSimple :: String -> Validation (NonEmpty String) String
validateNameSimple name = name <$ failureIf (null name) "Empty Name"
:}

-}

{- | Create a 'Failure' of 'NonEmpty' list with a single given error.

>>> failure "I am a failure"
Failure ("I am a failure" :| [])
-}
failure :: e -> Validation (NonEmpty e) a
failure e = Failure (e :| [])
{-# INLINE failure #-}

{- | Returns a 'Failure' in case of the given predicate is 'True'.
Returns @'Success' ()@ otherwise.

>>> let shouldFail = (==) "I am a failure"
>>> failureIf (shouldFail "I am a failure") "I told you so"
Failure ("I told you so" :| [])
>>> failureIf (shouldFail "I am NOT a failure") "okay"
Success ()
-}
failureIf :: Bool -> e -> Validation (NonEmpty e) ()
failureIf p e
    | p = failure e
    | otherwise = Success ()
{-# INLINE failureIf #-}

{- | Returns a 'Failure' unless the given predicate is 'True'.
Returns @'Success' ()@ in case of the predicate is satisfied.

Similar to 'failureIf' with the reversed predicate.

@
'failureUnless' p ≡ 'failureIf' (not p)
@

>>> let shouldFail = (==) "I am a failure"
>>> failureUnless (shouldFail "I am a failure") "doesn't matter"
Success ()
>>> failureUnless (shouldFail "I am NOT a failure") "I told you so"
Failure ("I told you so" :| [])
-}
failureUnless :: Bool -> e -> Validation (NonEmpty e) ()
failureUnless p e
    | p = Success ()
    | otherwise = failure e
{-# INLINE failureUnless #-}
