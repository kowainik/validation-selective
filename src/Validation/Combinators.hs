{- |
Copyright:  (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Helpful combinators to work with 'Validation' data type.
-}

module Validation.Combinators
    ( validateAll

      -- * When* functions
    , whenSuccess
    , whenFailure
    , whenSuccess_
    , whenFailure_
    , whenSuccessM
    , whenFailureM
    , whenSuccessM_
    , whenFailureM_

      -- * 'Maybe' conversion
    , failureToMaybe
    , successToMaybe
    , maybeToFailure
    , maybeToSuccess

    ) where

import Data.Foldable (foldl')

import Validation (Validation (..), validation)


{- | Validate all given checks in a 'Foldable'. Returns the 'Success' of the
start element when all checks are successful.


A basic example of usage could look like this:

@
> let validatePassword = validateAll
        [ validateEmptyPassword
        , validateShortPassword
        ]

> validateAll "VeryStrongPassword"
Success "VeryStrongPassword"

> validateAll ""
Failure (EmptyPassword :| [ShortPassword])
@
-}
validateAll :: (Foldable f, Semigroup e) => f (a -> Validation e b) -> a -> Validation e a
validateAll fs a = foldl' (\res f -> res <* f a) (Success a) fs
{-# INLINE validateAll #-}

{- | Applies the given action to 'Validation' if it is 'Failure' and returns the
result. In case of 'Success' the default value is returned.

>>> whenFailure "bar" (Failure 42) (\a -> "foo" <$ print a)
42
"foo"

>>> whenFailure "bar" (Success 42) (\a -> "foo" <$ print a)
"bar"
-}
whenFailure :: Applicative f => a -> Validation l r -> (l -> f a) -> f a
whenFailure _ (Failure  l) f = f l
whenFailure a (Success _) _  = pure a
{-# INLINE whenFailure #-}

{- | Applies given action to the 'Validation' content if it is 'Failure'.

Similar to 'whenFailure' but the default value is '()'.

>>> whenFailure_ (Success 42) putStrLn
>>> whenFailure_ (Failure "foo") putStrLn
foo
-}
whenFailure_ :: Applicative f => Validation l r -> (l -> f ()) -> f ()
whenFailure_ = whenFailure ()
{-# INLINE whenFailure_ #-}

{- | Monadic version of 'whenFailure'.
Applies monadic action to the given 'Validation' in case of 'Failure'.
Returns the resulting value, or provided default.

>>> whenFailureM "bar" (pure $ Failure 42) (\a -> "foo" <$ print a)
42
"foo"

>>> whenFailureM "bar" (pure $ Success 42) (\a -> "foo" <$ print a)
"bar"
-}
whenFailureM :: Monad m => a -> m (Validation l r) -> (l -> m a) -> m a
whenFailureM a me f = me >>= \e -> whenFailure a e f
{-# INLINE whenFailureM #-}

{- | Monadic version of 'whenFailure_'.
Applies monadic action to the given 'Validation' in case of 'Failure'.
Similar to 'whenFailureM' but the default is '()'.

>>> whenFailureM_ (pure $ Success 42) putStrLn
>>> whenFailureM_ (pure $ Failure "foo") putStrLn
foo
-}
whenFailureM_ :: Monad m => m (Validation l r) -> (l -> m ()) -> m ()
whenFailureM_ me f = me >>= \e -> whenFailure_ e f
{-# INLINE whenFailureM_ #-}

{- | Applies the given action to 'Validation' if it is 'Success' and returns the
result. In case of 'Failure' the default value is returned.

>>> whenSuccess "bar" (Failure "foo") (\a -> "success!" <$ print a)
"bar"

>>> whenSuccess "bar" (Success 42) (\a -> "success!" <$ print a)
42
"success!"
-}
whenSuccess :: Applicative f => a -> Validation l r -> (r -> f a) -> f a
whenSuccess a (Failure  _) _ = pure a
whenSuccess _ (Success r) f  = f r
{-# INLINE whenSuccess #-}

{- | Applies given action to the 'Validation' content if it is 'Success'.

Similar to 'whenSuccess' but the default value is '()'.

>>> whenSuccess_ (Failure "foo") print
>>> whenSuccess_ (Success 42) print
42
-}
whenSuccess_ :: Applicative f => Validation l r -> (r -> f ()) -> f ()
whenSuccess_ = whenSuccess ()
{-# INLINE whenSuccess_ #-}

{- | Monadic version of 'whenSuccess'.
Applies monadic action to the given 'Validation' in case of 'Success'.
Returns the resulting value, or provided default.

>>> whenSuccessM "bar" (pure $ Failure "foo") (\a -> "success!" <$ print a)
"bar"

>>> whenSuccessM "bar" (pure $ Success 42) (\a -> "success!" <$ print a)
42
"success!"
-}
whenSuccessM :: Monad m => a -> m (Validation l r) -> (r -> m a) -> m a
whenSuccessM a me f = me >>= \e -> whenSuccess a e f
{-# INLINE whenSuccessM #-}

{- | Monadic version of 'whenSuccess_'.
Applies monadic action to the given 'Validation' in case of 'Success'.
Similar to 'whenSuccessM' but the default is '()'.

>>> whenSuccessM_ (pure $ Failure "foo") print
>>> whenSuccessM_ (pure $ Success 42) print
42
-}
whenSuccessM_ :: Monad m => m (Validation l r) -> (r -> m ()) -> m ()
whenSuccessM_ me f = me >>= \e -> whenSuccess_ e f
{-# INLINE whenSuccessM_ #-}


{- | Maps 'Failure' of 'Validation' to 'Just'.

>>> failureToMaybe (Failure True)
Just True
>>> failureToMaybe (Success "aba")
Nothing
-}
failureToMaybe :: Validation l r -> Maybe l
failureToMaybe = validation Just (const Nothing)
{-# INLINE failureToMaybe #-}

{- | Maps 'Success' of 'Validation' to 'Just'.

>>> successToMaybe (Failure True)
Nothing
>>> successToMaybe (Success "aba")
Just "aba"
-}
successToMaybe :: Validation l r -> Maybe r
successToMaybe = validation (const Nothing) Just
{-# INLINE successToMaybe #-}

{- | Maps 'Just' to 'Failure' In case of 'Nothing' it wraps the given default
value into 'Success'.

>>> maybeToFailure True (Just "aba")
Failure "aba"
>>> maybeToFailure True Nothing
Success True
-}
maybeToFailure :: r -> Maybe l -> Validation l r
maybeToFailure r = maybe (Success r) Failure
{-# INLINE maybeToFailure #-}

{- | Maps 'Just' to 'Success'. In case of 'Nothing' it wraps the given default
value into 'Failure'

>>> maybeToSuccess True (Just "aba")
Success "aba"
>>> maybeToSuccess True Nothing
Failure True
-}
maybeToSuccess :: l -> Maybe r -> Validation l r
maybeToSuccess l = maybe (Failure l) Success
{-# INLINE maybeToSuccess #-}
