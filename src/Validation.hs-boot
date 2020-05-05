module Validation
    ( Validation (..)
    , validation
    ) where


data Validation e a
    = Failure e
    | Success a

instance (Semigroup e) => Applicative (Validation e)

validation :: (e -> x) -> (a -> x) -> Validation e a -> x