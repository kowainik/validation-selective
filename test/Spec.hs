module Main (main) where

import Test.Hspec (hspec)

import Test.Property (validationLawsSpec)


main :: IO ()
main = hspec validationLawsSpec
