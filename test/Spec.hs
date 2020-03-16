module Main (main) where

import Test.Hspec (hspec)

import Test.Laws (validationLawsSpec)
import Test.Properties (propertiesSpec)


main :: IO ()
main = hspec $ do
    validationLawsSpec
    propertiesSpec
