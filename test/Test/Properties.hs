module Test.Properties
    ( propertiesSpec
    ) where

import Hedgehog (forAll, (===))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Validation (failures, partitionValidations, successes)

import Test.Gen (Property, genSmallText, genValidationList)


propertiesSpec :: Spec
propertiesSpec = describe "Validation interface properties" $
    it "partitionValidations x ≡ (failures x, successes x)"
        partitionSpec

partitionSpec :: Property
partitionSpec = hedgehog $ do
    vs <- forAll $ genValidationList genSmallText
    partitionValidations vs === (failures vs, successes vs)
