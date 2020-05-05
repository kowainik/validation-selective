{-
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

DocTest's run function to keep docs up to date.
-}

module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
    $ "-XDeriveAnyClass"
    : "-XDeriveGeneric"
    : "-XDerivingStrategies"
    : "-XGeneralizedNewtypeDeriving"
    : "-XInstanceSigs"
    : "-XLambdaCase"
    : "-XOverloadedStrings"
    : "-XRecordWildCards"
    : "-XScopedTypeVariables"
    : "-XTypeApplications"
    : [ "src/Validation.hs"
      , "src/Validation/Combinators.hs"
      ]
