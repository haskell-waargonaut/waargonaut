{-# LANGUAGE OverloadedStrings #-}
module Generics
  ( genericsTests
  ) where

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (Assertion, testCase, (@?=))

import           Waargonaut.Encode  (Encoder)
import qualified Waargonaut.Encode  as E

import qualified Types.Common       as C

import qualified Waargonaut.Generic as G

testGenericFieldNameFunctionUsedOnce :: Assertion
testGenericFieldNameFunctionUsedOnce = do
  let
    encFudge :: Applicative f => Encoder f C.Fudge
    encFudge = G.untag . G.gEncoder $ C.fudgeJsonOpts
      { G._optionsFieldName = tail
      }

    expected = "{\"udge\":\"Chocolate\"}"

  E.simplePureEncodeTextNoSpaces encFudge C.testFudge @?= expected

genericsTests :: TestTree
genericsTests = testGroup "Generics"
  [ testCase "Options fieldName function is used only once" testGenericFieldNameFunctionUsedOnce
  ]
