{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds#-}

module Encoder.Issue69 (issue69Test) where

import           Data.Functor.Identity (Identity (runIdentity))

import            Data.Text          (Text)
import            Generics.SOP.TH    (deriveGeneric)

import           Test.Tasty          (TestTree)
import           Test.Tasty.HUnit    ((@?=), testCase)

import           Waargonaut.Encode   (Encoder)
import qualified Waargonaut.Encode     as E
import           Waargonaut.Generic  (GWaarg, JsonEncode (..), Options (..),
                                      Tagged (..),
                                      defaultOpts, gEncoder, untag)


data Foo = Foo
  { abc :: Text
  }
$(deriveGeneric ''Foo)

instance JsonEncode GWaarg Foo where
  mkEncoder = gEncoder (defaultOpts { _optionsFieldName  = tail })

fooEncoder :: Applicative f => Encoder f Foo
fooEncoder = untag (mkEncoder :: Applicative f => Tagged GWaarg (Encoder f Foo))

issue69Test :: TestTree
issue69Test = testCase "#69" $
  runIdentity (E.simpleEncodeText fooEncoder (Foo "test")) @?= "{\"bc\":\"test\"}"
