module Json
  ( jsonTests
  ) where

import           Control.Lens                (review,preview,_Empty)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Waargonaut.Types


jsonTests :: TestTree
jsonTests =
  testGroup "Json types"
    [ testCase "CommandSepareted's _Empty prism law"
      $ preview _Empty (review _Empty () :: CommaSeparated WS (JAssoc WS Json)) @?= Just ()
    , testCase "JObject's _Empty prism law"
      $ preview _Empty (review _Empty () :: JObject WS Json) @?= Just ()
    , testCase "JArray's _Empty prism law"
      $ preview _Empty (review _Empty () :: JArray WS Json) @?= Just ()
    , testCase "MapLikeObj's _Empty prism law"
      $ preview _Empty (review _Empty () :: MapLikeObj WS Json) @?= Just ()
    , testCase "WS's _Empty prism law"
      $ preview _Empty (review _Empty () :: WS) @?= Just ()
    ]
