module Main where

import           Test.DocTest

main :: IO ()
main = doctest
  [ "test/Utils.hs"
  , "src/Waargonaut/Types/CommaSep.hs"
  , "src/Waargonaut/Types/Whitespace.hs"
  , "src/Waargonaut/Types/JNumber.hs"
  , "src/Waargonaut/Types/JChar.hs"
  , "src/Waargonaut/Types/JString.hs"
  , "src/Waargonaut/Types/JArray.hs"
  , "src/Waargonaut/Types/JObject.hs"
  , "src/Waargonaut/Types/Json.hs"
  ]
