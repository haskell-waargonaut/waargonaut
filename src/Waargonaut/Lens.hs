{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Waargonaut.Lens where

import           Control.Lens (makeClassy, makeClassyPrisms, makeWrapped)
import           Waargonaut   (JAssoc (..), JObject (..), Json (..), Jsons (..))

makeClassy       ''JObject
makeWrapped      ''JObject

makeClassy       ''Json
makeClassyPrisms ''Json

makeClassy       ''JAssoc

makeClassy       ''Jsons
makeWrapped      ''Jsons
