module Test.Lang1Test where

import Prelude
import Control.Monad.Free
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Common

import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import GenericEncoding

import Types.XUnit
import Serialize
import Lang1

data A = A
data B = B1 Int String XUnit | B2 A
data C a = C a B

derive instance gA :: G.Generic A _
instance encodeA :: Encode A where
  encode = defaultEncode
instance showA :: Show A where
  show = GShow.genericShow

derive instance gB :: G.Generic B _
instance encodeB :: Encode B where
  encode = defaultEncode
instance showB :: Show B where
  show = GShow.genericShow

derive instance gC :: G.Generic (C a) _
instance encodeC :: Encode a => Encode (C a) where
  encode = defaultEncode
instance showC :: Show a => Show (C a) where
  show = GShow.genericShow

v3 = do
  x <- langA3 "cba"
  langA2 x lang2

{-
{ "tag":"LangA1"
, "contents":
  [ {"tag":"IntF"}
  , 1
  ]
}
{ "tag":"LangA2"
, "contents":
  [ 2
  , { "tag":"LangA1"
    , "contents":
      [ {"tag":"StringF"}
      , "abc"
      ]
    }
  ]
}
-}




lang1Test = do
  liftEff $ log $ encodeLang lang3
  pure unit

runTests = do
  describe "Tests " do
    it "Lang1 test" lang1Test
    pending "other tests..."
