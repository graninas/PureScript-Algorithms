module Lang1 where

import Prelude
import Data.Exists
import Control.Monad.Free
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Function.Uncurried (Fn2, runFn2)

import Types.Generic
import Types.XUnit

data Phantom s = IntF | StringF

data LangF a s
  = LangA1 (Phantom s) s a

type LangF' s a = LangF a s
data LangWrapper a = LangWrapper (Exists (LangF a))
type Lang a = Free LangWrapper a

wrap :: forall a s. LangF a s -> Lang a
wrap = liftF <<< LangWrapper <<< mkExists

langA1Int :: Int -> Lang XUnit
langA1Int i = wrap $ LangA1 IntF i xunit


------------------------------------------------------------------------------
derive instance genericPhantom :: G.Generic (Phantom s) _
derive instance genericLangF :: G.Generic (LangF a s) _

instance encodePhantom :: Encode a => Encode (Phantom a) where
  encode = defaultEncode
instance encodeLangF :: (Encode a, Encode s) => Encode (LangF a s) where
  encode = defaultEncode
