module Lang1 where

import Prelude
--import Data.Exists
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Map as M
import Control.Monad.State as S

import GenericEncoding
import Types.XUnit
import Types.Free
import Types.Exists
import Types.Serialize

data Phantom s = IntF | StringF

data LangF a s
  = LangA1 (S (Phantom s)) (S s) a

type LangF' s a = LangF a s
data LangWrapper a = LangWrapper (Exists (LangF a))
type Lang a = Free LangWrapper a

--------------------------------------------------------------------------------
derive instance genericPhantom :: G.Generic (Phantom s) _
instance encodePhantom :: Encode a => Encode (Phantom a) where
  encode = defaultEncode
instance showPhantom :: Show a => Show (Phantom a) where
  show = GShow.genericShow
instance phantomSerialize :: (Encode s, Show s) => Serialize (Phantom s) where
  serialize = defaultSerialize
{-
derive instance genericLangF :: G.Generic (LangF a s) _
instance encodeLangF :: (Encode a, Encode s) => Encode (LangF a s) where
  encode = defaultEncode
-}
--------------------------------------------------------------------------------

wrap :: forall a s. LangF a s -> Lang a
wrap = liftF <<< LangWrapper <<< mkExists

langA1Int :: Int -> Lang XUnit
langA1Int i = wrap $ LangA1 (defaultSerialize IntF) (defaultSerialize i) xunit

--------------------------------------------------------------------------------
type St =
  { level :: Int
  , items :: M.Map Int String
  , str :: String
  }
type ISt a = S.State St a

mkTaggedJSON s = "{tag\":\"" <> s <> "\"}"

encodePhantomJSON IntF = mkTaggedJSON "IntF"
encodePhantomJSON StringF = mkTaggedJSON "StringF"

encoder :: forall s. NaturalTransformation (LangF' s) ISt
encoder (LangA1 sPhant sVal next) = do
  st <- S.get
  let s1 = getJSON sPhant
  let s2 = getJSON sVal
  S.put $ { level: st.level + 1, items: st.items, str: st.str <> " " <> s1 <> " " <> s2}
  pure next

runEncode :: forall a. Lang a -> S.State St a
runEncode = foldFree (\(LangWrapper x) -> runExists encoder x)

encodeLang :: Lang XUnit -> String
encodeLang l = let
  emptySt = { level: 0, items: M.empty, str: "" }
  st = S.execState (runEncode l) emptySt
  in st.str
