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
import Data.String (joinWith)
import Control.Monad.State as S

import GenericEncoding
import Types.XUnit
import Types.Free
import Types.Exists
import Serialize

data Phantom s = IntF | StringF

data LangF a s
  = LangA1 (S (Phantom s)) (S s) a
  | LangA2 Int (Lang s) (s -> a)
  | LangA3 String (Int -> a)

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
--------------------------------------------------------------------------------

wrap :: forall a s. LangF a s -> Lang a
wrap = liftF <<< LangWrapper <<< mkExists

langA1Int :: Int -> Lang XUnit
langA1Int i = wrap $ LangA1 (defaultSerialize IntF) (defaultSerialize i) xunit

langA1String :: String -> Lang XUnit
langA1String i = wrap $ LangA1 (defaultSerialize StringF) (defaultSerialize i) xunit

langA2 :: forall s. Int -> Lang s -> Lang s
langA2 i l = wrap $ LangA2 i l id

langA3 :: String -> Lang Int
langA3 s = wrap $ LangA3 s id
--------------------------------------------------------------------------------
type St =
  { level :: Int
  , items :: M.Map Int String
  , str :: String
  }
type ISt a = S.State St a

-- This way leads to contradiction in serialization.

encoder :: forall s. Boolean -> NaturalTransformation (LangF' s) ISt
encoder idle (LangA1 sPhant sVal next) = do
  st <- S.get
  let s1 = getJSON sPhant
  let s2 = getJSON sVal
  let s3 = mkJSON1 "LangA1" [s1, s2]
  when (not idle) $ S.put { level: st.level + 1, items: st.items, str: st.str <> " " <> s3 }
  pure next
encoder idle (LangA2 i l next) = do
  st <- S.get
  let langS = encodeLang l
  let s1 = encodeJSON i
  let s2 = mkJSON1 "LangA2" [s1, langS]
  when (not idle) $ S.put { level: st.level + 1, items: st.items, str: st.str <> " " <> s2 }
  e <- runEncode true l
  pure $ next e
encoder idle (LangA3 s next) = do
  st <- S.get
  let s1 = mkJSON1 "LangA3" [encodeJSON s]
  when (not idle) $ S.put { level: st.level + 1, items: st.items, str: st.str <> " " <> s1 }
  e <- runEncode false next
  pure $ next e

runEncode :: forall a. Boolean -> Lang a -> S.State St a
runEncode idle = foldFree (\(LangWrapper x) -> runExists (encoder idle) x)

encodeLang :: forall a. Lang a -> String
encodeLang l = let
  emptySt = { level: 0, items: M.empty, str: "" }
  st = S.execState (runEncode false false l) emptySt
  in st.str
