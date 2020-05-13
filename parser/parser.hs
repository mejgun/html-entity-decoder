{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import           Numeric             (showHex, showInt)

fileName :: String
fileName = "entities.json"

resultFileName :: String
resultFileName = "../src/Map.hs"

data Entity = Entity
    { name       :: T.Text
    , codepoints :: [Int]
    , characters :: T.Text
    }
    deriving Show

data Entities = Entities [Entity]
    deriving Show

instance FromJSON Entities where
  parseJSON = parseEntities

type MyMap = HM.HashMap T.Text T.Text

main :: IO ()
main = do
  a <- decodeFileStrict fileName :: IO (Maybe Entities)
  let Entities b = fromJust a
      m = HM.empty :: HM.HashMap T.Text T.Text
      res = T.concat
              [ "--- GENERATED ---\n\n"
              , "{-# LANGUAGE OverloadedStrings #-}\n"
              , "module Map where\n\n"
              , "import qualified Data.Text as T\n"
              , "import qualified Data.HashMap.Strict as HM\n\n"
              , "entities :: HM.HashMap T.Text T.Text\n"
              , "entities = HM."
              , T.pack  (show (foldl foldF m b))
              ]
   in writeFile resultFileName (T.unpack res)

foldF :: MyMap ->Entity ->  MyMap
foldF m e = do
  let m1 = HM.insert (name e) (characters e) m
      hexCodes = map (\c->T.concat["&#x",T.pack(showHex c ""),";"]) (codepoints e)
      codes = map (\c->T.concat["&#",T.pack(showInt c ""),";"]) (codepoints e)
      chars = map (\t->T.singleton (fst t)) $ T.zip (characters e) "1234567890"
      tuples = zip codes chars
      hexTuples = zip hexCodes chars
      m2 = foldl (\mp (k,v) -> HM.insert k v mp) m1 tuples
      m3 = foldl (\mp (k,v) -> HM.insert k v mp) m2 hexTuples
   in m3

parseEntities :: Value -> Parser Entities
parseEntities = withObject "entities" $ \o -> return $ Entities $ map
  (\(ent, codes) ->
    let c  = getRes "codepoints" codes
        ch = getRes "characters" codes
    in  Entity { name = T.toLower ent, codepoints = c, characters = ch }
  )
  (HM.toList o)

getRes :: FromJSON a => T.Text -> Value -> a
getRes t = getJsonResult . fromJSON . getMapValue t . getObject

getObject :: Value -> Object
getObject o = case o of
  Object obj -> obj
  _          -> error "not object"

getMapValue :: T.Text -> Object -> Value
getMapValue v m = case HM.lookup v m of
  Nothing  -> error "no value found"
  Just val -> val

getJsonResult :: Result a -> a
getJsonResult res = case res of
  Success r -> r
  Error   e -> error e
