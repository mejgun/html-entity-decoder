{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T

fileName :: String
fileName = "entities.json"

data Entity = Entity
    { name       :: T.Text
    , codepoints :: [Int]
    , characters :: T.Text
    }
    deriving (Show)

data Entities = Entities [Entity]
    deriving Show

instance FromJSON Entities where
  parseJSON = parseEntities

main :: IO ()
main = do
  b <- decodeFileStrict fileName :: IO (Maybe Entities)
  print b

parseEntities :: Value -> Parser Entities
parseEntities = withObject "entities" $ \o ->
  return
    $ Entities
    $ (map
        (\(ent, codes) -> do
          let c  = getRes "codepoints" codes
              ch = getRes "characters" codes
          Entity { name = ent, codepoints = c, characters = ch }
        )
        (HM.toList o)
      )

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
