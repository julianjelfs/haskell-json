module FromJValue where

import           JValue (JValue (..))

class FromJValue a where
    fromJValue :: JValue -> Either String a

instance FromJValue String where
    fromJValue (JString s) = Right s
    fromJValue v        = Left $ "The provided JValue (" <> show v <> ") is not a string"

instance FromJValue Int where
    fromJValue (JNum n) = Right n
    fromJValue v        = Left $ "The provided JValue (" <> show v <> ") is not a number"

instance FromJValue Bool where
    fromJValue (JBool b) = Right b
    fromJValue v        = Left $ "The provided JValue (" <> show v <> ") is not a boolean"

instance FromJValue a => FromJValue [a] where
    fromJValue (JArray a) = traverse fromJValue a
    fromJValue _          = Left "The provided JValue is not a list"
