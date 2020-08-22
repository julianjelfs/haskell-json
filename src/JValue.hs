module JValue (JValue(..)) where

import           Data.List (intercalate)
import           Data.Map  as M

data JValue
    = JString String
    | JBool Bool
    | JNum Double
    | JNull
    | JArray [JValue]
    | JObject (M.Map String JValue)

instance Show JValue where
    show = showJValue

showJValue (JString s)   = show s
showJValue (JBool True)  = show "true"
showJValue (JBool False) = show "false"
showJValue (JNum n)      = show n
showJValue JNull         = show "null"
showJValue (JArray a)    = "[" <> intercalate ", " (show <$> a) <> "]"
showJValue (JObject o)   = "{" <> intercalate ", " (showKeyVal <$> M.toList o) <> "}"
    where showKeyVal (k, v) = k <> " : " <> show v
