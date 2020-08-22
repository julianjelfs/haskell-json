module Utils
  ( mandatoryProp
  , optionalProp
  , decode
  )
where

import           Data.Map   as M
import           FromJValue (FromJValue, fromJValue)
import           JValue     (JValue (..))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

mandatoryProp :: FromJValue a => JValue -> String -> Either String a
mandatoryProp (JObject o) name =
  maybeToEither ("Missing prop: " <> name)  (M.lookup name o) >>= fromJValue
mandatoryProp _ _ = Left "Can only extract props from JObjects"

optionalProp :: FromJValue a => JValue -> String -> Either String (Maybe a)
optionalProp (JObject o) name = traverse fromJValue (M.lookup name o)
optionalProp _           _    = Left "Can only extract props from JObjects"

decode :: (FromJValue a, Show e) => Either e JValue -> Either String a
decode res = case res of
  Left  err  -> Left (show err)
  Right json -> fromJValue json
