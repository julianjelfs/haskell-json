module Person
  ( Address(..)
  , Person(..)
  )
where

import           FromJValue (FromJValue, fromJValue)
import           JValue     (JValue (..))
import           Utils      (mandatoryProp, optionalProp)

data Address = Address
    { houseNumber :: Int
    , streetName  :: String
    , town        :: String
    , county      :: Maybe String
    , country     :: String
    }
    deriving Show

instance FromJValue Address where
  fromJValue val =
    Address
        <$> mandatoryProp val "houseNumber"
        <*> mandatoryProp val "streetName"
        <*> mandatoryProp val "town"
        <*> optionalProp val "county"
        <*> mandatoryProp val "country"

data Person = Person
    { firstName :: String
    , lastName  :: String
    , jobTitle  :: String
    , address   :: Address
    , hobbies   :: [String]
    , topSpeed  :: Int
    }
    deriving Show

instance FromJValue Person where
  fromJValue val =
    Person
      <$> mandatoryProp val "firstName"
      <*> mandatoryProp val "lastName"
      <*> mandatoryProp val "jobTitle"
      <*> mandatoryProp val "address"
      <*> mandatoryProp val "hobbies"
      <*> mandatoryProp val "topSpeed"
