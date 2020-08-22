module Person
  ( Address(..)
  , Person(..)
  )
where

import           FromJValue                     ( FromJValue
                                                , fromJValue
                                                )
import           JValue                         ( JValue(..) )
import           Utils                          ( mandatoryProp
                                                , optionalProp
                                                )

data Address = Address
    { houseNumber :: Int
    , streetName  :: String
    , town        :: String
    , county      :: Maybe String
    , country     :: String
    }
    deriving Show

instance FromJValue Address where
  fromJValue val = do
    houseNumber <- mandatoryProp val "houseNumber"
    streetName  <- mandatoryProp val "streetName"
    town        <- mandatoryProp val "town"
    country     <- mandatoryProp val "country"
    county      <- optionalProp val "county"
    pure Address { .. }

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
  fromJValue val = do
    firstName <- mandatoryProp val "firstName"
    lastName  <- mandatoryProp val "lastName"
    jobTitle  <- mandatoryProp val "jobTitle"
    address   <- mandatoryProp val "address"
    hobbies   <- mandatoryProp val "hobbies"
    topSpeed  <- mandatoryProp val "topSpeed"
    pure Person { .. }
