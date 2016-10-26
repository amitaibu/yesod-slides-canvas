{-# LANGUAGE DeriveGeneric #-}

module Model.Types where

import Prelude
import Yesod
import GHC.Generics

data MediaType = MediaImage | MediaYouTube
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

derivePersistField "MediaType"

instance ToJSON MediaType
instance FromJSON MediaType
