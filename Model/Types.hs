module Model.Types where

import Prelude
import Yesod

data MediaType = MediaImage | MediaYouTube
    deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "MediaType"    
