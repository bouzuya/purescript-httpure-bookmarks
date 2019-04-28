module Type
  ( DB
  , Bookmark
  ) where

import Data.Maybe (Maybe)

type DB = String

type Bookmark =
  { id :: String
  , url :: String
  , comment :: Maybe String
  }
