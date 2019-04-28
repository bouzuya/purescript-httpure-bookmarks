module Type
  ( DB
  , Bookmark
  ) where

import Effect.Ref (Ref)

type DB = Ref (Array Bookmark)

type Bookmark =
  { id :: String
  , url :: String
  , comment :: String
  }
