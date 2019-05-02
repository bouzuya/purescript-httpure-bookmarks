module View.BookmarkShow
  ( render
  ) where

import Simple.JSON as SimpleJSON
import Type (Bookmark)

render :: Bookmark -> String
render = SimpleJSON.writeJSON
