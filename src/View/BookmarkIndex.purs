module View.BookmarkIndex
  ( render
  ) where

import Simple.JSON as SimpleJSON
import Type (Bookmark)

render :: Array Bookmark -> String
render = SimpleJSON.writeJSON
