module Action.BookmarkUpdate
  ( execute
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Type (DB, Bookmark)
import View.BookmarkShow as ViewBookmarkShow

execute :: DB -> String -> Bookmark -> HTTPure.ResponseM
execute db id bookmark = do
  updated <- BookmarkModel.update db id bookmark
  if Maybe.isJust updated
    then HTTPure.ok (ViewBookmarkShow.render bookmark)
    else HTTPure.notFound
