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
  updatedMaybe <- BookmarkModel.update db id bookmark
  case updatedMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just updated -> HTTPure.ok (ViewBookmarkShow.render updated)
