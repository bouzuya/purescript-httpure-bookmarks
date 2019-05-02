module Action.BookmarkCreate
  ( execute
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Type (DB, Bookmark)
import View.BookmarkShow as ViewBookmarkShow

execute :: DB -> Bookmark -> HTTPure.ResponseM
execute db bookmark = do
  created <- BookmarkModel.create db bookmark
  if Maybe.isJust created
    then HTTPure.ok (ViewBookmarkShow.render bookmark)
    else HTTPure.badRequest "invalid" -- TODO
