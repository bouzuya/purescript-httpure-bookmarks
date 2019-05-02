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
  createdMaybe <- BookmarkModel.create db bookmark
  case createdMaybe of
    Maybe.Nothing -> HTTPure.badRequest "invalid" -- TODO
    Maybe.Just created -> HTTPure.ok (ViewBookmarkShow.render created)
