module Action.BookmarkCreate
  ( execute
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Simple.JSON as SimpleJSON
import Type (DB, Bookmark)
import View.BookmarkShow as ViewBookmarkShow

execute :: DB -> String -> HTTPure.ResponseM
execute db body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      created <- BookmarkModel.create db bookmark
      if Maybe.isJust created
        then HTTPure.ok (ViewBookmarkShow.render bookmark)
        else HTTPure.badRequest body
