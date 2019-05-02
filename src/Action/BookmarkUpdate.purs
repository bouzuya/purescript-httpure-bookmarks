module Action.BookmarkUpdate
  ( execute
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Simple.JSON as SimpleJSON
import Type (DB, Bookmark)
import View.BookmarkIndex as ViewBookmarkIndex
import View.BookmarkShow as ViewBookmarkShow

execute :: DB -> String -> String -> HTTPure.ResponseM
execute db id body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      updated <- BookmarkModel.update db id bookmark
      if Maybe.isJust updated
        then HTTPure.ok (ViewBookmarkShow.render bookmark)
        else HTTPure.notFound
