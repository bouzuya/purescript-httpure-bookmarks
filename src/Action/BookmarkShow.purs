module Action.BookmarkShow
  ( execute
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Type (DB)
import View.BookmarkShow as ViewBookmarkShow

execute :: DB -> String -> HTTPure.ResponseM
execute db id = do
  bookmarkMaybe <- BookmarkModel.show db id
  case bookmarkMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just bookmark -> HTTPure.ok (ViewBookmarkShow.render bookmark)
