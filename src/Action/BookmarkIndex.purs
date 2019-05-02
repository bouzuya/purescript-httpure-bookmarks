module Action.BookmarkIndex
  ( execute
  ) where

import Prelude

import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Type (DB)
import View.BookmarkIndex as ViewBookmarkIndex

execute :: DB -> HTTPure.ResponseM
execute db = do
  bookmarks <- BookmarkModel.index db
  HTTPure.ok (ViewBookmarkIndex.render bookmarks)
