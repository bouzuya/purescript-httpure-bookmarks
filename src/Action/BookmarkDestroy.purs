module Action.BookmarkDestroy
  ( execute
  ) where

import Prelude

import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Type (DB)

execute :: DB -> String -> HTTPure.ResponseM
execute db id = do
  deleted <- BookmarkModel.destroy db id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
