module Action.Bookmark
  ( index
  , create
  , show
  , update
  , destroy
  ) where

import Prelude

import Data.Maybe as Maybe
import HTTPure as HTTPure
import Model.Bookmark as BookmarkModel
import Simple.JSON as SimpleJSON
import Type (DB, Bookmark)

index :: DB -> HTTPure.ResponseM
index db = do
  bookmarks <- BookmarkModel.index db
  HTTPure.ok (SimpleJSON.writeJSON bookmarks)

create :: DB -> String -> HTTPure.ResponseM
create db body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      created <- BookmarkModel.create db bookmark
      if Maybe.isJust created
        then HTTPure.ok (SimpleJSON.writeJSON bookmark)
        else HTTPure.badRequest body

show :: DB -> String -> HTTPure.ResponseM
show db id = do
  bookmarkMaybe <- BookmarkModel.show db id
  case bookmarkMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just bookmark -> HTTPure.ok (SimpleJSON.writeJSON bookmark)

update :: DB -> String -> String -> HTTPure.ResponseM
update db id body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      updated <- BookmarkModel.update db id bookmark
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON bookmark)
        else HTTPure.notFound

destroy :: DB -> String -> HTTPure.ResponseM
destroy db id = do
  deleted <- BookmarkModel.destroy db id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
