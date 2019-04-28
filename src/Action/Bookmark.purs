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
import Model as Model
import Simple.JSON as SimpleJSON
import Type (DB, Bookmark)

index :: DB -> HTTPure.ResponseM
index db = do
  bookmarks <- Model.bookmarkIndex db
  HTTPure.ok (SimpleJSON.writeJSON bookmarks)

create :: DB -> String -> HTTPure.ResponseM
create db body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      created <- Model.bookmarkCreate db bookmark
      if Maybe.isJust created
        then HTTPure.ok (SimpleJSON.writeJSON bookmark)
        else HTTPure.badRequest body

show :: DB -> String -> HTTPure.ResponseM
show db id = do
  bookmarkMaybe <- Model.bookmarkShow db id
  case bookmarkMaybe of
    Maybe.Nothing -> HTTPure.notFound
    Maybe.Just bookmark -> HTTPure.ok (SimpleJSON.writeJSON bookmark)

update :: DB -> String -> String -> HTTPure.ResponseM
update db id body = do
  case SimpleJSON.readJSON_ body :: _ Bookmark of
    Maybe.Nothing -> HTTPure.badRequest body
    Maybe.Just bookmark -> do
      updated <- Model.bookmarkUpdate db id bookmark
      if Maybe.isJust updated
        then HTTPure.ok (SimpleJSON.writeJSON bookmark)
        else HTTPure.notFound

destroy :: DB -> String -> HTTPure.ResponseM
destroy db id = do
  deleted <- Model.bookmarkDestroy db id
  if deleted
    then HTTPure.noContent
    else HTTPure.notFound
