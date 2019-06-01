module Model.Bookmark
  ( index
  , create
  , show
  , update
  , destroy
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Prim.Boolean (False, True)
import QueryDsl (Column, Table)
import QueryDsl as QueryDsl
import QueryDsl.Expressions ((:==))
import QueryDsl.SQLite3 as QueryDslSQLite3
import SQLite3 as SQLite3
import Type (DB, Bookmark)

bookmarks =
  QueryDsl.makeTable "bookmarks" :: Table
  ( id :: Column String True
  , url :: Column String True
  , comment :: Column (Maybe String) False
  )

delete :: DB -> String -> Aff Unit
delete db id = do
  let
    b = QueryDsl.columns bookmarks
    query = QueryDsl.deleteFrom bookmarks (b.id :== id)
  conn <- SQLite3.newDB db
  _ <- QueryDslSQLite3.runQuery conn query
  _ <- SQLite3.closeDB conn
  pure unit

findAll :: DB -> Aff (Array Bookmark)
findAll db = do
  let
    query = do
      b <- QueryDsl.from bookmarks
      pure (QueryDsl.select { id: b.id, url: b.url, comment: b.comment })
  conn <- SQLite3.newDB db
  rows <- QueryDslSQLite3.runSelectManyQuery conn query
  _ <- SQLite3.closeDB conn
  pure rows

find :: DB -> String -> Aff (Maybe Bookmark)
find db id = do
  let
    query = do
      b <- QueryDsl.from bookmarks
      q <- pure (QueryDsl.select { id: b.id, url: b.url, comment: b.comment })
      pure (QueryDsl.where_ q (b.id :== id))
  conn <- SQLite3.newDB db
  rows <- QueryDslSQLite3.runSelectManyQuery conn query
  _ <- SQLite3.closeDB conn
  pure (Array.head rows)

insert :: DB -> Bookmark -> Aff Unit
insert db bookmark = do
  let query = QueryDsl.insertInto bookmarks bookmark
  conn <- SQLite3.newDB db
  _ <- QueryDslSQLite3.runQuery conn query
  _ <- SQLite3.closeDB conn
  pure unit

update' :: DB -> String -> Bookmark -> Aff Unit
update' db _ bookmark = do
  -- assert id == bookmark.id
  let
    c = QueryDsl.columns bookmarks
    query = QueryDsl.update bookmarks bookmark (c.id :== bookmark.id)
  conn <- SQLite3.newDB db
  _ <- QueryDslSQLite3.runQuery conn query
  _ <- SQLite3.closeDB conn
  pure unit

--

index :: DB -> Aff (Array Bookmark)
index db = findAll db

show :: DB -> String -> Aff (Maybe Bookmark)
show db id = find db id

create :: DB -> Bookmark -> Aff (Maybe Bookmark)
create db bookmark = do
  bookmarkMaybe <- find db bookmark.id
  case bookmarkMaybe of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <- insert db bookmark
      pure (Maybe.Just bookmark) -- TODO

update :: DB -> String -> Bookmark -> Aff (Maybe Bookmark)
update db id bookmark = do
  _ <- update' db id bookmark
  pure (Maybe.Just bookmark) -- TODO

destroy :: DB -> String -> Aff Boolean
destroy db id = do
  _ <- delete db id
  pure true -- TODO
