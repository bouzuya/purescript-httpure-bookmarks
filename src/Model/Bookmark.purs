module Model.Bookmark
  ( index
  , create
  , show
  , update
  , destroy
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Foreign as Foreign
import Prelude as Prelude
import SQLite3 as SQLite3
import Simple.JSON as SimpleJSON
import Type (DB, Bookmark)

delete :: DB -> String -> Aff Unit
delete db id = do
  conn <- SQLite3.newDB db
  _ <- SQLite3.queryDB conn query (map Foreign.unsafeToForeign [id])
  SQLite3.closeDB conn
  where
    query =
      String.joinWith
        "\n"
        [ "DELETE"
        , "  FROM"
        , "    bookmarks"
        , "  WHERE"
        , "    id = ?"
        ]

findAll :: DB -> Aff (Array Bookmark)
findAll db = do
  conn <- SQLite3.newDB db
  rows <- map SimpleJSON.read (SQLite3.queryDB conn query [])
  _ <- SQLite3.closeDB conn
  Either.either (\e -> Aff.throwError (Aff.error (Prelude.show e))) pure rows
  where
    query =
      String.joinWith
        "\n"
        [ "SELECT"
        , "    id"
        , "  , url"
        , "  , comment"
        , "  FROM"
        , "    bookmarks"
        ]

find :: DB -> String -> Aff (Maybe Bookmark)
find db id = do
  conn <- SQLite3.newDB db
  rows <-
    map
      SimpleJSON.read
      (SQLite3.queryDB conn query (map Foreign.unsafeToForeign [id]))
  _ <- SQLite3.closeDB conn
  Either.either
    (\e -> Aff.throwError (Aff.error (Prelude.show e)))
    (pure <<< Array.head)
    rows
  where
    query =
      String.joinWith
        "\n"
        [ "SELECT"
        , "    id"
        , "  , url"
        , "  , comment"
        , "  FROM"
        , "    bookmarks"
        , "  WHERE"
        , "    id = ?"
        ]

insert :: DB -> Bookmark -> Aff Unit
insert db bookmark = do
  conn <- SQLite3.newDB db
  _ <-
    SQLite3.queryDB
      conn
      query
      (map
        Foreign.unsafeToForeign
        [ bookmark.id
        , bookmark.url
        , Maybe.fromMaybe "" bookmark.comment
        ])
  SQLite3.closeDB conn
  where
    query =
      String.joinWith
        "\n"
        [ "INSERT INTO bookmarks"
        , "  ( id"
        , "  , url"
        , "  , comment"
        , "  )"
        , "  VALUES"
        , "  ( ?"
        , "  , ?"
        , "  , ?"
        , "  )"
        ]

update' :: DB -> String -> Bookmark -> Aff Unit
update' db id bookmark = do
  conn <- SQLite3.newDB db
  _ <-
    SQLite3.queryDB
      conn
      query
      (map
        Foreign.unsafeToForeign
        [ bookmark.url
        , Maybe.fromMaybe "" bookmark.comment
        , bookmark.id
        ])
  SQLite3.closeDB conn
  where
    query =
      String.joinWith
        "\n"
        [ "UPDATE"
        , "  bookmarks"
        , "  SET"
        , "    url = ?"
        , "  , comment = ?"
        , "  WHERE"
        , "    id = ?"
        ]

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
