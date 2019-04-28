module Main
  ( main
  ) where

import Prelude

import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable as Traversable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import Foreign as Foreign
import HTTPure as HTTPure
import Router as Router
import SQLite3 as SQLite3
import Type (Bookmark)

initialBookmarks :: Array Bookmark
initialBookmarks =
  [ { id: "1", url: "https://duckduckgo.com/", comment: pure "search engine" }
  , { id: "2", url: "https://bouzuya.net/", comment: pure "my page" }
  , { id: "3", url: "https://blog.bouzuya.net/", comment: pure "my blog" }
  ]

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- createDB db
  _ <- Class.liftEffect (HTTPure.serve port (Router.router db) booted)
  pure unit
  where
    createDB :: String -> Aff Unit
    createDB dbFile = do
      conn <- SQLite3.newDB dbFile
      _ <- SQLite3.queryDB conn createTableQuery []
      Traversable.for_ initialBookmarks \{ id, url, comment } -> do
        SQLite3.queryDB
          conn
          createSeed
          (map
            Foreign.unsafeToForeign
            [ id, url, Maybe.fromMaybe "" comment ])
      SQLite3.closeDB conn

    createSeed :: String
    createSeed =
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

    createTableQuery :: String
    createTableQuery =
      String.joinWith
        "\n"
        [ "CREATE TABLE IF NOT EXISTS bookmarks"
        , "  ( id TEXT PRIMARY KEY"
        , "  , url TEXT NOT NULL UNIQUE"
        , "  , comment TEXT"
        , "  )"
        ]

    db :: String
    db = "./main.sqlite"

    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
