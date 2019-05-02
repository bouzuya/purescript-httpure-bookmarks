module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Effect.Class as Class
import Foreign as Foreign
import Node.FS.Sync as FS
import Partial.Unsafe as Unsafe
import SQLite3 as SQLite3
import Simple.JSON as SimpleJSON
import Test.Router as Router
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Test.Unit.Main as TestUnitMain

type Bookmark =
  { id :: String
  , url :: String
  , comment :: Maybe String
  }

createTable :: String
createTable =
  String.joinWith
    "\n"
    [ "CREATE TABLE IF NOT EXISTS bookmarks"
    , "  ( id TEXT PRIMARY KEY"
    , "  , url TEXT NOT NULL UNIQUE"
    , "  , comment TEXT"
    , "  );"
    ]

insertRow :: String
insertRow =
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

selectRow :: String
selectRow =
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

main :: Effect Unit
main = TestUnitMain.runTest do
  Router.tests
  TestUnit.suite "SQLite3" do
    TestUnit.test "example" do
      let dbFile = "./example.sqlite"
      exists <- Class.liftEffect (FS.exists dbFile)
      when exists (Class.liftEffect (FS.unlink dbFile))
      conn <- SQLite3.newDB dbFile
      _ <- SQLite3.queryDB conn createTable []
      _ <-
        SQLite3.queryDB
          conn
          insertRow
          (map Foreign.unsafeToForeign ["1", "http://bouzuya.net", "my page"])
      _ <-
        SQLite3.queryDB
          conn
          insertRow
          (map Foreign.unsafeToForeign ["2", "http://blog.bouzuya.net", "blog"])
      results <-
        map
          SimpleJSON.read
          (SQLite3.queryDB conn selectRow (map Foreign.unsafeToForeign ["1"]))
      case results of
        Either.Left e -> TestUnit.failure (show e)
        Either.Right (bs :: Array Bookmark) -> do
          Assert.equal 1 (Array.length bs)
          case Array.head bs of
            Maybe.Nothing -> Unsafe.unsafeCrashWith "head"
            Maybe.Just b -> do
              Assert.equal "1" b.id
              Assert.equal "http://bouzuya.net" b.url
              Assert.equal (Maybe.Just "my page") b.comment
      SQLite3.closeDB conn
