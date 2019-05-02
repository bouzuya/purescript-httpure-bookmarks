module Test.Router
  ( tests
  ) where

import Prelude

import Action as Action
import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Foreign.Object as Object
import HTTPure (Request)
import HTTPure as HTTPure
import Partial.Unsafe as Unsafe
import Router as Router
import Simple.JSON as SimpleJSON
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Router" do
  let
    request :: String -> String -> Maybe String -> Request
    request s p b =
      let
        body = Maybe.fromMaybe mempty b
        headers = HTTPure.empty
        method = case s of
          "GET" -> HTTPure.Get
          "POST" -> HTTPure.Post
          "PATCH" -> HTTPure.Patch
          "DELETE" -> HTTPure.Delete
          _ -> Unsafe.unsafeCrashWith ("unknown method: " <> s)
        path = Array.drop 1 (String.split (String.Pattern "/") p)
        query = Object.empty
      in
        { body, headers, method, path, query }

  TestUnit.test "GET /bookmarks" do
    Assert.equal
      (Either.Right Action.BookmarkIndex)
      (Router.router (request "GET" "/bookmarks" Maybe.Nothing))

  TestUnit.test "POST /bookmarks" do
    let
      bookmark1 = { id: "1", url: "http://example.com", comment: pure "foo" }
      body1 = SimpleJSON.writeJSON bookmark1
    Assert.equal
      (Either.Right (Action.BookmarkCreate bookmark1))
      (Router.router (request "POST" "/bookmarks" (Maybe.Just body1)))

  TestUnit.test "GET /bookmarks/{id}" do
    Assert.equal
      (Either.Right (Action.BookmarkShow "abc"))
      (Router.router (request "GET" "/bookmarks/abc" Maybe.Nothing))

  TestUnit.test "PATCH /bookmarks/{id}" do
    let
      bookmark1 = { id: "abc", url: "http://example.com", comment: pure "foo" }
      body1 = SimpleJSON.writeJSON bookmark1
    Assert.equal
      (Either.Right (Action.BookmarkUpdate "abc" bookmark1))
      (Router.router (request "PATCH" "/bookmarks/abc" (Maybe.Just body1)))

  TestUnit.test "DELETE /bookmarks/{id}" do
    Assert.equal
      (Either.Right (Action.BookmarkDestroy "abc"))
      (Router.router (request "DELETE" "/bookmarks/abc" Maybe.Nothing))
