module Model
  ( bookmarkIndex
  , bookmarkCreate
  , bookmarkShow
  , bookmarkUpdate
  , bookmarkDestroy
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type (DB, Bookmark)

bookmarkIndex :: DB -> Aff (Array Bookmark)
bookmarkIndex bookmarksRef = liftEffect (Ref.read bookmarksRef)

bookmarkShow :: DB -> String -> Aff (Maybe Bookmark)
bookmarkShow bookmarksRef id = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  pure (Array.find ((eq id) <<< _.id) bookmarks)

bookmarkCreate :: DB -> Bookmark -> Aff (Maybe Bookmark)
bookmarkCreate bookmarksRef bookmark = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.find ((eq bookmark.id) <<< _.id) bookmarks of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <- liftEffect (Ref.write (Array.cons bookmark bookmarks) bookmarksRef)
      pure (Maybe.Just bookmark)

bookmarkUpdate :: DB -> String -> Bookmark -> Aff (Maybe Bookmark)
bookmarkUpdate bookmarksRef id bookmark = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.findIndex ((eq id) <<< _.id) bookmarks of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index ->
      case Array.updateAt index bookmark bookmarks of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just bookmarks' -> do
          _ <- liftEffect (Ref.write bookmarks' bookmarksRef)
          pure (Maybe.Just bookmark)

bookmarkDestroy :: DB -> String -> Aff Boolean
bookmarkDestroy bookmarksRef id = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.findIndex ((eq id) <<< _.id) bookmarks of
    Maybe.Nothing -> pure false
    Maybe.Just index ->
      case Array.deleteAt index bookmarks of
        Maybe.Nothing -> pure false
        Maybe.Just bookmarks' -> do
          _ <- liftEffect (Ref.write bookmarks' bookmarksRef)
          pure true
