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
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type (DB, Bookmark)

index :: DB -> Aff (Array Bookmark)
index bookmarksRef = liftEffect (Ref.read bookmarksRef)

show :: DB -> String -> Aff (Maybe Bookmark)
show bookmarksRef id = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  pure (Array.find ((eq id) <<< _.id) bookmarks)

create :: DB -> Bookmark -> Aff (Maybe Bookmark)
create bookmarksRef bookmark = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.find ((eq bookmark.id) <<< _.id) bookmarks of
    Maybe.Just _ -> pure Maybe.Nothing
    Maybe.Nothing -> do
      _ <- liftEffect (Ref.write (Array.cons bookmark bookmarks) bookmarksRef)
      pure (Maybe.Just bookmark)

update :: DB -> String -> Bookmark -> Aff (Maybe Bookmark)
update bookmarksRef id bookmark = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.findIndex ((eq id) <<< _.id) bookmarks of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index' ->
      case Array.updateAt index' bookmark bookmarks of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just bookmarks' -> do
          _ <- liftEffect (Ref.write bookmarks' bookmarksRef)
          pure (Maybe.Just bookmark)

destroy :: DB -> String -> Aff Boolean
destroy bookmarksRef id = do
  bookmarks <- liftEffect (Ref.read bookmarksRef)
  case Array.findIndex ((eq id) <<< _.id) bookmarks of
    Maybe.Nothing -> pure false
    Maybe.Just index' ->
      case Array.deleteAt index' bookmarks of
        Maybe.Nothing -> pure false
        Maybe.Just bookmarks' -> do
          _ <- liftEffect (Ref.write bookmarks' bookmarksRef)
          pure true
