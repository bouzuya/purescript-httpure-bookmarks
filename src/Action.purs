module Action
  ( Action(..)
  , BookmarkId
  , execute
  ) where

import Prelude

import Action.BookmarkCreate as ActionBookmarkCreate
import Action.BookmarkDestroy as ActionBookmarkDestroy
import Action.BookmarkIndex as ActionBookmarkIndex
import Action.BookmarkShow as ActionBookmarkShow
import Action.BookmarkUpdate as ActionBookmarkUpdate
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import HTTPure (ResponseM)
import Type (DB, Bookmark)

type BookmarkId = String

data Action
  = BookmarkIndex
  | BookmarkCreate Bookmark
  | BookmarkShow BookmarkId
  | BookmarkUpdate BookmarkId Bookmark
  | BookmarkDestroy BookmarkId

derive instance eqAction :: Eq Action
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow

execute :: DB -> Action -> ResponseM
execute db =
  case _ of
    BookmarkIndex -> ActionBookmarkIndex.execute db
    BookmarkCreate bookmark -> ActionBookmarkCreate.execute db bookmark
    BookmarkShow id -> ActionBookmarkShow.execute db id
    BookmarkUpdate id bookmark -> ActionBookmarkUpdate.execute db id bookmark
    BookmarkDestroy id -> ActionBookmarkDestroy.execute db id
