module Router
  ( router
  ) where

import Action.BookmarkCreate as ActionBookmarkCreate
import Action.BookmarkDestroy as ActionBookmarkDestroy
import Action.BookmarkIndex as ActionBookmarkIndex
import Action.BookmarkShow as ActionBookmarkShow
import Action.BookmarkUpdate as ActionBookmarkUpdate
import HTTPure as HTTPure
import Type (DB)

router :: DB -> HTTPure.Request -> HTTPure.ResponseM
router db = case _ of
  { method: HTTPure.Get, path: ["bookmarks"] } ->
    ActionBookmarkIndex.execute db
  { method: HTTPure.Post, path: ["bookmarks"], body } ->
    ActionBookmarkCreate.execute db body
  { method: HTTPure.Get, path: ["bookmarks", id] } ->
    ActionBookmarkShow.execute db id
  { method: HTTPure.Patch, path: ["bookmarks", id], body } ->
    ActionBookmarkUpdate.execute db id body
  { method: HTTPure.Delete, path: ["bookmarks", id] } ->
    ActionBookmarkDestroy.execute db id
  _ -> HTTPure.notFound
