module Router
  ( router
  ) where

import Action.Bookmark as BookmarkAction
import HTTPure as HTTPure
import Type (DB)

router :: DB -> HTTPure.Request -> HTTPure.ResponseM
router db = case _ of
  { method: HTTPure.Get, path: ["bookmarks"] } ->
    BookmarkAction.index db
  { method: HTTPure.Post, path: ["bookmarks"], body } ->
    BookmarkAction.create db body
  { method: HTTPure.Get, path: ["bookmarks", id] } ->
    BookmarkAction.show db id
  { method: HTTPure.Patch, path: ["bookmarks", id], body } ->
    BookmarkAction.update db id body
  { method: HTTPure.Delete, path: ["bookmarks", id] } ->
    BookmarkAction.destroy db id
  _ -> HTTPure.notFound
