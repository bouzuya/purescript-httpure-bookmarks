module Router
  ( RouteError(..)
  , router
  ) where

import Prelude

import Action (Action(..))
import Data.Either (Either)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import HTTPure as HTTPure
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON

data RouteError
  = ClientError String
  | NotFound

derive instance eqRouteError :: Eq RouteError
derive instance genericRouteError :: Generic RouteError _
instance showRouteError :: Show RouteError where
  show = genericShow

fromJSON :: forall a. ReadForeign a => String -> Either RouteError a
fromJSON s =
  Either.either
    (Either.Left <<< ClientError <<< show)
    Either.Right
    (SimpleJSON.readJSON s)

router :: HTTPure.Request -> Either RouteError Action
router = case _ of
  { method: HTTPure.Get, path: ["bookmarks"] } -> pure BookmarkIndex
  { method: HTTPure.Post, path: ["bookmarks"], body } -> do
    user <- fromJSON body
    pure (BookmarkCreate user)
  { method: HTTPure.Get, path: ["bookmarks", id] } -> pure (BookmarkShow id)
  { method: HTTPure.Patch, path: ["bookmarks", id], body } -> do
    user <- fromJSON body
    pure (BookmarkUpdate id user)
  { method: HTTPure.Delete, path: ["bookmarks", id] } ->
    pure (BookmarkDestroy id)
  _ -> Either.Left NotFound
