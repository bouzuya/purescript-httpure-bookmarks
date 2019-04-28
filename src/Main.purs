module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import HTTPure as HTTPure
import Router as Router
import Type (Bookmark)

initialBookmarks :: Array Bookmark
initialBookmarks =
  [ { id: "1", url: "https://duckduckgo.com/", comment: "search engine" }
  , { id: "2", url: "https://bouzuya.net/", comment: "my page" }
  , { id: "3", url: "https://blog.bouzuya.net/", comment: "my blog" }
  ]

main :: HTTPure.ServerM
main = do
  db <- Ref.new initialBookmarks
  HTTPure.serve port (Router.router db) booted
  where
    booted :: Effect Unit
    booted = Console.log "Server now up on port 8080"

    port :: Int
    port = 8080
