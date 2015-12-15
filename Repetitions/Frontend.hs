{-# LANGUAGE OverloadedStrings #-}
module Repetitions.Frontend where

import Prelude hiding (div, head, id)
import Text.Blaze.Html5 (Html, (!), docTypeHtml, head, meta, link, title, body, div, p)
import Text.Blaze.Html5.Attributes (charset, class_, href, rel, media)

index :: Html
index = layout "Repetitions" $
  div ! class_ "container" $
    div ! class_ "jumbotron" $
      p "Hello"

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
  head $ do
    title t
    meta ! charset "utf-8"
    link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
  body b
