{-# LANGUAGE OverloadedStrings #-}
module Repetitions.Frontend where

import Data.List (intersperse)
import Prelude hiding (div, head, id, span)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 (Html, (!), docTypeHtml, head, meta, link, title,
                         body, div, form, label, textarea, button, p, span)
import Text.Blaze.Html5.Attributes (charset, class_, href, rel, media, for, rows, id,
                                    style, type_, action, method, name)

import Repetitions.Core (AnnotatedWord(..))

index :: Html
index = layout "Repetitions" $
  form ! action "/process" ! method "post" $ do
    div ! class_ "form-group" $ do
      label ! for "text" $ "Текст"
      textarea ! name "text" ! class_ "form-control" ! rows "10" ! id "text" $ ""
    button ! type_ "submit" ! class_ "btn btn-default" $ "Проверить"

result :: [AnnotatedWord] -> Html
result xs = layout "Repetitions" $
  p ! style "font-size: 13pt;" $ mconcat . intersperse " " . map colorize $ xs
    where
      colorize (Ok x) = toHtml x
      colorize (Bad x) = span ! style "color: #d9534f;" $ toHtml x

layout :: Html -> Html -> Html
layout t c = docTypeHtml $ do
  head $ do
    title t
    meta ! charset "utf-8"
    link ! href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" ! rel "stylesheet" ! media "screen"
  body $
    div ! class_ "container" $
      div ! class_ "row" ! style "padding: 20px;" $ c
