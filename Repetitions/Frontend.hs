{-# LANGUAGE OverloadedStrings #-}
module Repetitions.Frontend where

import Prelude hiding (div, head, id, span, lines)
import Data.Text (lines)
import Data.Text.Lazy (toStrict)
import Data.List (intersperse)
import Text.Blaze.Html (toHtml, preEscapedToHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
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
result = layout "Repetitions" .
  mapM_ (p . preEscapedToHtml) .
  lines . toStrict . renderHtml .
  mconcat . intersperse " " . map colorize
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
