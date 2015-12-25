module Repetitions.Frontend (index, result) where

import Prelude hiding (div, head, id, span, lines)
import Data.Text.Lazy (Text)
import Data.List (intersperse)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!), div, form, label, textarea, button, span, br)
import Text.Blaze.Html5.Attributes (class_, href, id, name)
import qualified Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

import Repetitions.Core (AnnotatedWord(..))

index :: Text
index = renderHtml index'

result :: [AnnotatedWord] -> Text
result = renderHtml . result'

index' :: Html
index' = layout $
  form ! A.action "/result" ! A.method "post" $ do
    div ! class_ "form-group" $ do
      label ! A.for "text" $ "Текст"
      textarea ! name "text" ! class_ "form-control" ! A.rows "10" ! id "text" $ ""
    button ! A.type_ "submit" ! class_ "btn btn-default" $ "Проверить"

result' :: [AnnotatedWord] -> Html
result' = layout .
  mconcat . intersperse " " . map colorize
    where
      colorize OkLF = br
      colorize (Ok x) = toHtml x
      colorize (Bad x) = span ! class_ "bad" $ toHtml x

-- collapseLineFeeds :: [AnnotatedWord] -> [AnnotatedWord]
-- collapseLineFeeds = map head . groupBy (\a b -> a == b && b == OkLF)

layout :: Html -> Html
layout content = H.docTypeHtml $ do
  H.head $ do
    H.title "Repetitions"
    H.meta ! A.charset "utf-8"
    H.link ! href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" ! A.rel "stylesheet" ! A.media "screen"
    H.style ".bad {color: #d9534f;} .suspicious {color: rgb(246,184, 85);} .container {padding: 20px;}"
  H.body $
    div ! class_ "container" $
      div ! class_ "row" $
        content
