import Json.encode exposing (..)


main = toHtml <|
        object [ ("id", Json.Encode.int a.id)
           , ("title", Json.Encode.string ("22")) ]
