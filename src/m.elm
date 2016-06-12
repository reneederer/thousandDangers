import Json.Encode exposing (..)
import Html exposing (..)


main = 
        let x = Json.Encode.object [ ("id", Json.Encode.int 23), ("title", Json.Encode.string ("22")) ]
        in
            div [] [text <| toString x]
