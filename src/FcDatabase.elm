module FcDatabase exposing (..)

import Json.Encode exposing (..)
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task exposing (..)
import Http

import FcTypes exposing (..)
import FcElement exposing (..)


loadElements : Task Http.Error (List FcShape, List FcArrow)
loadElements =
    Http.post
        decodeElements
        "http://localhost/elm/thousandDangers/src/db.php"
        (Http.multipart
            [Http.stringData "action" "load"
            ])


decodeElements : Json.Decoder (List FcShape, List FcArrow)
decodeElements =
    let toFcShape =
        Json.object6 (\id  x y shapeType title text -> {id=id, x=x, y=y, shapeType=shapeType, title=title, text=text})
            ("id" := Json.int)
            ("x" := Json.float)
            ("y" := Json.float)
            ("shapeType" := (Json.string |> (Json.object1 readShapeType)))
            ("title" := Json.oneOf [Json.string, Json.object1 (\x -> toString x) Json.float ] )
            ("text" := Json.oneOf [Json.string, Json.object1 (\x -> toString x) Json.float ] )
        toFcArrow =
            Json.object8 (\id source_id destination_id source_offset_x source_offset_y destination_offset_x destination_offset_y title ->
                { id=id
                , startPos=
                    case source_id of
                        Nothing -> Global (source_offset_x, source_offset_y)
                        Just id -> Offset (id, source_offset_x, source_offset_y)
                , endPos=
                    case destination_id of
                        Nothing -> Global (destination_offset_x, destination_offset_y)
                        Just id -> Offset (id, destination_offset_x, destination_offset_y)
                , title=title})
                ("id" := Json.int)
                ("source_id" := Json.oneOf [Json.null Nothing, Json.map Just Json.int])
                ("destination_id" := Json.oneOf [Json.null Nothing, Json.map Just Json.int])
                ("source_offset_x" := Json.float)
                ("source_offset_y" := Json.float)
                ("destination_offset_x" := Json.float)
                ("destination_offset_y" := Json.float)
                ("title" := Json.oneOf [Json.string, Json.object1 (\x -> toString x) Json.float ] )
    in
        Json.object2 (,)
            ("fcShapes" := Json.list toFcShape)
            ("fcArrows" := Json.list toFcArrow)

readShapeType : String -> ShapeType
readShapeType s =
    case s of
        "Start" -> Start
        --"End" -> End
        --"Action" -> Action
        --"Condition" -> Condition
        --_ -> End
        _ -> Start


saveElements : Model -> Task Http.RawError Http.Response
saveElements model = 
    let toShapeObject s = 
            object [ ("id", Json.Encode.int s.id)
               , ("shapeType", Json.Encode.string (toString s.shapeType))
               , ("x", Json.Encode.float s.x)
               , ("y", Json.Encode.float s.y)
               , ("text", Json.Encode.string (s.text))
              , ("title", Json.Encode.string (s.title)) ]

        toArrowObject a = 
            let (source_id, source_offset_x, source_offset_y) = 
                case a.startPos of
                    Offset (sid, sx, sy) -> (Json.Encode.int sid, sx, sy)
                    Global (sx, sy) -> (Json.Encode.null, sx, sy)
                (destination_id, destination_offset_x, destination_offset_y) = 
                case a.endPos of
                    Offset (eid, ex, ey) -> (Json.Encode.int eid, ex, ey)
                    Global (ex, ey) -> (Json.Encode.null, ex, ey)
            in
                object [ ("id", Json.Encode.int a.id)
                       , ("source_id", source_id)
                       , ("destination_id", destination_id)
                       , ("source_offset_x", Json.Encode.float source_offset_x)
                       , ("source_offset_y", Json.Encode.float source_offset_y)
                       , ("destination_offset_x", Json.Encode.float destination_offset_x)
                       , ("destination_offset_y", Json.Encode.float destination_offset_y)
                       , ("title", Json.Encode.string a.title) ]
        json = encode 4 (object
            [ ("shapes", Json.Encode.list (List.map toShapeObject model.fcShapes))
            , ("arrows", Json.Encode.list (List.map toArrowObject model.fcArrows))])

    in
        Http.send
            Http.defaultSettings
            { verb="POST"
            , headers = []
            , url = "http://localhost/elm/thousandDangers/src/db.php"
            , body = Http.multipart [ Http.stringData "action" "save" , Http.stringData "flowchart" json]
            }
