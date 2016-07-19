port module Main exposing (..)

import Html.App
import Debug
import Mouse
import Keyboard
import VirtualDom exposing (onWithOptions)
import Char exposing (toCode, fromCode)
import List
import Task exposing (perform)

import FcTypes exposing (..)
import FcElement exposing (..)
import FcGraphics exposing (..)
import FcDatabase exposing (..)




main =
    Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



init : (Model, Cmd Msg)
init =
    ({
        fcShapes =
            [ ({id=1, shapeType=Start, x=40,y=90,text="abc",title="8"})
            , ({id=2, shapeType=Action, x=40,y=490,text="def",title="treasdfasdf2"})]
            --, ({id=3, shapeType=End, x=150,y=290,text="",title="j23j"})]
        , fcArrows =
            [ ({id=1, startPos= Offset (1, 10, 40), endPos=Offset (2, 50, 5), title="1"})
            , ({id=2, startPos=Global (190, 50), endPos=Global (800, 500), title="ijs2"})]
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElement= Just <| FcShapeId 1
        , currentLine=Nothing
        , displayedDivId=1
        , mainDivOffset={x=0.0, y=0.0}
    }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownMsg { areaType, id} ->
            case areaType of
                Inner -> {model | dragElement=(Just {areaType=areaType, id=id}), displayedDivId=id, selectedElement=Just <| FcShapeId id, currentLine=Nothing } ! []
                Outer -> {model | dragElement=(Just {areaType=areaType, id=id}), displayedDivId=id, selectedElement=Just <| FcShapeId id, currentLine=Just (Offset (id, 0, 0), Offset (id, 0, 0)) } ! []
        MouseDown lpos->
            let pos = localToGlobal {x=toFloat lpos.x, y=toFloat lpos.y} model.mainDivOffset 
                id = 
                    case model.dragElement of
                        Nothing -> Nothing
                        (Just  ({areaType, id})) -> Just id
                shapePos = Maybe.map (getShapeWithId model) id
            in
                case shapePos of
                    Just (Just el) ->
                        let offsetX=(pos.x)-el.x
                            offsetY=(pos.y)-el.y
                        in
                            Debug.log "mousedown!!!"
                            { model | dragOffsetX=offsetX
                                    , dragOffsetY=offsetY
                                    , currentLine=Maybe.map (\(s, e) -> 
                                                        case s of 
                                                            Offset (id, x, y) -> (Offset (id, offsetX, offsetY), Offset (id, offsetX, offsetY))
                                                            _ -> (s, e))
                                         model.currentLine } ! []
                    _ ->
                        model ! []
        KeyMsg code ->
            case code of 
                65 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Start, x=400.0, y=400.0, text="", title="Ein Startelement" } 
                                             :: model.fcShapes
                      } ! []
                66 ->
                    let x = loadElements |> perform (\a -> HttpFailure (toString a)) (\a -> HttpSuccess a)
                    in
                        model ! [x]
                67 ->
                    let x = (saveElements model |> perform (\a -> HttpFailure (toString a)) (\a -> HttpFailure (toString a)))
                    in
                        model ! [x]
                46 -> (Maybe.withDefault model (Maybe.map (removeElement model) model.selectedElement)) ! []
                c -> model ! []
        SetScrollPosition s -> 
            model ! [getScrollPosition s]
        ScrollPositionTold scrollOffset ->
            Debug.log (toString scrollOffset.x)
            { model | mainDivOffset = scrollOffset } ! []
        HttpSuccess s -> {model | fcShapes=(fst s), fcArrows=(snd s)} ! []
        HttpFailure s -> 
            model ! []
        TitleChanged s ->
            let m = { model | fcShapes = List.map (\el -> if Just (FcShapeId el.id) == model.selectedElement then {el | title = s} else el) model.fcShapes}
            in
                m ! []
        TextChanged s ->
            let m = { model | fcShapes = List.map (\el -> if Just (FcShapeId el.id) == model.selectedElement then {el | text = s} else el) model.fcShapes}
            in
                m ! []

        UpMsg { areaType, id} ->
            Debug.log "mouseup!!!"
            {model |dragElement=Nothing, currentLine=Maybe.map (\l -> (fst l, Offset (id, 0, 0))) model.currentLine } ! []
        DisplayDiv id ->
            { model | displayedDivId=id } ! []
        MouseUp pos ->
            let arrow = Maybe.map (\l -> { id=findFreeId model.fcShapes, startPos=(fst l), endPos=(snd l) }) model.currentLine
                h = case arrow of 
                    Nothing -> []
                    Just a ->
                        case a.endPos of
                            Offset (id, _, _) ->
                                let element = getShapeWithId model id
                                    (offsetX, offsetY) = 
                                        case element of
                                            Nothing -> (0.0, 0.0)
                                            Just el -> (toFloat pos.x - el.x, toFloat pos.y - el.y)
                                    elid = findFreeId model.fcArrows
                                in
                                    if (doesPositionShareElements a.startPos a.endPos) then [] else [{id=elid, startPos=a.startPos, endPos=Offset (id, offsetX, offsetY), title=toString elid}]
                            _ -> []
            in
                let newModel = {model | dragElement=Nothing, fcArrows=(h++model.fcArrows), currentLine=Nothing }
                in 
                    newModel ! []
        MouseMove lpos ->
            let pos = localToGlobal { x = toFloat lpos.x, y = toFloat lpos.y }  model.mainDivOffset
            in
            case model.dragElement of
                Nothing -> model ! []
                Just {areaType, id} -> 
                    case areaType of
                        Inner ->
                            let m = moveElementTo model model.dragElement (pos.x-model.dragOffsetX) (pos.y-model.dragOffsetY)
                            in
                                m ! []
                        Outer ->
                            let element = getShapeWithId model id
                                m = 
                                    case element of
                                        Nothing -> model
                                        Just el -> 
                                            { model | currentLine=(Maybe.map (\(startPos, _) -> (startPos, Global (pos.x, pos.y))) model.currentLine)}
                            in
                                m ! []

localToGlobal : Position -> Position -> Position
localToGlobal pos offset = 
    { x = pos.x + offset.x
    , y = pos.y + offset.y }


moveElementTo : Model -> Maybe ShapeArea -> Float -> Float -> Model
moveElementTo model movingElement x y = 
    case movingElement of
        Nothing -> model
        Just {areaType, id} ->
            let newShapes = List.map (\el ->
                    if el.id == id
                    then
                        {el | x = x, y=y}
                    else
                        el
                ) model.fcShapes
            in
                { model | fcShapes = newShapes }




port scrollPositionTold : (Position -> msg) -> Sub msg
port getScrollPosition : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ scrollPositionTold ScrollPositionTold
              , Mouse.moves MouseMove
              , Mouse.ups MouseUp
              , Mouse.downs MouseDown]

