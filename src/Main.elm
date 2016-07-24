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
            , ({id=2, shapeType=Start, x=40,y=490,text="def",title="treasdfasdf2"})
            , ({id=3, shapeType=Start, x=340,y=290,text="Noch ein Element",title="Noch ein Element"})
            ]
        , fcArrows =
            [ ({id=1, startPos= (Just 1, 10, 40), endPos=(Just 2, 50, 5), title="Start"})
            , ({id=2, startPos=(Just 2, 10, 50), endPos=(Just 3, 80, 50), title="ijs2"})
            ]
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElementId= Just <| FcShapeId 1
        , currentLine=Nothing
        , mainDivOffset={x=0.0, y=0.0}
        , graphicsSettings = { fontSize = 28.0
                             , fontFamily = "Courier"
                             , innerPadding = 40.0
                             , outerPadding = 40.0
                             }
    }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownMsg areaType ->
            case areaType of
                Inner id ->
                    let _ = Debug.log "Inner" id in
                    {model | dragElement=Just <| Inner id, selectedElementId=Just <| FcShapeId id, currentLine=Nothing } ! []
                Outer id ->
                    let _ = Debug.log "Outer" id in
                    {model | dragElement=Just <| Outer id, selectedElementId=Just <| FcShapeId id, currentLine=Just ((Just id, 0, 0), (Just id, 0, 0)) } ! []
                _ -> model ! []
        MouseDown lpos->
            let pos = localToGlobal {x=toFloat lpos.x, y=toFloat lpos.y} model.mainDivOffset 
                id = 
                    case model.dragElement of
                        Nothing -> Nothing
                        Just areaType ->
                            case areaType of
                                ArrowStart id -> Just id
                                ArrowEnd id -> Just id
                                ArrowMiddle id -> Just id
                                Inner id -> Just id
                                Outer id -> Just id
                shapePos = Maybe.map (getShapeWithId model) id
            in
                case shapePos of
                    Just (Just el) ->
                        let offsetX=(pos.x)-el.x
                            offsetY=(pos.y)-el.y
                        in
                            { model | dragOffsetX=offsetX
                                    , dragOffsetY=offsetY
                                    , currentLine=Maybe.map (\(s, e) -> 
                                                        case s of 
                                                            (Just id, x, y) -> ((Just id, offsetX, offsetY), (Just id, offsetX, offsetY))
                                                            _ -> (s, e))
                                         model.currentLine } ! []
                    _ ->
                        model ! []
        KeyMsg code ->
            case code of 
                65 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Start, x=0.0, y=0.0, text="", title="Aktion" } 
                                             :: model.fcShapes
                      } ! []
                66 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Start, x=0.0, y=0.0, text="", title="Bedingung" } 
                                             :: model.fcShapes
                      } ! []
                69 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Start, x=0.0, y=0.0, text="", title="Ende" } 
                                             :: model.fcShapes
                      } ! []
                76 ->
                    let x = loadElements |> perform (\a -> HttpFailure (toString a)) (\a -> HttpSuccess a)
                    in
                        model ! [x]
                83 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Start, x=0.0, y=0.0, text="", title="Start" } 
                                             :: model.fcShapes
                      } ! []
                67 ->
                    let x = (saveElements model |> perform (\a -> HttpFailure (toString a)) (\a -> HttpFailure (toString a)))
                    in
                        model ! [x]
                46 -> (Maybe.withDefault model (Maybe.map (removeElement model) model.selectedElementId)) ! []
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
            let m = { model | fcShapes = List.map (\el -> if Just (FcShapeId el.id) == model.selectedElementId then {el | title = s} else el) model.fcShapes}
            in
                m ! []
        TextChanged s ->
            let m = { model | fcShapes = List.map (\el -> if Just (FcShapeId el.id) == model.selectedElementId then {el | text = s} else el) model.fcShapes}
            in
                m ! []

        UpMsg areaType ->
            let _ = Debug.log "id ist.................................." areaType in
            case areaType of
                ArrowStart id -> model ! []
                ArrowEnd id -> model ! []
                ArrowMiddle id -> model ! []
                Inner id ->
                    let newCurrentLine = Maybe.map (\l -> (fst l, (Just id, 0, 0))) model.currentLine
                        _ = Debug.log "currentLine" newCurrentLine in
                    { model | dragElement=Nothing, currentLine=newCurrentLine } ! []
                Outer id ->
                    let newCurrentLine = Maybe.map (\l -> (fst l, (Just id, 0, 0))) model.currentLine
                        _ = Debug.log "currentLine" newCurrentLine
                    in { model | dragElement=Nothing, currentLine=newCurrentLine } ! []
        MouseUp pos ->
            let arrow = Maybe.map (\l -> setArrowEnd { id=findFreeId model.fcArrows, startPos=(fst l), endPos=(snd l) }) model.currentLine
                h = case arrow of 
                    Nothing -> []
                    Just a ->
                        case a.endPos of
                            (Just id, _, _) ->
                                let element = getShapeWithId model id
                                    (offsetX, offsetY) = 
                                        case element of
                                            Nothing -> (0.0, 0.0)
                                            Just el -> (toFloat pos.x - el.x, toFloat pos.y - el.y)
                                    elid = findFreeId model.fcArrows
                                in
                                    if (doesPositionShareElements a.startPos a.endPos) then [] else [{id=elid, startPos=a.startPos, endPos=(Just id, offsetX, offsetY), title=toString elid}]
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
                Just areaType -> 
                    case areaType of
                        Inner id ->
                            let m = moveElementTo model model.dragElement (pos.x-model.dragOffsetX) (pos.y-model.dragOffsetY)
                            in
                                m ! []
                        Outer id ->
                            let element = getShapeWithId model id
                                m = 
                                    case element of
                                        Nothing -> model
                                        Just el -> 
                                            { model | currentLine=(Maybe.map (\(startPos, _) -> (startPos, (Nothing, pos.x, pos.y))) model.currentLine)}
                            in
                                m ! []
                        _ -> model ! []
        CreateNewShape shapeType -> model ! []

localToGlobal : Position -> Position -> Position
localToGlobal pos offset = 
    { x = pos.x + offset.x
    , y = pos.y + offset.y }


moveElementTo : Model -> Maybe AreaType -> Float -> Float -> Model
moveElementTo model movingElement x y = 
    case movingElement of
        Nothing -> model
        Just areaType ->
            case areaType of
                Inner id -> let newShapes = List.map (\el ->
                            if el.id == id
                            then
                                {el | x = x, y=y}
                            else
                                el) model.fcShapes
                    in { model | fcShapes = newShapes }
                _ -> model




port scrollPositionTold : (Position -> msg) -> Sub msg
port getScrollPosition : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ scrollPositionTold ScrollPositionTold
              , Mouse.moves MouseMove
              , Mouse.ups MouseUp
              , Mouse.downs MouseDown]

