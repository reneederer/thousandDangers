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
    let x = loadElements |> perform (\a -> HttpFailure (toString a)) (\a -> HttpSuccess a)
    in
    {
        fcShapes = []
        , fcArrows = []
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElementId= Nothing
        , currentLine=Nothing
        , mainDivOffset={x=0.0, y=0.0}
        , graphicsSettings = { fontSize = 18.0
                             , fontFamily = "Courier"
                             , innerPadding = 10.0
                             , outerPadding = 20.0
                             }
    } ! [x]

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
                ArrowMiddle id -> { model | selectedElementId = Just <| FcArrowId id } ! []
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
                                             { id=findFreeId model.fcShapes, shapeType=Action, x=0.0, y=0.0, text="", title="Aktion" } 
                                             :: model.fcShapes
                      } ! []
                66 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=Condition, x=0.0, y=0.0, text="", title="Bedingung" } 
                                             :: model.fcShapes
                      } ! []
                69 -> { model | fcShapes =
                                             { id=findFreeId model.fcShapes, shapeType=End, x=0.0, y=0.0, text="", title="Ende" } 
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
                68 ->
                    let _ = Debug.log "fcShapes" model.fcShapes
                    in
                        model ! []
                46 -> (Maybe.withDefault model (Maybe.map (removeElement model) model.selectedElementId)) ! []
                c -> model ! []
        SetScrollPosition s -> 
            model ! [getScrollPosition s]
        ScrollPositionTold scrollOffset ->
            Debug.log (toString scrollOffset.x)
            { model | mainDivOffset = scrollOffset } ! []
        HttpSuccess s ->
            let _ = Debug.log "success" s
            in
            { model | fcShapes=(fst s), fcArrows=(snd s) } ! []
        HttpFailure s -> 
            let _ = Debug.log "Failed to load" s
            in
            model ! []
        TitleChanged s ->
            let m = { model | fcShapes =
                                  model.fcShapes
                                  |> List.map (\el -> if Just (FcShapeId el.id) == model.selectedElementId then {el | title = s} else el)
                            , fcArrows =  
                                  model.fcArrows
                                  |> List.map (\ar -> if Just (FcArrowId ar.id) == model.selectedElementId then {ar | title = s} else ar)
                    }
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
                        _ = Debug.log "upMsg currentLine" "hallo"
                    in
                    { model | dragElement=Nothing, currentLine=newCurrentLine } ! []
                Outer id ->
                    let newCurrentLine = Maybe.map (\l -> (fst l, (Just id, 0, 0))) model.currentLine
                        _ = Debug.log "upMsg currentLine" "hallo"
                    in { model | dragElement=Nothing, currentLine=newCurrentLine } ! []
        MouseUp pos ->
            let arrow = Maybe.map (\l -> { id=findFreeId model.fcArrows, startPos=(fst l), endPos=(snd l) }) model.currentLine
                _ = Debug.log "mouseup " model.currentLine
                h = case arrow of 
                    Nothing -> []
                    Just a ->
                        case a.endPos of
                            (Just id, _, _) ->
                                let element = getShapeWithId model id
                                    (offsetX, offsetY) = 
                                        case element of
                                            Nothing -> (0.0, 0.0)
                                            Just el -> (toFloat pos.x - el.x + model.mainDivOffset.x, toFloat pos.y - el.y + model.mainDivOffset.y)
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
        SaveElements _ ->
            let x = (saveElements model |> perform (\a -> HttpFailure (toString a)) (\a -> HttpFailure (toString a)))
            in
                model ! [x]

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
port unloadRequested : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ scrollPositionTold ScrollPositionTold
              , unloadRequested SaveElements
              , Mouse.moves MouseMove
              , Mouse.ups MouseUp
              , Mouse.downs MouseDown]

