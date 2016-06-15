port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.App as Html
import Http
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug
import Svg.Events exposing (..)
import Mouse
import Keyboard
import VirtualDom exposing (onWithOptions)
import Json.Encode exposing (..)
import Json.Decode as Json
import Json.Decode exposing ((:=))
import String exposing (length)
import Char exposing (toCode, fromCode)
import Maybe exposing (withDefault)
import List
import Task exposing (..)
import String
import List

myFontSize = 20
myFontFamily = "Courier"
innerPadding = 20.0

outerPadding = 30.0


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type ShapeType = 
      Start
    | End
    | Condition
    | Action

type alias Position = 
    { x : Float
    , y : Float
    }

type alias FcShape =
    { id : Id
    , shapeType : ShapeType
    , x : Float
    , y : Float
    , text : String
    , title : String }

type FcPos = 
      Offset (Id, Float, Float)
    | Global ( Float, Float)

type alias FcArrow = 
    { id : Id
    , startPos : FcPos
    , endPos : FcPos
    , text : String
    , title : String }

type alias Id = Int

type ShapeAreaType = 
      Outer
    | Inner

type alias ShapeArea = 
    { areaType : ShapeAreaType
    , id : Id }

type Msg =
      MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | DownMsg ShapeArea
    | UpMsg ShapeArea
    | HttpSuccess (List FcShape, List FcArrow)
    | HttpFailure String
    | TitleChanged String
    | TextChanged String
    | SetScrollPosition String
    | ScrollPositionTold Position
    | SelectArrowElement Id

type FcElement = 
      Arrow FcArrow
    | Shape FcShape

-- MODEL


type alias Model =
    { fcShapes : List FcShape
    , fcArrows : List FcArrow
    , debugMsg : String
    , dragElement : Maybe ShapeArea
    , dragOffsetX : Float
    , dragOffsetY : Float
    , selectedElement : Maybe FcElement
    , dragArrow : Maybe FcArrow
    , mainDivOffset : Position
    }


init : (Model, Cmd Msg)
init =
    ({
        fcShapes =
            [ ({id=1, shapeType=Start, x=40,y=90,text="abc",title="1"})
            , ({id=2, shapeType=Action, x=40,y=490,text="def",title="tre2"})]
            --, ({id=3, shapeType=End, x=150,y=290,text="",title="j23j"})]
        , fcArrows =
            [ ({id=1, startPos= Offset (1, 10, 40), endPos=Offset (2, 50, 5), text="t1", title="1"})
            --, ({id=2, startPos=Global (190, 50), endPos=Global (800, 500), text="t2", title="ijs2"})
            ]
        , debugMsg = ""
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElement=Nothing
        , dragArrow=Nothing
        , mainDivOffset={x=0.0, y=0.0}
    }, Cmd.none)

-- UPDATE



getShapeWithId : List FcShape -> Id -> Maybe FcShape
getShapeWithId l id = 
    List.head <| List.filter (\el -> el.id == id) l

getArrowWithId : List FcArrow -> Id -> Maybe FcArrow
getArrowWithId l id = 
    List.head <| List.filter (\el -> el.id == id) l
    

findFreeId : List {a | id:Id} -> Id
findFreeId l = 
    List.foldl (\el state -> if el.id >= state then el.id + 1 else state) 1 l

setTitle : FcElement -> String -> FcElement
setTitle fcElement title = 
    case fcElement of
        Shape shape -> Shape { shape | title = title } 
        Arrow arrow -> Arrow { arrow | title = title } 

getTitle : FcElement -> String
getTitle fcElement = 
    case fcElement of
        Shape shape -> shape.title
        Arrow arrow -> arrow.title

setText : FcElement -> String -> FcElement
setText fcElement text = 
    case fcElement of
        Shape shape -> Shape { shape | text = text } 
        Arrow arrow -> Arrow { arrow | text = text } 

getText : FcElement -> String
getText fcElement = 
    case fcElement of
        Shape shape -> shape.text
        Arrow arrow -> arrow.text

getId : FcElement -> Id
getId fcElement = 
    case fcElement of
        Shape shape -> shape.id
        Arrow arrow -> arrow.id

removeShape : Model -> Id -> Model
removeShape model id =
    model
--    let mshape = getElementWithId model id
--    in
--    case mshape of
--        Nothing -> model
--        Just shape ->
--            let newShapes = List.foldr (\el state -> if el.id == id then state else (el::state)) [] model.fcShapes
--                newArrows = List.map (\ar ->
--                    let newStartPos = 
--                        case ar.startPos of
--                            Offset (id, x, y) -> if (id == shape.id) then Global (x + shape.x, y + shape.y) else ar.startPos
--                            _ -> ar.startPos
--                        newEndPos = 
--                        case ar.endPos of
--                            Offset (id, x, y) -> if (id == shape.id) then Global (x + shape.x, y + shape.y) else ar.endPos
--                            _ -> ar.endPos
--                    in
--                        { ar | startPos = newStartPos, endPos = newEndPos }) model.fcArrows
--            in
--                { model | fcShapes=newShapes, fcArrows=newArrows }
--




loadElements : Task Http.Error (List FcShape, List FcArrow)
loadElements =
    Http.post
        decodeElements
        ("http://localhost/elm/thousandDangers/src/db.php")
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
                , text=""
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
        "End" -> End
        "Action" -> Action
        "Condition" -> Condition
        _ -> End


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




moveStartPosTo : Maybe FcArrow -> FcPos -> Maybe FcArrow
moveStartPosTo fcArrow fcPos = 
    fcArrow
    |> Maybe.map (\arrow -> { arrow | startPos = fcPos })



moveEndPosTo : Maybe FcArrow -> FcPos -> Maybe FcArrow
moveEndPosTo fcArrow fcPos = 
    fcArrow
    |> Maybe.map (\arrow -> { arrow | endPos = fcPos })



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownMsg { areaType, id} ->
            case areaType of
                Inner -> { model | dragElement=(Just {areaType=areaType, id=id})
                                , selectedElement=Maybe.map Shape (getShapeWithId model.fcShapes id)
                                , dragArrow=Nothing
                         } ! []
                Outer -> { model | dragElement=(Just {areaType=areaType, id=id})
                                , selectedElement=Maybe.map Shape (getShapeWithId model.fcShapes id)
                                , dragArrow=Just { id=findFreeId model.fcArrows
                                                   , startPos=Offset (id, 0, 0)
                                                   , endPos=Offset (id, 0, 0)
                                                   , text="dfas"
                                                   , title="adsdsfa" }
                         } ! []
        MouseDown lpos->
            let pos = localToGlobal {x=toFloat lpos.x, y=toFloat lpos.y} model.mainDivOffset 
                id = 
                    case model.dragElement of
                        Nothing -> Nothing
                        (Just  ({areaType, id})) -> Just id
                shapePos = Maybe.map (getShapeWithId model.fcShapes) id
            in
                case shapePos of
                    Just (Just el) ->
                        let offsetX=(pos.x)-el.x
                            offsetY=(pos.y)-el.y
                        in
                            { model | dragOffsetX=offsetX
                                    , dragOffsetY=offsetY
                                    , dragArrow=Maybe.map (\line -> 
                                                        case line.startPos of 
                                                            Offset (id, x, y) -> {line | startPos=Offset (id, offsetX, offsetY)
                                                                                       , endPos=Offset (id, offsetX, offsetY)}
                                                            _ -> line)
                                         model.dragArrow } ! []
                    _ ->
                        model ! []
        KeyMsg code ->
            case code of 
                65 -> {model | debugMsg = "a pressed", fcShapes = model.fcShapes ++ [ { id=findFreeId model.fcShapes
                                                                                      , shapeType=Start
                                                                                      , x=400.0
                                                                                      , y=400.0
                                                                                      , text="Start"
                                                                                      , title="Start"
                                                                                    } ]} ! []
                66 ->
                    let x = loadElements |> perform (\a -> HttpFailure (toString a)) (\a -> HttpSuccess a)
                    in
                        {model | debugMsg="b" ++ toString x} ! [x]
                67 ->
                    let x = (saveElements model |> perform (\a -> HttpFailure (toString a)) (\a -> HttpFailure (toString a)))
                    in
                        {model | debugMsg="jkas"} ! [x]
                46 -> (removeSelectedElement model) ! []
                c -> { model | debugMsg="kein event" ++ (toString c) } ! []
        SetScrollPosition s -> 
            model ! [getScrollPosition s]
        ScrollPositionTold scrollOffset ->
            { model | mainDivOffset = scrollOffset } ! []
        SelectArrowElement id ->
            { model | selectedElement=Maybe.map Arrow (getArrowWithId model.fcArrows id) } ! []
        HttpSuccess s -> {model | fcShapes=(fst s), fcArrows=(snd s)} ! []
        HttpFailure s -> 
            { model | debugMsg="HttpFailure " ++ s } ! []
        TitleChanged s ->
            --let m = { model | fcShapes = t.map (\el -> if Just el == model.selectedElement then (setTitle el s) else el) model.fcShapes}
            let m = model
            in
                m ! []
        TextChanged s ->
            let m = model
            --let m = { model | fcShapes = List.map (\el -> if Just el == model.selectedElement then {el | text = s} else el) model.fcShapes}
            in
                m ! []

        UpMsg { areaType, id} ->
            {model |dragElement=Nothing, dragArrow= moveEndPosTo model.dragArrow (Offset (id, 0, 0))} ! []
        MouseUp lpos ->
            let pos = localToGlobal {x=toFloat lpos.x, y=toFloat lpos.y} model.mainDivOffset
            in
                    { model | dragElement=Nothing
                            , dragArrow=Nothing
                            , fcArrows = (model.dragArrow
                                            |> Maybe.map (\a ->
                                                case a.endPos of
                                                    Offset (endId, _, _) ->
                                                        getShapeWithId model.fcShapes endId
                                                        |> Maybe.map (\el ->
                                                            if isSameElement a.startPos a.endPos
                                                            then []
                                                            else [{ a | endPos=Offset (endId,pos.x-el.x, pos.y - el.y) }])
                                                        |> Maybe.withDefault []
                                                    _ -> [])
                                            |> Maybe.withDefault []) ++ model.fcArrows } ! []

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
                            (getShapeWithId model.fcShapes id
                            |> Maybe.map (\el -> { model | dragArrow=moveEndPosTo model.dragArrow (Global (pos.x, pos.y))})
                            |> Maybe.withDefault model) ! []


removeSelectedElement model = 
    case model.selectedElement of
        Nothing -> model
        Just selectedElement ->
            case selectedElement of
                Shape shape -> { model | selectedElement=Nothing
                                       , fcShapes=List.filter (\currentShape -> currentShape.id /= shape.id) model.fcShapes
                               }
                Arrow arrow -> { model | selectedElement=Nothing
                                       , fcArrows=List.filter (\a -> a.id /= arrow.id) model.fcArrows
                               }

        


localToGlobal : Position -> Position -> Position
localToGlobal pos offset = 
    { x = pos.x + offset.x
    , y = pos.y + offset.y }


moveElementTo : Model -> Maybe ShapeArea -> Float -> Float -> Model
moveElementTo model movingElement x y = 
    case movingElement of
        Nothing -> {model | debugMsg="not found"}
        Just {areaType, id} ->
            let newShapes = List.map (\el ->
                    if el.id == id
                    then
                        {el | x = x, y=y}
                    else
                        el
                ) model.fcShapes
            in
                { model | debugMsg=toString id, fcShapes = newShapes }


isSameElement : FcPos -> FcPos -> Bool
isSameElement pos1 pos2 =
    case pos1 of
        Offset (id1, _, _) ->
            case pos2 of
                Offset (id2, _, _) -> id1 == id2
                _ -> False
        _ -> False


port scrollPositionTold : (Position -> msg) -> Sub msg

port getScrollPosition : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ scrollPositionTold ScrollPositionTold
              , Mouse.moves MouseMove
              , Mouse.ups MouseUp
              , Mouse.downs MouseDown]



arrowsWithStartShape : Model -> Id -> List FcArrow
arrowsWithStartShape model id =
    List.filter (\x ->
        case x.startPos of
            Offset (shapeId, _, _) -> shapeId == id
            _ -> False) model.fcArrows

arrowsWithEndShape : Model -> Id -> List FcArrow
arrowsWithEndShape model id =
    List.filter (\x ->
        case x.endPos of
            Offset (shapeId, _, _) -> shapeId == id
            _ -> False) model.fcArrows

createHtml model id = 
    createHtml1 model id 0

createHtml1 : Model -> Id -> Int -> List (Html Msg)
createHtml1 model id tries =
    let mshape = getShapeWithId model.fcShapes id
    in
        if tries == 1000 then []
        else
        case mshape of
            Nothing -> []
            Just shape ->
                let arrows = arrowsWithStartShape model id
                    newIds = List.filterMap (\x ->
                                            case x.endPos of
                                                Offset (endShapeId, _, _) -> Just endShapeId
                                                _ -> Nothing) arrows
                in
                    case shape.shapeType of
                        Condition ->
                            (Html.p
                                [Html.Attributes.id (toString id)]
                                [text shape.text])::
                            (List.foldl
                                (\el state ->
                                    case el.endPos of
                                        Offset (endId, _, _) ->
                                            Html.p [] [Html.a [Html.Attributes.href "#", Html.Events.onClick (SelectArrowElement endId)] [Html.text el.title]]::state
                                        _ -> state
                                ) [] arrows
                            )
                        _ -> 
                            (Html.p
                                [Html.Attributes.id (toString id)] [text shape.text])::
                            (createHtml1 model (Maybe.withDefault -1 (List.head newIds)) (tries+1))


getStartShapeId : FcArrow -> Maybe Id
getStartShapeId arr = 
    case arr.startPos of
       Offset (startId, _, _) -> Just startId
       _ -> Nothing

getEndShapeId : FcArrow -> Maybe Id
getEndShapeId arr = 
    case arr.endPos of
       Offset (endId, _, _) -> Just endId
       _ -> Nothing






view : Model -> Html Msg
view model =
    let cur = Maybe.withDefault [] (Maybe.map (\x -> [fcArrowToSvg model x]) model.dragArrow)
        allArrows = cur++(List.map (fcArrowToSvg model) model.fcArrows)
        (arrowSvgs, arrowHeads, arrowTexts) = List.foldl (\(x,y,z) (xs, ys, zs) ->
                                                        ((x::xs), (y::ys), (z::zs))
                                                    ) ([],[],[]) allArrows 
        --_ = Debug.log "test" (arrowTexts)
    in
        Html.div [Html.Attributes.style [("position", "absolute"), ("top", "0px"), ("left", "0px"), ("width", "100%"), ("height", "100%")]]
        [ Html.div [ Html.Attributes.id "mainEl"
                   , Html.Attributes.tabindex 0
                   , Html.Events.on "keydown" (Html.Events.keyCode |> Json.map (\keyCode -> KeyMsg keyCode))
                   , Html.Events.on "scroll" (Json.succeed (SetScrollPosition "mainEl"))
                   , Html.Attributes.style [("width", "70%"), ("height", "100%"),  ("overflow", "scroll"), ("backgroundColor", "yellow")]] [
            svg [ Svg.Attributes.cursor "default", Svg.Attributes.style "background-color:lightblue", viewBox "0 0 8500 8500", width "8500", height "8500", pointerEvents "none"]
                ([defs []
                    (arrowHeads ++ arrowTexts)
                ] ++
                (List.map (fcShapeToSvg model) model.fcShapes++
                arrowSvgs


                 ))] 
        , Html.div [
            Html.Attributes.style
            [ ("position", "fixed")
            , ("top", "0px")
            , ("right", "0px")
            , ("width", "30%")
            , ("height", "100%")
            , ("backgroundColor", "lightGray")]]
            [Html.table
                [Html.Attributes.style
                    [ ("width", "100%")
                    , ("height", "50%")]]
                [ Html.tr []
                    [ Html.td []
                        [ text "Titel"
                        , Html.input [ Html.Attributes.value (Maybe.withDefault "" (Maybe.map getTitle model.selectedElement))
                                     , Html.Events.onInput TitleChanged
                                     ][]
                        ]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ text "Text"
                        , Html.textarea [
                                            Html.Attributes.rows 20,
                                            Html.Attributes.style [("width", "70%")],
                                            Html.Attributes.value (Maybe.withDefault "" (Maybe.map getText model.selectedElement)),
                        Html.Events.onInput TextChanged][]]
                    ]
                ]
            , Html.div [Html.Attributes.style [("height", "50%"), ("overflow", "scroll")]] (createHtml model (Maybe.withDefault 1 (Maybe.map (\x -> getId x) model.selectedElement)))
            , Html.div [Html.Attributes.style[ ("position", "absolute")
                                               , ("bottom", "0px")
                                               , ("left", "0px")
                                               , ("width", "100%")
                                               , ("height", "300px")]
                        ]
                        [Html.text (List.foldl (\a state -> state ++ "(" ++ toString a.id ++ ", " ++ a.title ++ "), ") "" (model.fcArrows))
              ]
            ]
        ]



fcArrowToSvg : Model -> FcArrow -> (Svg.Svg Msg, Svg.Svg Msg, Svg.Svg Msg)
fcArrowToSvg model fcArrow = 
    let (tartX, tartY) = 
            case fcArrow.startPos of
                Global (x, y) -> (x, y)
                Offset (startId, x, y) ->
                    getShapeWithId model.fcShapes startId
                    |> Maybe.map (\e -> (x+e.x, y+e.y))
                    |> Maybe.withDefault (0, 0)
        (ndX, ndY) = 
            case fcArrow.endPos of
                Global (x, y) -> (x, y)
                Offset (endId, x, y) ->
                    getShapeWithId model.fcShapes endId
                    |> Maybe.map (\e -> (x+e.x, y+e.y))
                    |> Maybe.withDefault (0, 0)

        (startX, startY, endX, endY, myMarkerAttribute) = 
            if(tartX <= ndX || True)
            then (tartX, tartY, ndX, ndY, markerEnd ("url(#arrowHead" ++ toString fcArrow.id ++ ")"))
            else (ndX, ndY, tartX, tartY, markerEnd ("url(#arrowHead" ++ toString fcArrow.id ++ ")"))
        myStrokeWidth = 2
        l = Basics.max 1 (sqrt (((endX-startX) ^ 2) + ((endY-startY)^2))) / myStrokeWidth
        textOrientation = "auto" 

        mt1 = 
            if endX <= startX
            then "scale(-1,-1) translate(" ++ (toString <| 2*l/2) ++ ", 0)"
            else "translate(0, 0)"

        myMarker = marker [Svg.Attributes.id ( "arrowHead" ++ toString fcArrow.id)
                                             , markerWidth (toString l)
                                             , markerHeight "12"
                                             , overflow "visible"
                                             , viewBox "-6, -6, 12, 12"
                                             , refX "5"
                                             , refY "0"
                                             , orient textOrientation
                          ]
                          [ g []
                              [ polygon [ points "-2,0 -5,5 5,0 -5,-5", fill "red", stroke "black", strokeWidth "1px" ] []
                                      , rect [ y "-6"
                                             , x (toString (-l))
                                             , width (toString (l+6))
                                             , height "12"
                                             , Svg.Attributes.style "stroke-width:1;stroke-dasharray:10,10;stroke:rgb(0, 0, 190);fill-opacity:0.0;"] []

                                , text' [ Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                                , transform mt1
                                , fontSize (toString myFontSize)
                                , fontFamily myFontFamily
                                , x (toString (-l/2-(fst (getTextDimension fcArrow.title myFontFamily myFontSize))/2))
                                , y (toString -15)
                                , fill "blue"]
                                [Svg.text fcArrow.title]
                              ]
                          ]
                            
        myMarker1 = marker []
                                [ text' [ Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                                , fontSize (toString myFontSize)
                                , fontFamily myFontFamily
                                , x (toString (l/2-(fst (getTextDimension fcArrow.title myFontFamily myFontSize))/4))
                                , y (toString -15)
                                , fill "blue"] [Svg.text fcArrow.title]]

    in
        let distance = 50/myStrokeWidth
            (sx, sy) = (startX + ((endX - startX)) * ((distance/l)), startY + (endY - startY) * ((distance/l)))
            (ex, ey) =
                if l <= 2*distance
                then (sx, sy)
                else (endX - ((endX - startX)) * ((distance/l)), endY - (endY - startY) * ((distance/l)))
            t = 
                Svg.g [Svg.Attributes.cursor "default"] [
                    Svg.path [

                              d ( "M " ++ (toString startX) ++ ", " ++ (toString startY) ++
                                " L " ++ (toString ((endX-startX)/2+startX)) ++ ", " ++ (toString ((endY-startY)/2+startY)) ++
                                " L " ++ (toString endX) ++ ", " ++ (toString endY))
                              , myMarkerAttribute
                             --, markerStart ("url(#arrowCaption" ++ toString fcArrow.id ++ ")")
                             , Svg.Attributes.style ("stroke:rgb(255,0,0);stroke-width:" ++ (toString myStrokeWidth))] []
                    ,
                    Svg.path [ pointerEvents "all"
                             , Html.Events.onClick (KeyMsg 65)

                             , d ( "M " ++ (toString sx) ++ ", " ++ (toString sy) ++
                                " L " ++ (toString (ex)) ++ ", " ++ (toString <|ey))
                             , Svg.Attributes.style ("stroke:rgb(0,255,0);stroke-width:" ++ (toString <| 14 * myStrokeWidth) ++ ";stroke-opacity:0.0001")] []
                    ]
            in
                (t, myMarker, myMarker1)            
                         


fcShapeToSvg : Model -> FcShape -> Svg.Svg Msg
fcShapeToSvg model fcShape = 
    let outerColor = "#FFF9CE"
        innerColor = "blue"
        strokeColor = if model.selectedElement == Just (Shape fcShape) then "red" else outerColor
        textColor = "red"
    in
    case fcShape.shapeType of
        Start ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
            in
                g [ pointerEvents "all"] [
                         
                    rect [ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                         , onMouseUp (UpMsg {areaType=Outer, id=fcShape.id})
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "30"
                         , ry "30"
                         , stroke strokeColor
                         , strokeDasharray "10,10"
                         , fill outerColor
                         ] [],
                    rect [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                         , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                         , x (toString (fcShape.x + outerPadding))
                         , y (toString (fcShape.y + outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "25"
                         , ry "25"
                         , fill innerColor ] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                          , pointerEvents "none"
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize/3))
                          , fill textColor] [Svg.text fcShape.title]]
        Action ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
            in
                g [ pointerEvents "all"] [
                    rect [ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                          , onMouseUp (UpMsg {areaType=Outer, id=fcShape.id})
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         , stroke strokeColor
                         , strokeDasharray "10,10"
                         , fill outerColor] [],
                    rect [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                         , x (toString (fcShape.x + outerPadding))
                         , y (toString (fcShape.y + outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         , fill innerColor ] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill textColor ] [Svg.text fcShape.title]]
        End ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
            in
                g [ pointerEvents "all"] [
                         
                    rect [ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                         , onMouseUp (UpMsg {areaType=Outer, id=fcShape.id})
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "30"
                         , ry "30"
                         , stroke strokeColor
                         , strokeDasharray "10,10"
                         , fill outerColor
                         ] [],
                    rect [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                         , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                         , x (toString (fcShape.x + outerPadding))
                         , y (toString (fcShape.y + outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "25"
                         , ry "25"
                         , fill innerColor ] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , onMouseUp (UpMsg {areaType=Inner, id=fcShape.id})
                          , pointerEvents "none"
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize/3))
                          , fill textColor] [Svg.text fcShape.title]]
        Condition ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
                innerStartX = fcShape.x + outerPadding -- - smallAmount
                innerStartY = fcShape.y + outerPadding
            in
                g [pointerEvents "all"
                         , onMouseUp (UpMsg {areaType=Outer, id=fcShape.id})][
                    polygon[ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                         , points ((toString <| fcShape.x) ++ "," ++ (toString <| fcShape.y + outerShapeHeight/2) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth/2)) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth)) ++ "," ++ (toString (fcShape.y + outerShapeHeight/2)) ++ " "
                                ++ (toString <| fcShape.x + outerShapeWidth / 2) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " ")

                         , stroke strokeColor
                         , strokeDasharray "10,10"
                         , fill outerColor ] [],
                    polygon [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                         , points ((toString <| innerStartX) ++ "," ++ (toString <| innerStartY + innerShapeHeight/2) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth/2)) ++ "," ++ (toString <| innerStartY) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth)) ++ "," ++ (toString (innerStartY + innerShapeHeight/2)) ++ " "
                                ++ (toString <| innerStartX + innerShapeWidth / 2) ++ "," ++ (toString (innerStartY + innerShapeHeight)) ++ " ")

                         , fill innerColor] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill textColor] [Svg.text fcShape.title]]


getTextDimension : String -> String -> Int -> (Float,Float)
getTextDimension text  font  fontSize = 
    case font of
        "Courier" -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)
        _ -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)











