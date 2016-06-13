module Main exposing (..)

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
import List exposing (head)
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
    , title : String }

type alias Id = Int

createStartElement :Id ->  Float -> Float -> FcShape
createStartElement id x y = 
        { id = id
        , shapeType = Start
        , x = x
        , y = y
        , text = "Start"
        , title = toString id
        }

createEndElement : Id -> Float -> Float -> FcShape
createEndElement id x y = 
        { id = id
        , shapeType = End
        , x = x
        , y = y
        , text = toString id
        , title = "Ende"
        }

createConditionElement : Id -> Float -> Float -> FcShape
createConditionElement id x y =
        { id = id
        , shapeType = Condition
        , x = x
        , y = y
        , text = toString id
        , title = "Mein Titel"
        }

type FcElement = 
      ShapeElement FcShape
    | ArrowElement FcArrow

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
    | UpdateView
    | DisplayDiv Int

-- MODEL


type alias Model =
    { fcShapes : List FcShape
    , fcArrows : List FcArrow
    , debugMsg : String
    , dragElement : Maybe ShapeArea
    , dragOffsetX : Float
    , dragOffsetY : Float
    , selectedElement : Id
    , currentLine : Maybe (FcPos, FcPos)
    , displayedDivId : Int
    }


init : (Model, Cmd Msg)
init =
    ({
        fcShapes =
            [ ({id=1, shapeType=Start, x=40,y=90,text="abc",title="1"})
            , ({id=2, shapeType=Action, x=40,y=490,text="def",title="tre2"})]
            --, ({id=3, shapeType=End, x=150,y=290,text="",title="j23j"})]
        , fcArrows =
            [ ({id=1, startPos= Offset (1, 10, 40), endPos=Offset (2, 50, 5), title="1"})
            , ({id=2, startPos=Global (190, 50), endPos=Global (800, 500), title="ijs2"})]
        , debugMsg = ""
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElement=1
        , currentLine=Nothing
        , displayedDivId=1
    }, Cmd.none)

-- UPDATE



getShapeWithId : Model -> Id -> Maybe FcShape
getShapeWithId model id = 
    head <| List.filter (\el -> el.id == id) model.fcShapes

    

findFreeId : List {a | id:Id} -> Id
findFreeId l = 
    List.foldl (\el state -> if el.id >= state then el.id + 1 else state) 1 l


removeShape : Model -> Id -> Model
removeShape model id =
    let mshape = getShapeWithId model id
    in
    case mshape of
        Nothing -> model
        Just shape ->
            let newShapes = List.foldr (\el state -> if el.id == id then state else (el::state)) [] model.fcShapes
                newArrows = List.map (\ar ->
                    let newStartPos = 
                        case ar.startPos of
                            Offset (id, x, y) -> if (id == shape.id) then Global (x + shape.x, y + shape.y) else ar.startPos
                            _ -> ar.startPos
                        newEndPos = 
                        case ar.endPos of
                            Offset (id, x, y) -> if (id == shape.id) then Global (x + shape.x, y + shape.y) else ar.endPos
                            _ -> ar.endPos
                    in
                        { ar | startPos = newStartPos, endPos = newEndPos }) model.fcArrows
            in
                { model | fcShapes=newShapes, fcArrows=newArrows }





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
                       , ("title", Json.Encode.string (toString a.id)) ]
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










update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownMsg { areaType, id} ->
            case areaType of
                Inner -> {model | dragElement=(Just {areaType=areaType, id=id}), selectedElement=id, currentLine=Nothing } ! []
                Outer -> {model | dragElement=(Just {areaType=areaType, id=id}), selectedElement=id, currentLine=Just (Offset (id, 0, 0), Offset (id, 0, 0)) } ! []
        MouseDown pos->
            let id = 
                    case model.dragElement of
                        Nothing -> Nothing
                        (Just  ({areaType, id})) -> Just id
                shapePos = Maybe.map (getShapeWithId model) id
            in
                case shapePos of
                    Just (Just el) ->
                        let offsetX=(toFloat pos.x)-el.x
                            offsetY=(toFloat pos.y)-el.y
                        in
                        { model | dragOffsetX=offsetX
                                , dragOffsetY=offsetY
                                , currentLine=Maybe.map (\(s, e) -> 
                                                    case s of 
                                                        Offset (id, x, y) -> (Offset (id, offsetX, offsetY), Offset (id, offsetX, offsetY))
                                                        _ -> (s, e))
                                     model.currentLine } ! []
                    _ ->
                        --Debug.log "soll das so sein?"
                        model ! []
        KeyMsg code ->
            case code of 
                65 -> {model | debugMsg = "a pressed", fcShapes = model.fcShapes ++ [ (createStartElement (findFreeId model.fcShapes) 400.0 400.0)]} ! []
                66 ->
                    let x = loadElements |> perform (\a -> HttpFailure (toString a)) (\a -> HttpSuccess a)
                    in
                        case x of
                            _ -> {model | debugMsg="b" ++ toString x} ! [x]
                67 ->
                    let x = (saveElements model |> perform (\a -> HttpFailure (toString a)) (\a -> HttpFailure (toString a)))
                    in
                        {model | debugMsg="jkas"} ! [x]
                46 -> (removeShape model model.selectedElement) ! []
                c -> { model | debugMsg="kein event" ++ (toString c) } ! []
        HttpSuccess s -> {model | fcShapes=(fst s), fcArrows=(snd s)} ! []
        HttpFailure s -> { model | debugMsg="HttpFailure " ++ s } ! []
        TitleChanged s ->
            let m = { model | fcShapes = List.map (\el -> if el.id == model.selectedElement then {el | title = s} else el) model.fcShapes}
            in
                m ! []
        TextChanged s ->
            let m = { model | fcShapes = List.map (\el -> if el.id == model.selectedElement then {el | text = s} else el) model.fcShapes}
            in
                m ! []

        UpMsg { areaType, id} ->
            {model |dragElement=Nothing, currentLine=Maybe.map (\l -> (fst l, Offset (id, 0, 0))) model.currentLine } ! []
        UpdateView -> model ! []
        DisplayDiv id ->
            { model |  displayedDivId=id } ! []
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
                                    if (isSameElement a.startPos a.endPos) then [] else [{id=elid, startPos=a.startPos, endPos=Offset (id, offsetX, offsetY), title=toString elid}]
                            _ -> []
            in
                let newModel = {model | dragElement=Nothing, fcArrows=(h++model.fcArrows), currentLine=Nothing }
                in 
                    newModel ! []
        MouseMove pos ->
            case model.dragElement of
                Nothing -> model ! []
                Just {areaType, id} -> 
                    case areaType of
                        Inner ->
                            let l = Debug.log "offsetX " model.dragOffsetX
                                m = moveElementTo model model.dragElement (toFloat pos.x-l) (toFloat pos.y-model.dragOffsetY)
                            in
                                m ! []
                        Outer ->
                            let element = getShapeWithId model id
                                m = 
                                    case element of
                                        Nothing -> model
                                        Just el -> 
                                            { model | currentLine=(Maybe.map (\(startPos, _) -> (startPos, Global (toFloat pos.x, toFloat pos.y))) model.currentLine)}
                            in
                                m ! []


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



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.downs MouseDown, Mouse.moves MouseMove, Mouse.ups MouseUp]



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


createHtml : Model -> Id -> List (Html Msg)
createHtml model id =
    let mshape = getShapeWithId model id
    in
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
                                [Html.p
                                    [Html.Attributes.id (toString id)]
                                    [Html.a [Html.Attributes.href "#", Html.Events.onClick (DisplayDiv ((Maybe.withDefault 0 (head newIds))))] [text shape.text]]]
                            _ -> 
                                (Html.p
                                    [Html.Attributes.id (toString id)] [text shape.text])::
                                    (List.foldl (\el state ->
                                        ((Html.p [Html.Attributes.id (toString el)] (createHtml { model | fcArrows = List.filter (\x -> not (x `List.member` arrows)) model.fcArrows } el))::state)) [] newIds)


-- VIEW


view : Model -> Html Msg
view model =
    let cur = 
        case model.currentLine of
            Nothing -> []
            Just (startPos, endPos)-> [fcArrowToSvg model {id=-1, startPos=startPos, endPos=endPos, title="no title"}]
    in
        Html.div [Html.Attributes.style [("width", "100%"), ("height", "100%")]]
        [ Html.div [ Html.Attributes.tabindex 0, Html.Attributes.autofocus True, Html.Events.on "keydown" (Html.Events.keyCode |> Json.map (\x -> KeyMsg (Debug.log "ev : " x))), Html.Attributes.style [("width", "70%"), ("height", "100%"),  ("overflow", "scroll"), ("backgroundColor", "yellow")]] [
            svg [ Svg.Attributes.style "background-color:lightblue", viewBox "0 0 8500 8500", width "8500", height "8500",  pointerEvents "none"]
                (([defs []
                    [ marker [id "arrowHead", markerWidth "15", markerHeight "10", viewBox "-6, -6, 12, 12", refX "5", refY "0", orient "auto"]
                        [ g []
                            [ polygon [points "-2,0 -5,5 5,0 -5,-5", fill "red", stroke "black", strokeWidth "1px" ] []]
                        --    , rect [ y "-24", x "-60", width "400", height "140", Svg.Attributes.style "fill:rgb(144, 0, 144);stroke-width:3;stroke:rgb(0, 40, 150)"] []
                        ]
                    , marker [id "arrowHeadRotated", markerWidth "15", markerHeight "10", viewBox "-6, -6, 12, 12", refX "5", refY "0", orient "auto-start-reverse"]
                            [ polygon [points "-2,0 -5,5 5,0 -5,-5", fill "red", stroke "black", strokeWidth "1px" ] []]
                    , marker [id "arrowCaption", markerWidth "600", viewBox "-300, -120, 600, 120", markerHeight "120", refX "50", refY "0", orient "auto"]
                                [ text' [ Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;", fontSize (toString myFontSize), fontFamily myFontFamily, x (toString -50), y (toString -10), fill "green"] [Svg.text "Hallo Welt"]]
                    , marker [id "arrowCaptionRotated", markerWidth "180", viewBox "-40, -60, 420, 120", markerHeight "52", refX "50", refY "0", orient "auto-start-reverse"]
                            [text' [Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;", fontSize (toString myFontSize), fontFamily myFontFamily, x (toString -50), y (toString -10), fill "green"] [Svg.text "Hallo Welt"]]]
                          
                ]) ++
                (List.map (fcShapeToSvg model) model.fcShapes ++
                 List.map (fcArrowToSvg model) model.fcArrows ++
                 cur))] 
        , Html.div [ Html.Attributes.style
            [ ("position", "fixed")
            , ("top", "0px")
            , ("right", "0px")
            , ("width", "30%")
            , ("height", "100%")
            , ("backgroundColor", "lightGray")]]
            [Html.table [Html.Attributes.style [("width", "100%")]]
                [ Html.tr []
                    [ Html.td []
                        [ text "Titel"
                        , Html.input [ Html.Attributes.value (Maybe.withDefault "hal" (Maybe.map (\x -> x.title) (getShapeWithId model (model.selectedElement)))), Html.Events.onInput TitleChanged][]]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ text "Text"
                        , Html.textarea [ Html.Attributes.rows 20, Html.Attributes.style [("width", "70%")], Html.Attributes.value (Maybe.withDefault "hal" (Maybe.map (\x -> x.text) (getShapeWithId model (model.selectedElement)))), Html.Events.onInput TextChanged][]]
                    ]
                ]
            --, Html.div [Html.Attributes.style [("minWidth", "50%")]] [Html.a [Html.Attributes.href "http://localhost"] [text "hallo" ]]
            , Html.div [Html.Attributes.style [("minWidth", "50%")]] (createHtml model model.displayedDivId)
            ]
        ]



fcArrowToSvg : Model -> FcArrow -> Svg.Svg Msg
fcArrowToSvg model {id, startPos, endPos, title} = 
    let (startX, startY) = 
        case startPos of
            Global (x, y) -> (x, y)
            Offset (id, x, y) ->
                let el = getShapeWithId model id
                in
                    case el of
                        Nothing -> (0, 0)
                        Just e -> (x + e.x, y + e.y)
        (endX, endY) = 
            case endPos of
                Global (x, y) -> (x, y)
                Offset (id, x, y) ->
                    let el = getShapeWithId model id
                    in
                        case el of
                            Nothing -> (0, 0)
                            Just e -> (x + e.x, y + e.y)
    in
        if endX > startX
        then 
            Svg.path [
                    d ( "M " ++ (toString startX) ++ ", " ++ (toString startY) ++
                        " L " ++ (toString ((endX-startX)/2+startX)) ++ ", " ++ (toString ((endY-startY)/2+startY)) ++
                        " L " ++ (toString endX) ++ ", " ++ (toString endY)), markerEnd "url(#arrowHead)", markerMid "url(#arrowCaption)", Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"] []
        else
            Svg.path [
                    d ( "M " ++ (toString endX) ++ ", " ++ (toString endY) ++
                        " L " ++ (toString ((startX-endX)/2+endX)) ++ ", " ++ (toString ((startY-endY)/2+endY)) ++
                        " L " ++ (toString startX) ++ ", " ++ (toString startY)), markerStart "url(#arrowHeadRotated)", markerMid "url(#arrowCaptionRotated)", Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"] []
                          
                         


fcShapeToSvg : Model -> FcShape -> Svg.Svg Msg
fcShapeToSvg model fcShape = 
    let outerColor = "#FFF9CE"
        innerColor = "blue"
        strokeColor = if model.selectedElement == fcShape.id then "red" else outerColor
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











