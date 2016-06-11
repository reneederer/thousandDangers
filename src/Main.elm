module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug as Debug
import Svg.Events exposing (onMouseUp, onMouseDown, onClick, onMouseMove)
import Mouse
import Keyboard
import VirtualDom exposing (onWithOptions)
import Json.Encode exposing (..)
import Json.Decode as Json exposing(..)
import String exposing (length)
import Char exposing (toCode, fromCode)
import Maybe exposing (withDefault)
import List exposing (head)

myFontSize = 20
myFontFamily = "Courier"
innerPadding = 20.0

outerPadding = 30.0

paralleloGrammShift = 40

options = {preventDefault=True, stopPropagation=False}

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
    | Output
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
    , endPos : FcPos }

type alias Id = Int

createStartElement :Id ->  Float -> Float -> FcShape
createStartElement id x y = 
        { id = id
        , shapeType = Start
        , x = x
        , y = y
        , text = "Start"
        , title = "Start"
        }

createEndElement : Id -> Float -> Float -> FcShape
createEndElement id x y = 
        { id = id
        , shapeType = End
        , x = x
        , y = y
        , text = "Ende"
        , title = "Ende"
        }

createConditionElement : Id -> Float -> Float -> FcShape
createConditionElement id x y =
        { id = id
        , shapeType = Condition
        , x = x
        , y = y
        , text = "Condition"
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

-- MODEL


type alias Model =
    { fcShapes : List FcShape
    , fcArrows : List FcArrow
    , debugMsg : String
    , dragElement : Maybe ShapeArea
    , dragOffsetX : Float
    , dragOffsetY : Float
    , selectedElement : Id
    , currentLine : Maybe (FcPos, FcPos)}


init : (Model, Cmd Msg)
init =
    ({
        fcShapes =
            [ ({id=1, shapeType=Start, x=40,y=90,text="",title="Start"})
            , ({id=2, shapeType=Action, x=40,y=490,text="",title="Action"})
            , ({id=3, shapeType=End, x=150,y=290,text="",title="End"})]
        , fcArrows =
            [ ({id=4, startPos= Offset (1, 10, 40), endPos=Offset (2, 50, 5)})
            , ({id=5, startPos=Global (190, 50), endPos=Global (800, 500)}) ]
        , debugMsg = ""
        , dragElement=Nothing
        , dragOffsetX=0
        , dragOffsetY=0
        , selectedElement=1
        , currentLine=Nothing
    }, Cmd.none)

-- UPDATE



getElementWithId : Model -> Id -> Maybe FcShape
getElementWithId model id = 
    head <| List.filter (\el -> el.id == id) model.fcShapes

    

findFreeId : Model -> Id
findFreeId model = 
    let minShapes = List.foldl (\el state -> if el.id >= state then el.id + 1 else state) 1 model.fcShapes
        minArrows = List.foldl (\el state -> if el.id >= state then el.id + 1 else state) minShapes model.fcArrows
    in
        minArrows

removeElement : Model -> Id -> Model
removeElement model id =
    let newShapes = List.foldr (\el state -> if el.id == id then state else (el::state)) [] model.fcShapes
        newArrows = List.foldr (\el state -> if el.id == id then state else (el::state)) [] model.fcArrows
    in
        { model | fcShapes=newShapes, fcArrows=newArrows }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownMsg { areaType, id} ->
            {model | dragElement=(Just {areaType=areaType, id=id}), selectedElement=id, currentLine=Just (Offset (id, 0, 0), Offset (id, 0, 0)) } ! []
        MouseDown pos->
            let id = 
                    case model.dragElement of
                        Nothing -> Nothing
                        (Just  ({areaType, id})) -> Just id
                shapePos = Maybe.map (getElementWithId model) id
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
                        Debug.log "soll das so sein?"
                        model ! []
        KeyMsg code ->
            case code of 
                97 -> {model | debugMsg = "a pressed", fcShapes = model.fcShapes ++ [ (createStartElement (findFreeId model) 400.0 400.0)]} ! []
                127 -> (removeElement model model.selectedElement) ! []
                _ -> model ! []
        UpMsg { areaType, id} ->
            {model |dragElement=Nothing, currentLine=Maybe.map (\l -> (fst l, Offset (id, 0, 0))) model.currentLine } ! []
        MouseUp pos ->
            let arrow = Maybe.map (\l -> { id=findFreeId model, startPos=(fst l), endPos=(snd l) }) model.currentLine
                h = case arrow of 
                    Nothing -> []
                    Just a ->
                        case a.endPos of
                            Offset (id, _, _) ->
                                let element = getElementWithId model id
                                    (offsetX, offsetY) = 
                                        case element of
                                            Nothing -> (0.0, 0.0)
                                            Just el -> (toFloat pos.x - el.x, toFloat pos.y - el.y)
                                in
                                    if (isSameElement a.startPos a.endPos) then [] else [{id= a.id, startPos=a.startPos, endPos=Offset (id, offsetX, offsetY)}]
                            _ -> []
            in
                {model | dragElement=Nothing, fcArrows=(h++model.fcArrows), currentLine=Nothing } ! []
        MouseMove pos ->
            case model.dragElement of
                Nothing -> model ! []
                Just {areaType, id} -> 
                    case areaType of
                        Inner ->
                            let m = moveElementTo model model.dragElement (toFloat pos.x-model.dragOffsetX) (toFloat pos.y-model.dragOffsetY)
                            in
                                m ! []
                        Outer ->
                            let element = getElementWithId model id
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
    Sub.batch [ Keyboard.presses KeyMsg, Mouse.downs MouseDown, Mouse.moves MouseMove, Mouse.ups MouseUp]



-- VIEW


view : Model -> Html Msg
view model =
    let cur = 
        case model.currentLine of
            Nothing -> []
            Just (startPos, endPos)-> [fcArrowToSvg model {id=-1, startPos=startPos, endPos=endPos}]
    in
    Debug.log model.debugMsg
    svg [ viewBox "0 0 1500 1500", width "1500",  pointerEvents "none"]
        (List.map (fcShapeToSvg model) model.fcShapes ++
         List.map (fcArrowToSvg model) model.fcArrows ++
         cur)



fcArrowToSvg : Model -> FcArrow -> Svg.Svg Msg
fcArrowToSvg model {id, startPos, endPos} = 
    let (startX, startY) = 
        case startPos of
            Global (x, y) -> (x, y)
            Offset (id, x, y) ->
                let el = getElementWithId model id
                in
                    case el of
                        Nothing -> (0, 0)
                        Just e -> (x + e.x, y + e.y)
        (endX, endY) = 
            case endPos of
                Global (x, y) -> (x, y)
                Offset (id, x, y) ->
                    let el = getElementWithId model id
                    in
                        case el of
                            Nothing -> (0, 0)
                            Just e -> (x + e.x, y + e.y)
    in
        line [x1 (toString <| startX), y1 (toString <| startY), x2 (toString <| endX), y2 (toString <| endY), Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2", markerEnd "url(#triangle)"] []


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
                circle [ pointerEvents "all"] []
            --circle [ onMouseDown (Inner fcShape.id), cx (toString fcShape.x), cy (toString fcShape.y), r "45", fill "#0B79CE" ] []
        Condition ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
                innerStartX = fcShape.x + outerPadding -- - smallAmount
                innerStartY = fcShape.y + outerPadding
            in
                g [pointerEvents "all"] [
                    polygon[ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                         , points ((toString <| fcShape.x) ++ "," ++ (toString <| fcShape.y + outerShapeHeight/2) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth/2)) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth)) ++ "," ++ (toString (fcShape.y + outerShapeHeight/2)) ++ " "
                                ++ (toString <| fcShape.x + outerShapeWidth / 2) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " ")

                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    polygon [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                         , points ((toString <| innerStartX) ++ "," ++ (toString <| innerStartY + innerShapeHeight/2) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth/2)) ++ "," ++ (toString <| innerStartY) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth)) ++ "," ++ (toString (innerStartY + innerShapeHeight/2)) ++ " "
                                ++ (toString <| innerStartX + innerShapeWidth / 2) ++ "," ++ (toString (innerStartY + innerShapeHeight)) ++ " ")

                         , stroke "blue"
                         , fill "#FFF9CE" ] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill "red" ] [Svg.text fcShape.title]]
        Output ->
            let (textWidth, textHeight) = getTextDimension fcShape.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
                innerStartX = fcShape.x + innerPadding -- - smallAmount
                innerStartY = fcShape.y + innerPadding
            in
                g [ pointerEvents "all"] [
                    polygon[ onMouseDown (DownMsg {areaType=Outer, id=fcShape.id})
                         , points ((toString <| fcShape.x) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth)) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x - paralleloGrammShift + outerShapeWidth)) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " "
                                ++ (toString <| fcShape.x - paralleloGrammShift) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " ")

                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    polygon[ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                         , points ((toString <| fcShape.x ) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth)) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x - paralleloGrammShift + outerShapeWidth)) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " "
                                ++ (toString <| fcShape.x - paralleloGrammShift) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " ")
                         , stroke "blue"
                         , fill "#FFF9CE" ] [],
                    text' [ onMouseDown (DownMsg {areaType=Inner, id=fcShape.id})
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + outerPadding + innerPadding))
                          , y (toString (fcShape.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill "red" ] [Svg.text fcShape.title]]


getTextDimension : String -> String -> Int -> (Float,Float)
getTextDimension text  font  fontSize = 
    case font of
        "Courier" -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)
        _ -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)


















