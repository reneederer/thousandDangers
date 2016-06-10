module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug as Debug
import Svg.Events exposing (onMouseUp, onMouseDown, onClick, onMouseMove)
import Mouse
import Keyboard
import VirtualDom
import Json.Encode exposing (..)
import String exposing (length)

myFontSize = 20
myFontFamily = "Courier"
innerPadding = 80.0

outerPadding = 90.0

paralleloGrammShift = 40

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type FcShape = 
      Start FcShapeAttributes
    | End FcShapeAttributes
    | Condition FcShapeAttributes
    | Output FcShapeAttributes
    | Action FcShapeAttributes
    

type alias FcShapeAttributes =
    { id : Int
    , x : Float
    , y : Float
    , text : String
    , title : String }

type FcPos = 
      Offset (FcElement, Float, Float)
    | Global ( Float, Float)

type FcElement = 
      FcShapeElement FcShape
    | FcArrowElement FcArrow

type alias FcArrow = 
    { id : Int
    , pos : FcPos }


newStart : Float -> Float -> FcElement
newStart x y = 
    FcShapeElement <| Start
        { id = 9
        , x = x
        , y = y
        , text = "Start"
        , title = "Start1"
        }



-- MODEL


type alias Model =
    { fcElements : List FcElement
    , debugMsg : String
    , dragElementId : Maybe Int }


init : (Model, Cmd Msg)
init =
  ({fcElements = [ (FcShapeElement <| Start {id=1 ,x=40,y=90,text="",title="Start"})], debugMsg = "", dragElementId = Just 33}, Cmd.none)

      --[ fcShapeToSvg (FcShapeElement <| Start {id=1 ,x=40,y=90,text="",title="Start"})]
      --, fcShapeToSvg (Output {id=2, x=170, y=170, text="ABC", title="abcdefghijkl"})
      --, fcShapeToSvg (Action {id=3, x=500, y=270, text="ABC", title="abcdefghijkl"})
      --, fcShapeToSvg (Condition {id=2, x=770, y=170, text="ABC", title="123456897890132abcdefghijkl"})
      --, line [ onMouseDown (Inner 3), x1 "750", y1 "50", x2 "69", y2 "90", stroke "#023963" ] []


-- UPDATE


type alias Id = Int

type Msg =
      MouseMsg Mouse.Position
    | MouseUp Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | Inner Id
    | Outer Id
    | Move Id


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMsg position -> {model | debugMsg = ("mouseMsg" ++ (toString position))} ! []
    KeyMsg code -> {model | debugMsg = "keyMsg"} ! []
    Inner id -> {model | debugMsg = "Inner " ++ (toString id)} ! []
    Outer id -> {model | debugMsg = "Outer " ++ (toString id)} ! []
    Move  id -> model ! []
    MouseUp pos ->
        {model | debugMsg = "mouseUp", fcElements = model.fcElements ++ [newStart (toFloat pos.x) (toFloat pos.y)]} ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.presses KeyMsg, Mouse.downs MouseMsg, Mouse.moves MouseMsg, Mouse.ups MouseUp]



-- VIEW


view : Model -> Html Msg
view model =
    Debug.log model.debugMsg
    svg [ viewBox "0 0 1500 1500", width "1500"]
        (List.map fcShapeToSvg model.fcElements)


fcShapeToSvg : FcElement -> Svg.Svg Msg
fcShapeToSvg fcElement = 
    case fcElement of
        FcShapeElement (Start v) ->
            let (textWidth, textHeight) = getTextDimension v.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
            in
                g [][
                    rect [ onMouseDown (Outer 31)
                         , onMouseUp (Outer 922)
                         , onMouseMove (Move 34)
                         , x (toString v.x)
                         , y (toString <| v.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "30"
                         , ry "30"
                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    rect [ onMouseDown (Inner 31)
                         , x (toString (v.x + outerPadding))
                         , y (toString (v.y + outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "25"
                         , ry "25"
                         , fill "#0B79CE" ] [],
                    text' [ onMouseDown (Inner 31)
                          , pointerEvents "none"
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (v.x + outerPadding + innerPadding))
                          , y (toString (v.y+outerPadding + innerPadding + textHeight / 2 + myFontSize/3))
                          , fill "red" ] [Svg.text v.title]]
        FcShapeElement (Action v) ->
            let (textWidth, textHeight) = getTextDimension v.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
            in
                g [] [
                    rect [ onMouseDown (Outer v.id)
                         , x (toString v.x)
                         , y (toString <| v.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    rect [ onMouseDown (Inner 31)
                         , x (toString (v.x + outerPadding))
                         , y (toString (v.y + outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         , fill "#0B79CE" ] [],
                    text' [ onMouseDown (Inner v.id)
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (v.x + outerPadding + innerPadding))
                          , y (toString (v.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill "red" ] [Svg.text v.title]]
        FcShapeElement (End v) ->
            circle [ onMouseDown (Inner v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        FcShapeElement (Condition v) ->
            let (textWidth, textHeight) = getTextDimension v.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
                innerStartX = v.x + outerPadding -- - smallAmount
                innerStartY = v.y + outerPadding
            in
                g [][
                    polygon[ onMouseDown (Outer 32)
                         , points ((toString <| v.x) ++ "," ++ (toString <| v.y + outerShapeHeight/2) ++ " "
                                ++ (toString (v.x + outerShapeWidth/2)) ++ "," ++ (toString v.y) ++ " "
                                ++ (toString (v.x + outerShapeWidth)) ++ "," ++ (toString (v.y + outerShapeHeight/2)) ++ " "
                                ++ (toString <| v.x + outerShapeWidth / 2) ++ "," ++ (toString (v.y + outerShapeHeight)) ++ " ")

                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    polygon [ onMouseDown (Inner 32)
                         , points ((toString <| innerStartX) ++ "," ++ (toString <| innerStartY + innerShapeHeight/2) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth/2)) ++ "," ++ (toString <| innerStartY) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth)) ++ "," ++ (toString (innerStartY + innerShapeHeight/2)) ++ " "
                                ++ (toString <| innerStartX + innerShapeWidth / 2) ++ "," ++ (toString (innerStartY + innerShapeHeight)) ++ " ")

                         , stroke "blue"
                         , fill "#FFF9CE" ] [],
                    text' [ onMouseDown (Inner 31)
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (v.x + outerPadding + innerPadding))
                          , y (toString (v.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill "red" ] [Svg.text v.title]]
        FcShapeElement (Output v) ->
            let (textWidth, textHeight) = getTextDimension v.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding + innerPadding
                innerShapeHeight = textHeight + innerPadding + innerPadding
                outerShapeWidth = innerShapeWidth + outerPadding + outerPadding
                outerShapeHeight = innerShapeHeight + outerPadding + outerPadding
                innerStartX = v.x + innerPadding -- - smallAmount
                innerStartY = v.y + innerPadding
            in
                g [][
                    polygon[ onMouseDown (Outer 32)
                         , points ((toString <| v.x) ++ "," ++ (toString v.y) ++ " "
                                ++ (toString (v.x + outerShapeWidth)) ++ "," ++ (toString v.y) ++ " "
                                ++ (toString (v.x - paralleloGrammShift + outerShapeWidth)) ++ "," ++ (toString (v.y + outerShapeHeight)) ++ " "
                                ++ (toString <| v.x - paralleloGrammShift) ++ "," ++ (toString (v.y + outerShapeHeight)) ++ " ")

                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    polygon[ onMouseDown (Inner 32)
                         , points ((toString <| v.x ) ++ "," ++ (toString v.y) ++ " "
                                ++ (toString (v.x + outerShapeWidth)) ++ "," ++ (toString v.y) ++ " "
                                ++ (toString (v.x - paralleloGrammShift + outerShapeWidth)) ++ "," ++ (toString (v.y + outerShapeHeight)) ++ " "
                                ++ (toString <| v.x - paralleloGrammShift) ++ "," ++ (toString (v.y + outerShapeHeight)) ++ " ")
                         , stroke "blue"
                         , fill "#FFF9CE" ] [],
                    text' [ onMouseDown (Inner 31)
                          , fontFamily myFontFamily
                          ,  fontSize (toString myFontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (v.x + outerPadding + innerPadding))
                          , y (toString (v.y+outerPadding + innerPadding + textHeight / 2 + myFontSize / 3))
                          , fill "red" ] [Svg.text v.title]]
        FcArrowElement v ->
            g [][]



getTextDimension : String -> String -> Int -> (Float,Float)
getTextDimension text  font  fontSize = 
    case font of
        "Courier" -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)
        _ -> ((toFloat (fontSize * (length text))) * 0.6, toFloat fontSize)


















