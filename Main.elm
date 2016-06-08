import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug as Debug
import Svg.Events exposing (onMouseDown, onClick)
import Mouse
import Keyboard
import VirtualDom
import Json.Decode exposing ((:=), int, string)
import Json.Encode exposing (..)
import String exposing (length)

myFontSize = 20
myFontFamily = "Courier"
innerPadding =
    { left = 20
    , bottom = 40
    , right = 80
    , top = 160 }

outerPadding =
    { left = 40
    , bottom = 30
    , right = 20
    , top = 10 }

type alias InnerPadding = 
    { left : Int
    , bottom : Int
    , right : Int
    , top : Int }

type alias OuterPadding = 
    { left : Int
    , bottom : Int
    , right : Int
    , top : Int }

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
    , x : Int
    , y : Int
    , text : String
    , title : String }

type FcPos = 
      Offset (FcElement, Int, Int)
    | Global (Int, Int)

type FcElement = 
      FcShapeElement FcShape
    | FcArrowElement FcArrow

type alias FcArrow = 
    { id : Int
    , pos : FcPos }




-- MODEL


type alias Model =
    { fcElements : List FcElement
    , debugMsg : String }


init : (Model, Cmd Msg)
init =
  ({fcElements = [], debugMsg = ""}, Cmd.none)



-- UPDATE


type alias Id = Int

type Msg =
      MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | Inner Id
    | Outer Id


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMsg position -> {model | debugMsg = ("mouseMsg" ++ (toString position))} ! []
    KeyMsg code -> {model | debugMsg = "keyMsg"} ! []
    Inner id -> {model | debugMsg = "Inner " ++ (toString id)} ! []
    Outer id -> {model | debugMsg = "Outer " ++ (toString id)} ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
   -- Sub.batch [ Keyboard.presses KeyMsg]
    Sub.batch [ Mouse.downs MouseMsg]



-- VIEW


view : Model -> Html Msg
view model =
    Debug.log model.debugMsg
    svg [ viewBox "0 0 500 500", width "500"]
      [  fcShapeToSvg (Start {id=0,x=0,y=0,text="",title="Start"}),
         fcShapeToSvg (Output {id=2,x=170, y=170, text="ABC", title="abc"})
      , line [ onMouseDown (Inner 3), x1 "50", y1 "50", x2 "69", y2 "90", stroke "#023963" ] []
      ]


fcShapeToSvg : FcShape -> Svg.Svg Msg
fcShapeToSvg fcShape = 
    case fcShape of
        Start v ->
            let (textWidth, textHeight) = getTextDimension v.title "Courier" 20
                innerShapeWidth = textWidth + innerPadding.left + innerPadding.right
                innerShapeHeight = textHeight + innerPadding.bottom + innerPadding.top
                outerShapeWidth = innerShapeWidth + outerPadding.left + outerPadding.right
                outerShapeHeight = innerShapeHeight + outerPadding.bottom + outerPadding.top
            in
                Debug.log ((toString textWidth) ++ (toString textHeight) ++ ", " ++ (toString outerShapeWidth))
                g [][
                    rect [ onMouseDown (Outer 31)
                         , x (toString v.x)
                         , y (toString <| v.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "15"
                         , ry "10"
                         , stroke "red"
                         , strokeDasharray "10,10"
                         , fill "#FFF9CE" ] [],
                    rect [ onMouseDown (Inner 31)
                         , x (toString (v.x + outerPadding.left))
                         , y (toString (v.y + outerPadding.top))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "15"
                         , ry "10"
                         , fill "#0B79CE" ] [],
                text' [ onMouseDown (Inner 31)
                      , fontFamily myFontFamily
                      ,  fontSize (toString myFontSize)
                      ,  Svg.Attributes.cursor "default"
                      , x (toString (v.x + outerPadding.left + innerPadding.left))
                      , y (toString (v.y+outerPadding.top + innerPadding.top + (round ((toFloat textHeight) / 2)) + (round ((toFloat myFontSize) / 3))))
                      , fill "red" ] [Svg.text v.title]]
        Action v ->
            circle [ onMouseDown (Inner v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        End v ->
            circle [ onMouseDown (Inner v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        Condition v ->
            circle [ onMouseDown (Inner v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        Output v ->
            let t = text' [ Svg.Attributes.title "sdaf", x (toString v.x), y (toString v.y), fill "red" ] [Svg.text "hasd"]
            in
                t



getTextDimension : String -> String -> Int -> (Int, Int)
getTextDimension text  font  fontSize = 
    case font of
        "Courier" -> (round ((toFloat (fontSize * (length text))) * 0.6), fontSize)
        _ -> (round ((toFloat (fontSize * (length text))) * 0.6), fontSize)


















