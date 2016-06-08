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
    | Select Id


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMsg position -> {model | debugMsg = ("mouseMsg" ++ (toString position))} ! []
    KeyMsg code -> {model | debugMsg = "keyMsg"} ! []
    Select id -> {model | debugMsg = "Selected " ++ (toString id)} ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
--    Sub.batch [ Keyboard.presses KeyMsg]
    Sub.batch [ Mouse.downs MouseMsg]



-- VIEW


view : Model -> Html Msg
view model =
    Debug.log model.debugMsg
    svg [ viewBox "0 0 500 500", width "800px" ]
      [  fcShapeToSvg (Start {id=0,x=30,y=50,text="",title=""}),
         fcShapeToSvg (Output {id=2,x=170, y=170, text="ABC", title="abc"})
      , line [ onMouseDown (Select 3), x1 "50", y1 "50", x2 "69", y2 "90", stroke "#023963" ] []
      ]


fcShapeToSvg : FcShape -> Svg.Svg Msg
fcShapeToSvg fcShape = 
    case fcShape of
        Start v ->
            g [][
                rect [ onMouseDown (Select 31), x (toString <| v.x - 30), y (toString <| v.y - 30), width "190", height "110",  rx "15", ry "10", fill "#FFF9CE" ] [],
                rect [ onMouseDown (Select 32), x (toString v.x), y (toString v.y), width "60", height "80",  rx "15", ry "10", fill "#0B79CE" ] [],
            text' [ Svg.Attributes.title "sdaf", x (toString v.x), y (toString (v.y+30)), fill "red" ] [Svg.text "jkasdfl"]]
        Action v ->
            circle [ onMouseDown (Select v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        End v ->
            circle [ onMouseDown (Select v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        Condition v ->
            circle [ onMouseDown (Select v.id), cx (toString v.x), cy (toString v.y), r "45", fill "#0B79CE" ] []
        Output v ->
            let t = text' [ Svg.Attributes.title "sdaf", x (toString v.x), y (toString v.y), fill "red" ] [Svg.text "hasd"]
            in
                t





















