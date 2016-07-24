module FcTypes exposing (..)

import Keyboard
import Mouse

type ShapeType = 
      Start
    --| End
    --| Condition
    --| Action

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
    , title : String }

type alias Id = Int


type FcElement = 
      ShapeElement FcShape
    | ArrowElement FcArrow

type AreaType = 
      Outer Id
    | Inner Id
    | ArrowStart Id
    | ArrowEnd Id
    | ArrowMiddle Id

type FcElementId = 
      FcShapeId Id
    | FcArrowId Id

type Msg =
      MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | DownMsg AreaType
    | UpMsg AreaType
    | HttpSuccess (List FcShape, List FcArrow)
    | HttpFailure String
    | TitleChanged String
    | TextChanged String
    | SetScrollPosition String
    | ScrollPositionTold Position
    | CreateNewShape ShapeType

type alias GraphicsSettings = 
    { fontFamily : String
    , fontSize : Float
    , innerPadding : Float
    , outerPadding : Float
    }


type alias Model =
    { fcShapes : List FcShape
    , fcArrows : List FcArrow
    , dragElement : Maybe AreaType
    , dragOffsetX : Float
    , dragOffsetY : Float
    , selectedElementId : Maybe FcElementId
    , currentLine : Maybe (FcPos, FcPos)
    , mainDivOffset : Position
    , graphicsSettings : GraphicsSettings
    }
