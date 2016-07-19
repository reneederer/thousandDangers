module FcTypes exposing (..)

import Keyboard
import Mouse

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
    , title : String }

type alias Id = Int


type FcElement = 
      ShapeElement FcShape
    | ArrowElement FcArrow

type ShapeAreaType = 
      Outer
    | Inner

type FcElementId = 
      FcShapeId Id
    | FcArrowId Id

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
    | DisplayDiv Int
    | SetScrollPosition String
    | ScrollPositionTold Position


type alias Model =
    { fcShapes : List FcShape
    , fcArrows : List FcArrow
    , dragElement : Maybe ShapeArea
    , dragOffsetX : Float
    , dragOffsetY : Float
    , selectedElement : Maybe FcElementId
    , currentLine : Maybe (FcPos, FcPos)
    , displayedDivId : Int
    , mainDivOffset : Position
    }
