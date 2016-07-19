module FcGraphics exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import String
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Encode exposing (..)
import Json.Decode as Json
import Json.Decode exposing ((:=))

import FcTypes exposing (..)
import FcElement exposing (..)
import Tests exposing (..)

myFontSize = 14
myFontFamily = "Courier"
innerPadding = 10.0
outerPadding = 15.0

view : Model -> Html Msg
view model =
    let cur = 
        case model.currentLine of
            Nothing -> []
            Just (startPos, endPos)-> [fcArrowToSvg model {id=-1, startPos=startPos, endPos=endPos, title="no title"}]
    in
        Html.div [Html.Attributes.style [("width", "100%"), ("height", "100%")]]
        [ Html.div [ Html.Attributes.id "mainEl"
                   , Html.Attributes.tabindex 0
                   , Html.Events.on "keydown" (Html.Events.keyCode |> Json.map (\keyCode -> KeyMsg keyCode))
                   , Html.Events.on "scroll" (Json.succeed (SetScrollPosition "mainEl"))
                   , Html.Attributes.style [("width", "70%"), ("height", "80%"),  ("overflow", "scroll"), ("backgroundColor", "yellow")]] [
        Html.div
            [
                Html.Attributes.style
                [ ("position", "fixed")
                , ("bottom", "0px")
                , ("left", "0px")
                , ("width", "70%")
                , ("height", "20%")
                , ("backgroundColor", "yellow")
                ]]
            [Html.textarea
                [ Html.Attributes.value getTestResults
                , Html.Attributes.style
                    [ ("width", "100%")
                    , ("height", "100%")
                    , ("overflow", "scroll")
                    ]
                ]
                []
            ]
            , svg [ Svg.Attributes.style "background-color:lightblue", viewBox "0 0 8500 8500", width "8500", height "8500", pointerEvents "none"]
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
        , Html.div [
            Html.Attributes.style
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
                        , Html.input [ Html.Attributes.value (Maybe.withDefault "---" (Maybe.map (\x -> 
                                case x of
                                    Just (ShapeElement sh) ->
                                        sh.title
                                    Just (ArrowElement ar) ->
                                        ar.title
                                    _ -> "") (Maybe.map (getElementWithId model) (model.selectedElement)))), Html.Events.onInput TitleChanged][]]
                    ]
                , Html.tr []
                    [ Html.td []
                        [ Html.text "Text"
                        , Html.textarea [ Html.Attributes.rows 20, Html.Attributes.style [("width", "70%")]
                        , Html.Attributes.value (Maybe.withDefault "---" (Maybe.map (\x -> 
                                case x of
                                    Just (ShapeElement sh) ->
                                        sh.text
                                    Just (ArrowElement ar) ->
                                        "Pfeile haben keinen Text"
                                    _ -> "") (Maybe.map (getElementWithId model) (model.selectedElement)))), Html.Events.onInput TextChanged][]]
                    ]
                ]
            , Html.div [Html.Attributes.style [("minWidth", "50%")]] (createHtml model model.displayedDivId)
            ]
        ]

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
                            (Html.p
                                [Html.Attributes.id (toString id)]
                                [text shape.text])::
                            (List.foldl
                                (\el state ->
                                    case el.endPos of
                                        Offset (endId, _, _) ->
                                            Html.p [] [Html.a [Html.Attributes.href "#", Html.Events.onClick (DisplayDiv endId)] [Html.text el.title]]::state
                                        _ -> state
                                ) [] arrows
                            )
                        _ -> 
                            (Html.p
                                [Html.Attributes.id (toString id)] [text shape.text])::
                            (createHtml model (Maybe.withDefault -1 (List.head newIds)))


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
        innerColor = "yellow"
        strokeColor =
            if model.selectedElement == Just (FcShapeId fcShape.id) then "red" else outerColor
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
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; ms-user-select: none; khtml-user-select: none;"
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
        "Courier" -> ((toFloat (fontSize * (String.length text))) * 0.6, toFloat fontSize)
        _ -> ((toFloat (fontSize * (String.length text))) * 0.6, toFloat fontSize)

