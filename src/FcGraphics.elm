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
                   , Html.Events.on "focus" (Json.succeed (SetScrollPosition "mainEl"))
                   , Html.Attributes.style [("width", "70%"), ("height", "80%"),  ("overflow", "scroll")]] [
        Html.div
            [ Html.Attributes.style
                [ ("position", "fixed")
                , ("bottom", "0px")
                , ("left", "0px")
                , ("width", "70%")
                , ("height", "20%")
                , ("backgroundColor", "yellow")
                ]
            ]
            [ Html.textarea
                [ Html.Attributes.value getTestResults
                , Html.Attributes.style
                    [ ("width", "100%")
                    , ("height", "100%")
                    , ("overflow", "scroll")
                    ]
                ]
                []
            ]
            , svg [ Svg.Attributes.style "background-color:lightblue;", viewBox "0 0 8500 8500", width "8500", height "8500", pointerEvents "none"]
                (([defs []
                    [ marker [id "arrowHead", markerWidth "15", markerHeight "10", viewBox "-6, -6, 12, 12", refX "5", refY "0", orient "auto"]
                        [ g []
                            [ polygon [points "-2,0 -5,5 5,0 -5,-5", fill "red", stroke "black", strokeWidth "1px" ] []]
                        --    , rect [ y "-24", x "-60", width "400", height "140", Svg.Attributes.style "fill:rgb(144, 0, 144);stroke-width:3;stroke:rgb(0, 40, 150)"] []
                        ]
                    ]
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
                                    _ -> "") (Maybe.map (getElementWithId model) (model.selectedElementId)))), Html.Events.onInput TitleChanged][]]
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
                                    _ -> "") (Maybe.map (getElementWithId model) (model.selectedElementId)))), Html.Events.onInput TextChanged][]]
                    ]
                ]
            , Html.div [Html.Attributes.style [("minWidth", "50%")]] ((\x -> case x of
                                                                                 Nothing -> []
                                                                                 Just (FcArrowId id) -> []
                                                                                 Just (FcShapeId id) -> createHtml model id) model.selectedElementId)
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
                                                (Just endShapeId, _, _) -> Just endShapeId
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
                                        (Just endId, _, _) ->
                                            Html.p [] [Html.a [Html.Attributes.href "#", Html.Events.onClick (DownMsg <| Inner endId)] [Html.text el.title]]::state
                                        _ -> state
                                ) [] arrows
                            )
                        _ -> 
                            (Html.p
                                [Html.Attributes.id (toString id)] [text shape.text])::
                            (createHtml model (Maybe.withDefault -1 (List.head newIds)))


fcArrowToSvg : Model -> FcArrow -> Svg.Svg Msg
fcArrowToSvg model {id, startPos, endPos, title} = 
    let (startX1, startY1) = 
        case startPos of
            (Nothing, x, y) -> (x, y)
            (Just id, x, y) ->
                let el = getShapeWithId model id
                in
                    case el of
                        Nothing -> (0, 0)
                        Just e -> (x + e.x, y + e.y)
        (endX1, endY1) = 
            case endPos of
                (Nothing, x, y) -> (x, y)
                (Just id, x, y) ->
                    let el = getShapeWithId model id
                    in
                        case el of
                            Nothing -> (0, 0)
                            Just e -> (x + e.x, y + e.y)
        l = Basics.max 1 (sqrt (((endX1-startX1) ^ 2) + ((endY1-startY1)^2))) / myStrokeWidth
        myStrokeWidth = 1
        distance = 50/myStrokeWidth
        (sx1, sy1) = (startX1 + ((endX1 - startX1)) * ((distance/l)), startY1 + (endY1 - startY1) * ((distance/l)))
        (ex1, ey1) =
            if l <= 2*distance
            then (sx1, sy1)
            else (endX1 - ((endX1 - startX1)) * ((distance/l)), endY1 - (endY1 - startY1) * ((distance/l)))
        (startX, startY, endX, endY, sx, sy, ex, ey) = 
            if endX1 > startX1
            then (startX1, startY1, endX1, endY1, sx1, sy1, ex1, ey1)
            else (startX1, startY1, endX1, endY1, sx1, sy1, ex1, ey1)
        in
            Svg.svg
                []
                [ defs
                    []
                    [ marker
                        [ Svg.Attributes.id <| "arrowCaption" ++ (toString id)
                        , markerWidth "8500"
                        , markerHeight "8500"
                        , markerUnits "userSpaceOnUse"
                        , viewBox "-300 -120 8500 8500"
                        , orient "auto-start-reverse"
                        , refX (model.graphicsSettings.fontSize / 2
                                |> getTextDimension title model.graphicsSettings.fontFamily
                                |> fst
                                |> toString)
                        , refY "5"
                        , orient "auto"
                        ]
                        [ text'
                            [ Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                            , fontSize (toString <| model.graphicsSettings.fontSize)
                            , fontFamily model.graphicsSettings.fontFamily
                            , x "0"
                            , y "0"
                            , fill "black"
                            ]
                            [Svg.text title]
                        ]
                    ]
                , Svg.path
                    [ d ( "M " ++ (toString startX) ++ ", " ++ (toString startY) ++
                          " L " ++ (toString ((endX-startX)/2+startX)) ++ ", " ++ (toString ((endY-startY)/2+startY)) ++
                          " L " ++ (toString endX) ++ ", " ++ (toString endY))
                         , markerEnd "url(#arrowHead)"
                         , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
                    ]
                    []
                , Svg.path
                    [ (if endX > startX
                       then d ( "M " ++ (toString startX) ++ ", " ++ (toString startY) ++
                          " L " ++ (toString ((endX-startX)/2+startX)) ++ ", " ++ (toString ((endY-startY)/2+startY)) ++
                          " L " ++ (toString endX) ++ ", " ++ (toString endY))
                       else d ( "M " ++ (toString endX) ++ ", " ++ (toString endY) ++
                          " L " ++ (toString ((startX-endX)/2+endX)) ++ ", " ++ (toString ((startY-endY)/2+endY)) ++
                          " L " ++ (toString startX) ++ ", " ++ (toString startY)))
                         , markerMid ("url(#arrowCaption" ++ (toString id) ++ ")")
                         , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
                    ]
                    []
                , Svg.path
                    [ pointerEvents (if id == -1 then "none" else "all")
                    , Html.Events.onDoubleClick (DownMsg <| ArrowMiddle id)
                    , d ( "M " ++ (toString sx) ++ ", " ++ (toString sy) ++
                          " L " ++ (toString (ex)) ++ ", " ++ (toString <|ey))
                    , Svg.Attributes.style ("stroke:rgb(0,255,0);stroke-width:" ++ (toString <| 14 * 2) ++ ";stroke-opacity:0.7001")] []
                , Svg.path
                    [ pointerEvents (if id == -1 then "none" else "all")
                    , Html.Events.onDoubleClick (DownMsg <| ArrowStart id)
                    , d ( "M " ++ (toString startX) ++ ", " ++ (toString startY) ++
                          " L " ++ (toString (sx)) ++ ", " ++ (toString <| sy))
                    , Svg.Attributes.style ("stroke:rgb(255,255,0);stroke-width:" ++ (toString <| 14 * 2) ++ ";stroke-opacity:0.7001")] []
                , Svg.path
                    [ pointerEvents (if id == -1 then "none" else "all")
                    , Html.Events.onDoubleClick (DownMsg <| ArrowEnd id)
                    , d ( "M " ++ (toString endX) ++ ", " ++ (toString endY) ++
                          " L " ++ (toString (ex)) ++ ", " ++ (toString <| ey))
                    , Svg.Attributes.style ("stroke:rgb(255,0,0);stroke-width:" ++ (toString <| 14 * 2) ++ ";stroke-opacity:0.7001")] []
                ]

fcShapeToSvg : Model -> FcShape -> Svg.Svg Msg
fcShapeToSvg model fcShape = 
    let outerColor = "#FFF9CE"
        innerColor = "yellow"
        strokeColor =
            if model.selectedElementId == Just (FcShapeId fcShape.id) then "red" else outerColor
        textColor = "red"
        myStrokeDashArray = if model.selectedElementId == Just (FcShapeId fcShape.id) then "10, 10" else "0, 0"
    in
    case fcShape.shapeType of
        Start ->
            let (textWidth, textHeight) = getTextDimension fcShape.title model.graphicsSettings.fontFamily model.graphicsSettings.fontSize
                innerShapeWidth = textWidth + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                innerShapeHeight = textHeight + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                outerShapeWidth = innerShapeWidth + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
                outerShapeHeight = innerShapeHeight + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
            in
                g [ pointerEvents "all"] [
                         
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Outer fcShape.id))
                         , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Outer fcShape.id))
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "30"
                         , ry "30"
                         , stroke strokeColor
                         , strokeDasharray myStrokeDashArray
                         , fill outerColor
                         ] [],
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Inner fcShape.id))
                         , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Inner fcShape.id))
                         , x (toString (fcShape.x + model.graphicsSettings.outerPadding))
                         , y (toString (fcShape.y + model.graphicsSettings.outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "25"
                         , ry "25"
                         , fill innerColor ] [],
                    text' [ pointerEvents "none"
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; ms-user-select: none; khtml-user-select: none;"
                          , fontFamily model.graphicsSettings.fontFamily
                          ,  fontSize (toString model.graphicsSettings.fontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding))
                          , y (toString (fcShape.y+model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding + textHeight / 2 + model.graphicsSettings.fontSize/3))
                          , fill textColor] [Svg.text fcShape.title]]
        Action ->
            let (textWidth, textHeight) = getTextDimension fcShape.title model.graphicsSettings.fontFamily model.graphicsSettings.fontSize
                innerShapeWidth = textWidth + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                innerShapeHeight = textHeight + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                outerShapeWidth = innerShapeWidth + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
                outerShapeHeight = innerShapeHeight + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
            in
                g [ pointerEvents "all"] [
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Outer fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Outer fcShape.id))
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         , stroke strokeColor
                         , strokeDasharray myStrokeDashArray
                         , fill outerColor] [],
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Inner fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Inner fcShape.id))
                         , x (toString (fcShape.x + model.graphicsSettings.outerPadding))
                         , y (toString (fcShape.y + model.graphicsSettings.outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         , fill innerColor ] [],
                    text' [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Inner fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Inner fcShape.id))
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , fontFamily model.graphicsSettings.fontFamily
                          ,  fontSize (toString model.graphicsSettings.fontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding))
                          , y (toString (fcShape.y+model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding + textHeight / 2 + model.graphicsSettings.fontSize / 3))
                          , fill textColor ] [Svg.text fcShape.title]]
        End ->
            let (textWidth, textHeight) = getTextDimension fcShape.title model.graphicsSettings.fontFamily model.graphicsSettings.fontSize
                innerShapeWidth = textWidth + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                innerShapeHeight = textHeight + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                outerShapeWidth = innerShapeWidth + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
                outerShapeHeight = innerShapeHeight + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
            in
                g [ pointerEvents "all"] [
                         
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Outer fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Outer fcShape.id))
                         , x (toString fcShape.x)
                         , y (toString <| fcShape.y)
                         , width <| toString outerShapeWidth
                         , height <| toString outerShapeHeight
                         ,  rx "30"
                         , ry "30"
                         , stroke strokeColor
                         , strokeDasharray myStrokeDashArray
                         , fill outerColor
                         ] [],
                    rect [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Inner fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Inner fcShape.id))
                         , x (toString (fcShape.x + model.graphicsSettings.outerPadding))
                         , y (toString (fcShape.y + model.graphicsSettings.outerPadding))
                         , width <| toString innerShapeWidth
                         , height <| toString innerShapeHeight
                         ,  rx "25"
                         , ry "25"
                         , fill innerColor ] [],
                    text' [ Html.Events.onWithOptions
                               "mousedown"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (DownMsg <| Inner fcShape.id))
                          , Html.Events.onWithOptions
                               "mouseup"
                               { stopPropagation=False
                               , preventDefault=True
                               }
                               (Json.succeed (UpMsg <| Inner fcShape.id))
                          , pointerEvents "none"
                          , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                          , fontFamily model.graphicsSettings.fontFamily
                          ,  fontSize (toString model.graphicsSettings.fontSize)
                          ,  Svg.Attributes.cursor "default"
                          , x (toString (fcShape.x + model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding))
                          , y (toString (fcShape.y+model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding + textHeight / 2 + model.graphicsSettings.fontSize/3))
                          , fill textColor] [Svg.text fcShape.title]]
        Condition ->
            let (textWidth, textHeight) = getTextDimension fcShape.title model.graphicsSettings.fontFamily model.graphicsSettings.fontSize
                innerShapeWidth = textWidth + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                innerShapeHeight = textHeight + model.graphicsSettings.innerPadding + model.graphicsSettings.innerPadding
                outerShapeWidth = innerShapeWidth + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
                outerShapeHeight = innerShapeHeight + model.graphicsSettings.outerPadding + model.graphicsSettings.outerPadding
                innerStartX = fcShape.x + model.graphicsSettings.outerPadding -- - smallAmount
                innerStartY = fcShape.y + model.graphicsSettings.outerPadding
            in
                g [ pointerEvents "all"
                  , onMouseUp (UpMsg <| Outer fcShape.id)
                  ]
                  [ polygon
                      [ Html.Events.onWithOptions
                            "mousedown"
                            { stopPropagation=False
                            , preventDefault=True
                            }
                            (Json.succeed (DownMsg <| Outer fcShape.id))
                      , points ((toString <| fcShape.x) ++ "," ++ (toString <| fcShape.y + outerShapeHeight/2) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth/2)) ++ "," ++ (toString fcShape.y) ++ " "
                                ++ (toString (fcShape.x + outerShapeWidth)) ++ "," ++ (toString (fcShape.y + outerShapeHeight/2)) ++ " "
                                ++ (toString <| fcShape.x + outerShapeWidth / 2) ++ "," ++ (toString (fcShape.y + outerShapeHeight)) ++ " ")

                      , stroke strokeColor
                      , strokeDasharray myStrokeDashArray
                      , fill outerColor
                      ]
                      []
                  , polygon
                      [ Html.Events.onWithOptions
                            "mousedown"
                            { stopPropagation=False
                            , preventDefault=True
                            }
                            (Json.succeed (DownMsg <| Inner fcShape.id))
                      , points ((toString <| innerStartX) ++ "," ++ (toString <| innerStartY + innerShapeHeight/2) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth/2)) ++ "," ++ (toString <| innerStartY) ++ " "
                                ++ (toString (innerStartX + innerShapeWidth)) ++ "," ++ (toString (innerStartY + innerShapeHeight/2)) ++ " "
                                ++ (toString <| innerStartX + innerShapeWidth / 2) ++ "," ++ (toString (innerStartY + innerShapeHeight)) ++ " ")

                       , fill innerColor
                      ]
                      []
                  , text'
                      [ Html.Events.onWithOptions
                            "mousedown"
                            { stopPropagation=False
                            , preventDefault=True
                            }
                            (Json.succeed (DownMsg <| Inner fcShape.id))
                      , fontFamily model.graphicsSettings.fontFamily
                      , fontSize (toString model.graphicsSettings.fontSize)
                      ,  Svg.Attributes.cursor "default"
                      , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none;"
                      , x (toString (fcShape.x + model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding))
                      , y (toString (fcShape.y+model.graphicsSettings.outerPadding + model.graphicsSettings.innerPadding + textHeight / 2 + model.graphicsSettings.fontSize / 3))
                      , fill textColor
                      ]
                      [Svg.text fcShape.title]
                  ]


getTextDimension : String -> String -> Float -> (Float,Float)
getTextDimension text  font fontSize = 
    case font of
        "Courier" -> (((fontSize * (toFloat <| String.length text))) * 0.625, fontSize)
        _ ->         (((fontSize * (toFloat <| String.length text))) * 0.625, fontSize)

