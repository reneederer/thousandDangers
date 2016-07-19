module FcElement exposing (..)

import FcTypes exposing (..)
import List exposing (..)

getShapeWithId : Model -> Id -> Maybe FcShape
getShapeWithId model id = 
    head <| List.filter (\el -> el.id == id) model.fcShapes

getArrowWithId : Model -> Id -> Maybe FcArrow
getArrowWithId model id = 
    head <| List.filter (\el -> el.id == id) model.fcArrows

getElementWithId : Model -> FcElementId -> Maybe FcElement
getElementWithId model fcElementId = 
    case fcElementId of
        FcShapeId id -> Maybe.map (\sh -> ShapeElement sh) (getShapeWithId model id)
        FcArrowId id -> Maybe.map (\ar -> ArrowElement ar) (getArrowWithId model id)

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


findFreeId : List {a | id:Id} -> Id
findFreeId l = 
    List.foldl (\el state -> if el.id >= state then el.id + 1 else state) 1 l



removeElement : Model -> FcElementId -> Model
removeElement model fcElementId =
    case fcElementId of
        FcShapeId id ->
            { model | fcShapes = List.filter (\sh -> sh.id /= id) model.fcShapes
                    , fcArrows = List.map (\ar ->
                        case ar.endPos of
                            Offset (endShapeId, x, y) ->
                                if endShapeId == id
                                then { ar | endPos = Global (Maybe.withDefault (0, 0) (Maybe.map (\sh -> (sh.x + x, sh.y + y)) (getShapeWithId model id))) }
                                else ar
                            _ -> ar)
                        (List.map (\ar ->
                            case ar.startPos of
                                Offset (startShapeId, x, y) ->
                                    if startShapeId == id
                                    then { ar | startPos = Global (Maybe.withDefault (0, 0) (Maybe.map (\sh -> (sh.x + x, sh.y + y)) (getShapeWithId model id))) }
                                    else ar
                                _ -> ar) model.fcArrows) }
        FcArrowId id ->
            { model | fcArrows = List.filter (\sh -> sh.id /= id) model.fcArrows }

doesPositionShareElements : FcPos -> FcPos -> Bool
doesPositionShareElements pos1 pos2 =
    case pos1 of
        Offset (id1, _, _) ->
            case pos2 of
                Offset (id2, _, _) -> id1 == id2
                _ -> False
        _ -> False










