module FcElementTest exposing (fcElementTests)

import String
import ElmTest exposing (..)
import FcElement exposing (..)
import FcTypes exposing (..)

setup : Model
setup = 
    { fcShapes = [ { id=1
                   , shapeType=Start
                   , x=0.0
                   , y=0.0
                   , text="text"
                   , title="title"
                   }
                 , { id=2
                   , shapeType=Start
                   , x=30.0
                   , y=40.0
                   , text="text"
                   , title="title"
                   }
                 , { id=3
                   , shapeType=Start
                   , x=0.0
                   , y=0.0
                   , text="text"
                   , title="title"
                   }
                 , { id=4
                   , shapeType=Start
                   , x=0.0
                   , y=0.0
                   , text="text"
                   , title="title"
                   }
                 , { id=5
                   , shapeType=Start
                   , x=0.0
                   , y=0.0
                   , text="text"
                   , title="title"
                   }
                 ]
    , fcArrows = [ { id=1
                   , startPos=Offset (1, 20, 20)
                   , endPos=Global (300, 200)
                   , title="title"
                   }
                 , { id=2
                   , startPos=Offset (1, 20, 20)
                   , endPos=Offset (2, 300, 200)
                   , title="title"
                   }
                 , { id=3
                   , startPos=Offset (1, 20, 20)
                   , endPos=Global (300, 200)
                   , title="title"
                   }
                 , { id=4
                   , startPos=Offset (8, 20, 20)
                   , endPos=Offset (3, 300, 200)
                   , title="title"
                   }
                 ]
    , dragElement = Nothing
    , dragOffsetX = 0.0
    , dragOffsetY = 0.0
    , selectedElementId = Nothing
    , currentLine = Nothing
    , mainDivOffset = { x = 0, y = 0 }
    , graphicsSettings = { fontFamily = "Courier"
                         , fontSize = 20.0
                         , innerPadding = 15.0
                         , outerPadding = 10.0
                         }
    }

fcElementTests : Test
fcElementTests = 
    let model = setup
    in
    suite "FcElementTest"
        [ suite "getShapeWithId"
            [ test "getShapeWithId_NonexistentId_ReturnsNothing" (assertEqual Nothing (getShapeWithId model 12))
            , test "getShapeWithId_ExistingShape_ReturnsShape" (assertEqual (Just { id=1, shapeType=Start,x=0.0, y=0.0, text="text", title="title" }) (getShapeWithId model 1))
            ]
        , suite "getArrowWithId"
            [ test "getArrowWithId_NonexistentArrow_ReturnsNothing" (assertEqual Nothing (getArrowWithId model 12))
            , test "getArrowWithId_ExistingArrow_ReturnsArrow" (assertEqual (Just { id=1, startPos=Offset (1, 20, 20), endPos=Global (300, 200), title="title" }) (getArrowWithId model 1))
            ]
        , suite "getElementWithId"
            [ test "getElementWithId_NonexistentShapeElement_ReturnsNothing" (assertEqual Nothing (getElementWithId model (FcShapeId 12)))
            , test "getElementWithId_ExistingShapeElement_ReturnsShapeElement" (assertEqual (Just (ShapeElement { id=1, shapeType=Start,x=0.0, y=0.0, text="text", title="title" })) (getElementWithId model (FcShapeId 1)))
            , test "getElementWithId_NonexistentArrowElement_ReturnsNothing" (assertEqual Nothing (getElementWithId model (FcArrowId 12)))
            , test "getElementWithId_ExistingArrowElement_ReturnsArrowElement" (assertEqual (Just (ArrowElement { id=1, startPos=Offset (1, 20, 20), endPos=Global (300, 200), title="title" })) (getElementWithId model (FcArrowId 1)))
            ]
        , suite "getStartShapeId"
            [ test "getStartShapeId_OffsetArrow_ReturnsId" (assertEqual (Just 1) (getStartShapeId {id=2, startPos=Offset (1, 20, 20), endPos=Offset (3, 300, 200), title="title" }))
            , test "getStartShapeId_OffsetArrow_ReturnsId" (assertEqual Nothing (getStartShapeId {id=2, startPos=Global ( 20, 20), endPos=Offset (3, 300, 200), title="title" }))
            ]
        , suite "getEndShapeId"
            [ test "getEndShapeId_OffsetArrow_ReturnsId" (assertEqual (Just 3) (getEndShapeId {id=2, startPos=Offset (1, 20, 20), endPos=Offset (3, 300, 200), title="title" }))
            , test "getEndShapeId_OffsetArrow_ReturnsId" (assertEqual Nothing (getEndShapeId {id=2, startPos=Global ( 20, 20), endPos=Global (300, 200), title="title" }))
            ]
        , suite "arrowsWithStartShape"
            [ test "arrowsWithStartShape_NonexistentId_ReturnsEmptyList" (assertEqual [] (arrowsWithStartShape model 122))
            , test "arrowsWithStartShape_ExistentId_ReturnsArrowList"
                (assertEqual
                    [
                       { id=1
                       , startPos=Offset (1, 20, 20)
                       , endPos=Global (300, 200)
                       , title="title"
                       }
                     , { id=2
                       , startPos=Offset (1, 20, 20)
                       , endPos=Offset (2, 300, 200)
                       , title="title"
                       }
                     , { id=3
                       , startPos=Offset (1, 20, 20)
                       , endPos=Global (300, 200)
                       , title="title"
                       }
                    ]
                    (arrowsWithStartShape model 1)
                )
            ]
        , suite "arrowsWithEndShape"
            [ test "arrowsWithEndShape_NonexistentId_ReturnsEmptyList"
                (assertEqual
                    []
                    (arrowsWithEndShape model 122)
                )
            , test "arrowsWithEndShape_ExistentId_ReturnsArrowList"
                (assertEqual
                    [
                       { id=4
                       , startPos=Offset (8, 20, 20)
                       , endPos=Offset (3, 300, 200)
                       , title="title"
                       }
                    ]
                    (arrowsWithEndShape model 3)
                )
            ]
        , suite "findFreeId"
            [ test
                "findFreeId_EmptyList_ReturnsOne"
                (assertEqual
                    (findFreeId [])
                    1
                )
            , test "findFreeId_NonEmptyList_ReturnsNextId"
                (assertEqual
                    6
                    (findFreeId
                        [
                          { id=1 }
                        , { id=2 }
                        , { id=5 }
                        ]
                    )
                )
            ]
        , suite "removeElement"
            [ test "removeElement_NonExistentShapeId_ReturnsSameModel"
                (assertEqual
                    model
                    (removeElement
                        model
                        (FcShapeId 89))
                )
            , test "removeElement_NonExistentArrowId_ReturnsSameModel"
                (assertEqual
                    (removeElement
                        model
                        (FcArrowId 89))
                    model
                )
            , test "removeElement_ExistingShapeId_ReturnsUpdatedModel"
                (assertEqual
                    { fcShapes =
                        [ { id=1
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=3
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=4
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=5
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        ]
                    , fcArrows =
                        [ { id=1
                          , startPos=Offset (1, 20, 20)
                          , endPos=Global (300, 200)
                          , title="title"
                          }
                        , { id=2
                          , startPos=Offset (1, 20, 20)
                          , endPos=Global (330, 240)
                          , title="title"
                          }
                        , { id=3
                          , startPos=Offset (1, 20, 20)
                          , endPos=Global (300, 200)
                          , title="title"
                          }
                        , { id=4
                          , startPos=Offset (8, 20, 20)
                          , endPos=Offset (3, 300, 200)
                          , title="title"
                          }
                        ]
                    , dragElement = Nothing
                    , dragOffsetX = 0.0
                    , dragOffsetY = 0.0
                    , selectedElementId = Nothing
                    , currentLine = Nothing
                    , mainDivOffset = { x = 0, y = 0 }
                    , graphicsSettings = { fontSize = 20.0
                                       , fontFamily = "Courier"
                                       , innerPadding = 10.0
                                       , outerPadding = 15.0
                                       }
                    }
                    (removeElement model (FcShapeId 2)))
            , test "removeElement_ExistingArrowId_ReturnsUpdatedModel"
                (assertEqual
                    { fcShapes =
                        [ { id=1
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=2
                          , shapeType=Start
                          , x=30.0
                          , y=40.0
                          , text="text"
                          , title="title"
                          }
                        , { id=3
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=4
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        , { id=5
                          , shapeType=Start
                          , x=0.0
                          , y=0.0
                          , text="text"
                          , title="title"
                          }
                        ]
                    , fcArrows =
                        [ { id=1
                          , startPos=Offset (1, 20, 20)
                          , endPos=Global (300, 200)
                          , title="title"
                          }
                        , { id=3
                          , startPos=Offset (1, 20, 20)
                          , endPos=Global (300, 200)
                          , title="title"
                          }
                        , { id=4
                          , startPos=Offset (8, 20, 20)
                          , endPos=Offset (3, 300, 200)
                          , title="title"
                          }
                        ]
                    , dragElement = Nothing
                    , dragOffsetX = 0.0
                    , dragOffsetY = 0.0
                    , selectedElementId = Nothing
                    , currentLine = Nothing
                    , mainDivOffset = { x = 0, y = 0 }
                    , graphicsSettings = { fontFamily = "Courier"
                                         , fontSize = 20.0
                                         , innerPadding = 15.0
                                         , outerPadding = 10.0
                                         }
                    }
                    (removeElement model (FcArrowId 2)))
            ]
        , suite "doesPositionShareElements"
            [ test "doesPositionShareElements_UnsharedOffsetPositions_ReturnsFalse"
                (assertEqual
                    False
                    (doesPositionShareElements
                        (Offset (1, 0, 0))
                        (Offset (2, 0, 0))
                    )
                )
            , test "doesPositionShareElements_GlobalPositions_ReturnsFalse"
                (assertEqual
                    False
                    (doesPositionShareElements
                        (Global (0, 0))
                        (Global (0, 0))
                    )
                )
            , test "doesPositionShareElements_OffsetAndGlobalPositions_ReturnsFalse"
                (assertEqual
                    False
                    (doesPositionShareElements
                        (Offset (1, 0, 0))
                        (Global (0, 0))
                    )
                )
            , test "doesPositionShareElements_SharedPosition_ReturnsTrue"
                (assertEqual
                    True
                    (doesPositionShareElements
                        (Offset (1, 0, 0))
                        (Offset (1, 0, 0))
                    )
                )
            ]
        ]







