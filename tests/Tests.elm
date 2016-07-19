module Tests exposing (..)

import String
import ElmTest exposing (..)
import FcElementTest exposing (..)
import FcDatabaseTest exposing (..)

tests : Test
tests = 
    suite "All Tests"
        [ fcElementTests
        , fcDatabaseTests
        ]

getTestResults = stringRunner tests
















