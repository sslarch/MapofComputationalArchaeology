module Tests exposing (..)

import TeachingMaterial exposing (parseTeachingResources)
import TeachingMaterialData exposing (teachingMaterialString)

import Expect
import Test exposing (Test, describe, test)

suite : Test
suite =
    describe "Pseudo-test for teaching material list validation"
        [ test "if the .tsv table can be parsed" <|
            \_ ->
                let res = parseTeachingResources teachingMaterialString
                in Expect.ok res
        ]
