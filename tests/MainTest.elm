module MainTest exposing (..)

import Expect
import Fuzz exposing (string)
import Main exposing (Msg(..), createSelectedText, update)
import Test exposing (..)


createSelectedTextTest : Test
createSelectedTextTest =
    describe "#createSelectedText"
        [ describe "johnが選ばれていて、TomとAliceが残っているとき"
            [ test "[ john ]となる" <|
                \_ ->
                    createSelectedText { selectedList = [ "john" ], restList = [ "Tom", "Alice" ] }
                        |> Expect.equal "[ john ]"
            ]
        , describe "johnとTomが選ばれていて、Aliceが残っているとき"
            [ test "全員確定するので、 [ john, Tom, Alice ]となる" <|
                \_ ->
                    createSelectedText { selectedList = [ "john", "Tom" ], restList = [ "Alice" ] }
                        |> Expect.equal "[ john, Tom, Alice ]"
            ]
        , describe "johnが残っているとき"
            [ test "ルーレットは回せないので、 [ ] 空となる" <|
                \_ ->
                    createSelectedText { selectedList = [], restList = [ "john" ] }
                        |> Expect.equal "[ ]"
            ]
        ]
