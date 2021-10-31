port module Main exposing (..)

import Array
import Browser
import Browser.Dom as Dom
import Fuzz exposing (result)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Process
import Random
import Result
import Task


port saveList : JE.Value -> Cmd msg


encodeJsonList : List String -> JE.Value
encodeJsonList list =
    JE.list JE.string list


type alias Model =
    { seed : Random.Seed
    , list : List String
    , isPushedLever : Bool
    , isInputted : Bool
    , targetCount : Int
    , currentCount : Int
    , resultList : List String
    , inputItemName : String
    }


init : { currentTime : Int, listValue : JD.Value } -> ( Model, Cmd Msg )
init { currentTime, listValue } =
    ( { list = Result.withDefault [] <| JD.decodeValue (JD.list JD.string) listValue
      , isPushedLever = False
      , isInputted = False
      , targetCount = 0
      , currentCount = 0
      , resultList = []
      , seed = Random.initialSeed currentTime
      , inputItemName = ""
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | PushLever
    | NewItem
    | InputItemName String
    | CloseNewItem
    | AddItem
    | RemoveItem String
    | DoRouletteAnimation ()
    | UndoRoulette
    | BombList


getTargetList : List String -> List String -> List String
getTargetList list lastList =
    case lastList of
        [] ->
            list

        _ ->
            lastList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PushLever ->
            let
                newModel =
                    pushLever model

                animationList =
                    List.map
                        (\x ->
                            Process.sleep (toFloat x * 100)
                                |> Task.perform DoRouletteAnimation
                        )
                        (List.range 0 newModel.targetCount)
            in
            ( newModel
            , if newModel == model then
                Cmd.none

              else
                Cmd.batch animationList
            )

        InputItemName name ->
            ( { model | inputItemName = name }, Cmd.none )

        NewItem ->
            ( { model | isInputted = True }, Task.attempt (\_ -> NoOp) (Dom.focus "new-item-input") )

        AddItem ->
            let
                newList =
                    model.list ++ [ model.inputItemName ]
            in
            ( { model | list = newList, inputItemName = "" }
            , Cmd.batch
                [ saveList <| encodeJsonList newList
                , Task.attempt (\_ -> NoOp) (Dom.focus "new-item-input")
                ]
            )

        RemoveItem name ->
            let
                newList =
                    List.filter ((/=) name) model.list
            in
            ( { model | list = newList }, saveList <| encodeJsonList newList )

        CloseNewItem ->
            ( { model | isInputted = False }, Cmd.none )

        DoRouletteAnimation () ->
            let
                { lastList } =
                    createListList model.list model.resultList

                targetIndex =
                    modBy
                        (List.length
                            (case lastList of
                                [] ->
                                    model.list

                                _ ->
                                    lastList
                            )
                        )
                        model.currentCount

                appendNameList =
                    maybeToList <|
                        Array.get targetIndex <|
                            Array.fromList (getTargetList model.list lastList)

                newResultList =
                    if model.currentCount == model.targetCount then
                        model.resultList ++ appendNameList

                    else
                        model.resultList

                isStop =
                    model.currentCount == model.targetCount
            in
            ( { model
                | currentCount =
                    if isStop then
                        0

                    else
                        model.currentCount + 1
                , isPushedLever = not isStop
                , resultList =
                    newResultList
              }
            , Cmd.none
            )

        UndoRoulette ->
            ( { model
                | targetCount = 0
                , currentCount = 0
                , resultList = []
              }
            , Cmd.none
            )

        BombList ->
            ( { model | list = [] }, saveList <| encodeJsonList [] )


pushLever : Model -> Model
pushLever model =
    let
        { lastList } =
            createListList model.list model.resultList

        targetCount =
            List.length (getTargetList model.list lastList) * 5
    in
    if model.isPushedLever || List.length model.list <= 1 || List.length lastList == 1 then
        model

    else
        let
            ( randomNum, nextSeed ) =
                Random.step
                    (Random.int 0 <|
                        List.length (getTargetList model.list lastList)
                    )
                <|
                    model.seed
        in
        { model | isPushedLever = True, seed = nextSeed, targetCount = targetCount + randomNum, currentCount = 0 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias ResultListList =
    { listList : List ( String, List String )
    , lastList : List String
    }


createListList : List String -> List String -> ResultListList
createListList base resultList =
    createListList_ base resultList { listList = [], lastList = [] }


createListList_ : List String -> List String -> ResultListList -> ResultListList
createListList_ base resultList resultListList =
    case resultList of
        [] ->
            resultListList

        x :: xs ->
            let
                nextBase =
                    List.filter ((/=) x) base
            in
            createListList_ nextBase xs { listList = resultListList.listList ++ [ ( x, base ) ], lastList = nextBase }


view : Model -> Html Msg
view model =
    let
        { listList, lastList } =
            createListList model.list model.resultList

        targetIndex =
            if List.isEmpty model.list then
                0

            else
                modBy
                    (List.length
                        (case lastList of
                            [] ->
                                model.list

                            _ ->
                                lastList
                        )
                    )
                    model.currentCount

        controlView =
            [ img
                [ onClick PushLever
                , src <|
                    if model.isPushedLever then
                        "assets/after-lever.svg"

                    else
                        "assets/before-lever.svg"
                , class "roulette-lever"
                ]
                []
            , i [ class "fas fa-undo roulette-undo", onClick UndoRoulette ] []
            ]

        -- リストを作るところ
        listViewForCreate =
            [ div [ class "roulette" ]
                [ header [ class "roulette-header" ]
                    [ h1 [ class "roulette-header-title" ]
                        [ text "1"
                        , i [ class "fas fa-bomb roulette-list-bomb-button", onClick BombList ] []
                        ]
                    ]
                , ul [ class "roulette-list" ] <|
                    List.indexedMap
                        (\index name ->
                            li
                                [ class <|
                                    "roulette-list-item roulette-list-item__removeable "
                                        ++ (if index == targetIndex then
                                                "selected-target"

                                            else
                                                ""
                                           )
                                , onClick <| RemoveItem name
                                ]
                                [ text name ]
                        )
                        model.list

                -- アイテム追加するところ
                , div [ class "roulette-list-new-item", onClick NewItem ]
                    [ if model.isInputted then
                        input
                            [ id "new-item-input"
                            , type_ "text"
                            , placeholder "名前を入力"
                            , class "roulette-list-new-item-input"
                            , value model.inputItemName
                            , onInput InputItemName
                            ]
                            []

                      else
                        span [] [ text "+ アイテムを追加" ]
                    ]
                , if model.isInputted then
                    div [ class "roulette-list-new-item-button-wrapper" ]
                        [ button [ class "roulette-list-new-item-button", onClick AddItem ] [ text "アイテムを追加" ]
                        , i [ onClick CloseNewItem, class "fas fa-times roulette-list-new-item-button-close" ] []
                        ]

                  else
                    text ""
                ]
            ]

        -- 回し終わった後のリスト
        shuffledList =
            List.indexedMap
                (\index ( n, list ) ->
                    div [ class "roulette" ]
                        [ header [ class "roulette-header" ]
                            [ h1 [ class "roulette-header-title" ] [ text <| String.fromInt (index + 1) ]
                            ]
                        , ul [ class "roulette-list" ] <|
                            List.map
                                (\name ->
                                    li
                                        [ class <|
                                            "roulette-list-item "
                                                ++ (if name == n then
                                                        "selected-target"

                                                    else
                                                        ""
                                                   )
                                        ]
                                        [ text name ]
                                )
                                list
                        ]
                )
                listList

        -- ルーレットを回す対象のリスト
        shuffleList =
            [ div [ class "roulette" ]
                [ header [ class "roulette-header" ]
                    [ h1 [ class "roulette-header-title" ] [ text <| String.fromInt <| List.length listList + 1 ]
                    ]
                , ul [ class "roulette-list" ] <|
                    List.indexedMap
                        (\index name ->
                            li
                                [ class <|
                                    "roulette-list-item "
                                        ++ (if index == targetIndex then
                                                "selected-target"

                                            else
                                                ""
                                           )
                                ]
                                [ text name ]
                        )
                        lastList
                ]
            ]

        isStartedRoulette =
            List.isEmpty model.resultList
    in
    -- レバーとか回すところ
    div [ class "main-container" ]
        [ div [ class "roulette-results" ] <|
            controlView
                ++ (if isStartedRoulette then
                        listViewForCreate

                    else
                        shuffledList ++ shuffleList
                   )
        , h2 [ class "roulette-result-text" ]
            [ text <|
                createSelectedText
                    { selectedList = model.resultList
                    , restList = lastList
                    }
            ]
        ]


type alias SelectedListArgs =
    { selectedList : List String
    , restList : List String
    }


createSelectedText : SelectedListArgs -> String
createSelectedText { selectedList, restList } =
    case ( selectedList, restList ) of
        ( [], [] ) ->
            "[ ]"

        ( [], [ a ] ) ->
            "[ ]"

        _ ->
            "[ "
                ++ String.join ", "
                    (selectedList
                        ++ (case restList of
                                [ last ] ->
                                    [ last ]

                                _ ->
                                    []
                           )
                    )
                ++ " ]"


main : Program { currentTime : Int, listValue : JE.Value } Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]
