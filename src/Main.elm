port module Main exposing (..)

import Array
import Browser
import Browser.Dom as Dom
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
    , resultText : String
    , inputItemName : String
    }


init : { currentTime : Int, listValue : String } -> ( Model, Cmd Msg )
init { currentTime, listValue } =
    ( { list = Result.withDefault [] <| JD.decodeString (JD.list JD.string) listValue
      , isPushedLever = False
      , isInputted = False
      , targetCount = 0
      , currentCount = 0
      , resultList = []
      , resultText = "[ ]"
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
    | StartAnimation Int
    | PlusTargetCount ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getTargetList lastList =
            case lastList of
                [] ->
                    model.list

                _ ->
                    lastList
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PushLever ->
            let
                { lastList } =
                    createListList model.list model.resultList
            in
            if model.isPushedLever || List.length lastList == 1 then
                ( model, Cmd.none )

            else
                let
                    ( randomNum, nextSeed ) =
                        Random.step
                            (Random.int 0 <|
                                List.length (getTargetList lastList)
                            )
                        <|
                            model.seed
                in
                update (StartAnimation randomNum) { model | isPushedLever = True, seed = nextSeed }

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

        StartAnimation randomNum ->
            let
                { lastList } =
                    createListList model.list model.resultList

                targetCount =
                    (List.length (getTargetList lastList) * 5)
                        + randomNum
            in
            ( { model | targetCount = targetCount, currentCount = 0 }
            , Cmd.batch <|
                List.map
                    (\x ->
                        Process.sleep (toFloat x * 100)
                            |> Task.perform PlusTargetCount
                    )
                    (List.range 0 targetCount)
            )

        PlusTargetCount () ->
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
                            Array.fromList (getTargetList lastList)

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
                , resultText =
                    let
                        lastNameList =
                            List.filter
                                (\n ->
                                    n
                                        /= (Maybe.withDefault "" <|
                                                Array.get targetIndex <|
                                                    Array.fromList (getTargetList lastList)
                                           )
                                )
                                lastList
                    in
                    if isStop then
                        "[ "
                            ++ String.join ", "
                                (List.map (\name -> "\"" ++ name ++ "\"")
                                    (newResultList
                                        ++ (if List.length lastNameList == 1 then
                                                lastNameList

                                            else
                                                []
                                           )
                                    )
                                )
                            ++ " ]"

                    else
                        model.resultText
              }
            , Cmd.none
            )


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
    in
    div [ class "main-container" ]
        [ div [ class "roulette-results" ] <|
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
            ]
                ++ (case model.resultList of
                        [] ->
                            [ div [ class "roulette" ]
                                [ header [ class "roulette-header" ]
                                    [ h1 [ class "roulette-header-title" ] [ text "1" ]
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

                        _ ->
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
                                ++ [ div [ class "roulette" ]
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
                   )
        , h2 [ class "roulette-result-text" ] [ text model.resultText ]
        ]


main : Program { currentTime : Int, listValue : String } Model Msg
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
