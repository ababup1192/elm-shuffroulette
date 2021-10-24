module Main exposing (..)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Random
import Task


type alias Model =
    { seed : Random.Seed
    , list : List String
    , isPushedLever : Bool
    , isInputted : Bool
    , targetCount : Int
    , currentCount : Int
    , resultList : List String
    , resultText : String
    }


init : { currentTime : Int } -> ( Model, Cmd Msg )
init { currentTime } =
    ( { list = [ "さわ", "ぞーま", "キム", "松尾", "高山", "かの" ]
      , isPushedLever = False
      , isInputted = False
      , targetCount = 0
      , currentCount = 0
      , resultList = []
      , resultText = "[ ]"
      , seed = Random.initialSeed currentTime
      }
    , Cmd.none
    )


type Msg
    = PushLever
    | NewItem
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

        NewItem ->
            ( { model | isInputted = True }, Cmd.none )

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
                                                    "roulette-list-item "
                                                        ++ (if index == targetIndex then
                                                                "selected-target"

                                                            else
                                                                ""
                                                           )
                                                ]
                                                [ text name ]
                                        )
                                        model.list
                                , div [ class "roulette-list-new-item", onClick NewItem ]
                                    [ if model.isInputted then
                                        input [ type_ "text", placeholder "名前を入力", class "roulette-list-new-item-input" ] []

                                      else
                                        span [] [ text "+ アイテムを追加" ]
                                    ]
                                , if model.isInputted then
                                    button [ class "roulette-list-new-item-button" ] [ text "アイテムを追加" ]

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


main : Program { currentTime : Int } Model Msg
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
