module Main exposing (..)

import Collage
import Element exposing (toHtml)
import Html.App as App
import Html
import Random
import Set
import Time


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


canvasWidth : Int
canvasWidth =
    960


canvasHeight : Int
canvasHeight =
    540


type alias Position =
    { x : Int, y : Int }


type alias Model =
    { currentPosition : Position
    , previousPositions : List Position
    }


type Direction
    = None
    | Up
    | Down
    | Left
    | Right
    | UpLeft
    | UpRight
    | DownLeft
    | DownRight


type Msg
    = RandomDirection Int
    | Tick Time.Time


init : ( Model, Cmd Msg )
init =
    -- Note the Collage API has Y going up and (0,0) the origin at the centre of the canvas
    ( { currentPosition =
            { x = 0
            , y = 0
            }
      , previousPositions = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model
            , Random.generate RandomDirection (Random.int 1 9)
            )

        RandomDirection n ->
            ( { currentPosition = wander (toDirection n) model.currentPosition
              , previousPositions = positionsHistoryUpdate model.previousPositions model.currentPosition 100
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (500 * Time.millisecond) Tick


view : Model -> Html.Html Msg
view model =
    let
        _ =
            -- Set requires comparable, so toString the record
            -- includes numbers, characters, strings, lists of comparable things, and tuples of comparable things
            Debug.log "model" (Set.fromList <| List.map toString model.previousPositions)
    in
        -- Note the Collage API has Y going up and (0,0) the origin at the centre of the canvas
        Element.toHtml
            <| Collage.collage canvasWidth
                canvasHeight
                [ Collage.traced Collage.defaultLine
                    (Collage.path <| List.map floatify (model.currentPosition :: model.previousPositions))
                ]


positionsHistoryUpdate : List Position -> Position -> Int -> List Position
positionsHistoryUpdate positions newPosition maxHistorySize =
    let
        historySize =
            List.length positions

        newHistorySize =
            historySize + 1
    in
        if newHistorySize > maxHistorySize then
            newPosition :: (List.take (maxHistorySize - 1) positions)
        else
            newPosition :: positions


floatify : Position -> ( Float, Float )
floatify position =
    ( toFloat position.x, toFloat position.y )


wander : Direction -> Position -> Position
wander direction position =
    offset position direction |> clampToCanvasBoundary


clampToCanvasBoundary : Position -> Position
clampToCanvasBoundary position =
    { x = clamp 0 canvasWidth position.x
    , y = clamp 0 canvasHeight position.y
    }


offset : Position -> Direction -> Position
offset position dir =
    case dir of
        Up ->
            { position | y = position.y - 1 }

        Down ->
            { position | y = position.y + 1 }

        Left ->
            { position | x = position.x - 1 }

        Right ->
            { position | x = position.x + 1 }

        UpLeft ->
            { x = position.x - 1, y = position.y - 1 }

        UpRight ->
            { x = position.x + 1, y = position.y - 1 }

        DownLeft ->
            { x = position.x - 1, y = position.y + 1 }

        DownRight ->
            { x = position.x + 1, y = position.y + 1 }

        None ->
            position


toDirection n =
    case n of
        1 ->
            Up

        2 ->
            Down

        3 ->
            Left

        4 ->
            Right

        5 ->
            UpLeft

        6 ->
            UpRight

        7 ->
            DownLeft

        8 ->
            DownRight

        _ ->
            None
