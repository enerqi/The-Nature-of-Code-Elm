module Main exposing (..)

import Collage
import Element exposing (toHtml)
import Html.App as App
import Html
import Random
import Time


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


screenSize =
    ( 960, 540 )


type alias Position =
    { x : Int, y : Int }


type alias Model =
    Position


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
    ( { x = truncate <| (fst screenSize) / 2
      , y = truncate <| (snd screenSize) / 2
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
            ( wander (toDirection n) model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (500 * Time.millisecond) Tick


view : Model -> Html.Html Msg
view model =
    Html.text <| toString model



--Collage.traced Collage.defaultLine
--    (Collage.segment ( 0.0, 0.0 ) ( 300.0, 300.0 ))


wander : Direction -> Model -> Model
wander direction model =
    offset model direction |> clampToCanvasBoundary


clampToCanvasBoundary : Model -> Model
clampToCanvasBoundary model =
    let
        ( screenWidth, screenHeight ) =
            screenSize
    in
        { x = clamp 0 screenWidth model.x
        , y = clamp 0 screenHeight model.y
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
