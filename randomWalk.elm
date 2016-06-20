module Main exposing (..)

import Collage
import Color
import Element exposing (toHtml)
import Html.App as App
import Html
import Random
import Set
import Set exposing (Set)
import Time


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Position =
    { x : Int, y : Int }


type alias PositionKey =
    ( Int, Int )


type alias TrailSet =
    Set PositionKey


type alias Trail =
    List Position


type alias Model =
    { currentPosition : Position
    , previousPositionsTrail : Trail
    , previousPositions : TrailSet
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


trailSize =
    500


init : ( Model, Cmd Msg )
init =
    -- Note the Collage API has Y going up and (0,0) the origin at the centre of the canvas
    ( { currentPosition = { x = 0, y = 0 }
      , previousPositionsTrail = []
      , previousPositions = Set.empty
      }
    , Cmd.none
    )



-- VIEW


canvasWidth : Int
canvasWidth =
    960


canvasMaxX =
    480


canvasMinX =
    -480


canvasHeight : Int
canvasHeight =
    540


canvasMaxY =
    270


canvasMinY =
    -270


view : Model -> Html.Html Msg
view model =
    let
        --Debug.log "model" <| toString model
        defaultLineStyle =
            Collage.defaultLine
    in
        -- Note the Collage API has Y going up and (0,0) the origin at the centre of the canvas
        Element.toHtml
            <| Collage.collage canvasWidth
                canvasHeight
                [ Collage.traced
                    { defaultLineStyle
                        | color = Color.green
                        , width = (toFloat movementRate) / 2.0
                        , join = Collage.Smooth
                        , cap = Collage.Round
                    }
                    (Collage.path <| List.map (\{ x, y } -> ( toFloat x, toFloat y )) (model.currentPosition :: model.previousPositionsTrail))
                ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model
            , Random.generate RandomDirection (Random.int 1 9)
            )

        RandomDirection n ->
            let
                ( trail, trailSet ) =
                    extendTrailToCurrentPosition model trailSize
            in
                ( { currentPosition = wander (toDirection n) model
                  , previousPositionsTrail = trail
                  , previousPositions = trailSet
                  }
                , Cmd.none
                )


movementRate =
    15


tickEveryMilliseconds =
    33


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (tickEveryMilliseconds * Time.millisecond) Tick


asPositionKey : Position -> PositionKey
asPositionKey { x, y } =
    ( x, y )


extendTrailToCurrentPosition : Model -> Int -> ( Trail, TrailSet )
extendTrailToCurrentPosition model maxTrailSize =
    if not <| Set.member (asPositionKey model.currentPosition) model.previousPositions then
        let
            trailSize =
                List.length model.previousPositionsTrail

            newTrailSize =
                trailSize + 1

            currentPositionKey =
                asPositionKey model.currentPosition
        in
            if newTrailSize > maxTrailSize then
                let
                    nextTrail =
                        -- how do we make this faster -> Implement our own circular buffer on top of arrays
                        model.currentPosition :: (List.take (maxTrailSize - 1) model.previousPositionsTrail)

                    nextTrailSet =
                        removeTrailRearFromSet model.previousPositionsTrail model.previousPositions
                in
                    ( nextTrail, nextTrailSet )
            else
                ( model.currentPosition :: model.previousPositionsTrail
                , Set.insert currentPositionKey model.previousPositions
                )
    else
        ( model.previousPositionsTrail, model.previousPositions )


removeTrailRearFromSet : Trail -> TrailSet -> TrailSet
removeTrailRearFromSet trail trailSet =
    let
        trailSize =
            List.length trail

        trailRear =
            -- how do we make this faster -> Implement our own circular buffer on top of arrays
            List.drop (trailSize - 1) trail
                |> List.head
    in
        case trailRear of
            Just position ->
                Set.remove (asPositionKey position) trailSet

            Nothing ->
                trailSet


wander : Direction -> Model -> Position
wander direction model =
    let
        nextPosition =
            offset model.currentPosition direction

        positionCrossingTrail =
            Set.member (asPositionKey nextPosition) model.previousPositions

        positionInBounds =
            nextPosition.x
                >= canvasMinX
                && nextPosition.x
                < canvasMaxX
                && nextPosition.y
                >= canvasMinY
                && nextPosition.y
                < canvasMaxY
    in
        if positionCrossingTrail || not positionInBounds then
            -- If all routes block, just cross the trail anyway, otherwise avoid doing so
            if positionInBounds && allRoutesBlocked model.currentPosition model.previousPositions then
                nextPosition
            else
                model.currentPosition
        else
            nextPosition


allRoutesBlocked : Position -> TrailSet -> Bool
allRoutesBlocked position trailSet =
    let
        offsetFromCurrent =
            offset position

        positionOffsets =
            List.map offsetFromCurrent [ Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight ]
    in
        List.any (\pos -> Set.member (asPositionKey pos) trailSet) positionOffsets


offset : Position -> Direction -> Position
offset position dir =
    case dir of
        Up ->
            { position | y = position.y - movementRate }

        Down ->
            { position | y = position.y + movementRate }

        Left ->
            { position | x = position.x - movementRate }

        Right ->
            { position | x = position.x + movementRate }

        UpLeft ->
            { x = position.x - movementRate, y = position.y - movementRate }

        UpRight ->
            { x = position.x + movementRate, y = position.y - movementRate }

        DownLeft ->
            { x = position.x - movementRate, y = position.y + movementRate }

        DownRight ->
            { x = position.x + movementRate, y = position.y + movementRate }

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
