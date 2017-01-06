module Metronome exposing (..)

import Html exposing (Html, div, input, label)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { angle : Float
    , sweepCenter : Point
    , timeDiff : Time
    , beatCount : Int
    , bpm : Int
    , speed : Float
    , beats : List Point
    }


type alias Point =
    { x : Float
    , y : Float
    }


tau =
    2 * pi


markerRadius =
    3


faceCenter =
    Point 50 50


faceRadius =
    45


defaultAngle =
    3 * tau / 4


defaultBeatCount =
    4


defaultBpm =
    60


init : ( Model, Cmd Msg )
init =
    let
        center =
            pointOnFace defaultAngle

        timeDiff =
            0

        beatCount =
            defaultBeatCount

        bpm =
            defaultBpm

        speed =
            calculateSpeed beatCount bpm

        beats =
            calculateBeats beatCount
    in
        ( (Model defaultAngle center timeDiff beatCount bpm speed beats), Cmd.none )


pointOnFace : Float -> Point
pointOnFace angle =
    let
        x =
            (faceCenter.x + faceRadius * cos angle)

        y =
            (faceCenter.y + faceRadius * sin angle)
    in
        Point x y


type Msg
    = TimeUpdate Time
    | BpmUpdate String
    | BeatUpdate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate deltaT ->
            ( (updatePosition model deltaT), Cmd.none )

        BpmUpdate newBpm ->
            let
                intBpm =
                    case String.toInt newBpm of
                        Err msg ->
                            0

                        Ok intBpm ->
                            intBpm
            in
                ( (updateBpm model intBpm), Cmd.none )

        BeatUpdate newBeatCount ->
            let
                beatCount =
                    case String.toInt newBeatCount of
                        Err msg ->
                            defaultBeatCount

                        Ok converted ->
                            converted
            in
                ( (updateBeatCount model beatCount), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs TimeUpdate


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label []
                [ Html.text "BPM"
                , input [ type_ "text", value (toString model.bpm), onInput BpmUpdate ] []
                ]
            ]
        , div []
            [ label []
                [ Html.text "Beat Count"
                , input [ type_ "text", value (toString model.beatCount), onInput BeatUpdate ] []
                ]
            ]
        , svg [ viewBox "0 0 100 100", width "300px" ] (buildFace model)
        ]


buildFace : Model -> List (Svg Msg)
buildFace { sweepCenter, beats } =
    let
        x =
            toString sweepCenter.x

        y =
            toString sweepCenter.y
    in
        List.concat
            [ [ circle [ cx (toString faceCenter.x), cy (toString faceCenter.y), r (toString faceRadius), stroke "#BADA55", fill "none" ] [] ]
            , (beatMarkers beats sweepCenter)
            , [ circle [ cx x, cy y, r (toString markerRadius), stroke "#666666", fill "none" ] [] ]
            ]


calculateBeats : Int -> List Point
calculateBeats n =
    beatReducer [] defaultAngle (tau / toFloat n) n


beatReducer accumulator angle increment counter =
    if counter <= 0 then
        accumulator
    else
        let
            beat =
                pointOnFace angle

            nextAngle =
                angle + increment

            nextCounter =
                counter - 1
        in
            beatReducer (beat :: accumulator) nextAngle increment nextCounter


beatMarkers : List Point -> Point -> List (Svg Msg)
beatMarkers beats sweepCenter =
    List.map (\beat -> buildMarker beat sweepCenter) beats


buildMarker : Point -> Point -> Svg Msg
buildMarker beat sweepCenter =
    let
        x =
            toString beat.x

        y =
            toString beat.y

        fillColor =
            if intersects beat sweepCenter then
                "#fe57a1"
            else
                "#999999"
    in
        circle [ cx x, cy y, r (toString markerRadius), fill fillColor ] []


intersects : Point -> Point -> Bool
intersects point1 point2 =
    ((point1.x - point2.x) ^ 2 + (point1.y - point2.y) ^ 2) <= markerRadius


updatePosition : Model -> Time -> Model
updatePosition model deltaT =
    let
        angle =
            newAngle model

        center =
            pointOnFace angle
    in
        { model | timeDiff = deltaT, angle = angle, sweepCenter = center }


updateBpm : Model -> Int -> Model
updateBpm model newBpm =
    { model | bpm = newBpm, angle = defaultAngle, speed = (calculateSpeed model.beatCount newBpm) }


updateBeatCount : Model -> Int -> Model
updateBeatCount model beatCount =
    let
        beats =
            calculateBeats beatCount

        speed =
            calculateSpeed beatCount model.bpm
    in
        { model | beatCount = beatCount, beats = beats, angle = defaultAngle, speed = speed }


newAngle : Model -> Float
newAngle model =
    model.angle + model.timeDiff * model.speed


calculateSpeed : Int -> Int -> Float
calculateSpeed beatCount bpm =
    let
        rpm =
            (1 / (toFloat beatCount)) * (toFloat bpm)
    in
        tau * (rpm / 60000)
