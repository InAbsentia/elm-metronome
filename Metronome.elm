module Metronome exposing (..)

import Html exposing (Html, div, input, label)
import Html.Attributes exposing (value)
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
    , timeDiff : Time
    , beats : Int
    , bpm : Int
    , speed : Float
    }


init : ( Model, Cmd Msg )
init =
    let
        angle =
            ((3 * pi) / 2)

        timeDiff =
            0

        beats =
            4

        bpm =
            60

        speed =
            calculateSpeed beats bpm
    in
        ( (Model angle timeDiff beats bpm speed), Cmd.none )


type Msg
    = TimeUpdate Time
    | BpmUpdate String


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


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs TimeUpdate


view : Model -> Html Msg
view model =
    let
        newX =
            toString (50 + 45 * cos model.angle)

        newY =
            toString (50 + 45 * sin model.angle)
    in
        div []
            [ div []
                [ label []
                    [ Html.text "BPM"
                    , input [ type_ "text", value (toString model.bpm), onInput BpmUpdate ] []
                    ]
                ]
            , svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", fill "#bada55" ] []
                , circle [ cx newX, cy newY, r "3", fill "#666666" ] []
                ]
            ]


updatePosition : Model -> Time -> Model
updatePosition model deltaT =
    { model | timeDiff = deltaT, angle = (newAngle model) }


updateBpm : Model -> Int -> Model
updateBpm model newBpm =
    { model | bpm = newBpm, speed = (calculateSpeed model.beats newBpm) }


newAngle : Model -> Float
newAngle model =
    model.angle + model.timeDiff * model.speed


calculateSpeed : Int -> Int -> Float
calculateSpeed beats bpm =
    let
        rpm =
            (1 / (toFloat beats)) * (toFloat bpm)

        tau =
            (2 * pi)
    in
        tau * (rpm / 60000)
