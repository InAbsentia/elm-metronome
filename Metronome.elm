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
    , timeDiff : Time
    , beats : Int
    , noteLength : Int
    , bpm : Int
    , speed : Float
    }


defaultAngle =
    ((3 * pi) / 2)


defaultBeats =
    4


defaultNoteLength =
    4


defaultBpm =
    60


init : ( Model, Cmd Msg )
init =
    let
        timeDiff =
            0

        beats =
            defaultBeats

        noteLength =
            defaultNoteLength

        bpm =
            defaultBpm

        speed =
            calculateSpeed beats bpm
    in
        ( (Model defaultAngle timeDiff beats noteLength bpm speed), Cmd.none )


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
            , div []
                [ label []
                    [ Html.text "Time Signature"
                    , input [ type_ "text", disabled True, value (toString model.beats) ] []
                    , Html.text " / "
                    , input [ type_ "text", disabled True, value (toString model.noteLength) ] []
                    ]
                ]
            , svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", stroke "#bada55", fill "none" ] []
                , circle [ cx newX, cy newY, r "3", stroke "#666666", fill "none" ] []
                ]
            ]


updatePosition : Model -> Time -> Model
updatePosition model deltaT =
    { model | timeDiff = deltaT, angle = (newAngle model) }


updateBpm : Model -> Int -> Model
updateBpm model newBpm =
    { model | bpm = newBpm, angle = defaultAngle, speed = (calculateSpeed model.beats newBpm) }


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
