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
    , beatCount : Int
    , noteLength : Int
    , bpm : Int
    , speed : Float
    , beats : List Beat
    }


type alias Beat =
    { x : Float
    , y : Float
    }


defaultAngle =
    ((3 * pi) / 2)


defaultBeatCount =
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

        beatCount =
            defaultBeatCount

        noteLength =
            defaultNoteLength

        bpm =
            defaultBpm

        speed =
            calculateSpeed beatCount bpm

        beats =
            calculateBeats beatCount
    in
        ( (Model defaultAngle timeDiff beatCount noteLength bpm speed beats), Cmd.none )


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
                    , input [ type_ "text", disabled True, value (toString model.beatCount) ] []
                    , Html.text " / "
                    , input [ type_ "text", disabled True, value (toString model.noteLength) ] []
                    ]
                ]
            , svg [ viewBox "0 0 100 100", width "300px" ] (buildFace ( newX, newY ) model.beats)
            ]


buildFace : ( String, String ) -> List Beat -> List (Svg Msg)
buildFace ( newX, newY ) beats =
    List.concat
        [ [ circle [ cx "50", cy "50", r "45", stroke "#BADA55", fill "none" ] [] ]
        , (beatMarkers beats)
        , [ circle [ cx newX, cy newY, r "3", stroke "#666666", fill "none" ] [] ]
        ]


calculateBeats : Int -> List Beat
calculateBeats n =
    beatReducer [] defaultAngle (360 / toFloat n) n


beatReducer accumulator position increment counter =
    if counter <= 0 then
        accumulator
    else
        let
            beat =
                buildBeat position

            nextAngle =
                position + degrees increment

            nextCounter =
                counter - 1
        in
            beatReducer (beat :: accumulator) nextAngle increment nextCounter


buildBeat : Float -> Beat
buildBeat angle =
    let
        x =
            50 + 45 * cos angle

        y =
            50 + 45 * sin angle
    in
        Beat x y


beatMarkers : List Beat -> List (Svg Msg)
beatMarkers beats =
    List.map buildMarker beats


buildMarker : Beat -> Svg Msg
buildMarker beat =
    let
        markerX =
            toString beat.x

        markerY =
            toString beat.y
    in
        circle [ cx markerX, cy markerY, r "3", fill "#999999" ] []


updatePosition : Model -> Time -> Model
updatePosition model deltaT =
    { model | timeDiff = deltaT, angle = (newAngle model) }


updateBpm : Model -> Int -> Model
updateBpm model newBpm =
    { model | bpm = newBpm, angle = defaultAngle, speed = (calculateSpeed model.beatCount newBpm) }


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
