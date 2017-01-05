module Metronome exposing (..)

import Html exposing (Html)
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
    { angle : Float,
      timeDiff : Time,
      bpm : Int
    }

init : (Model, Cmd Msg)
init =
    ((Model ((3 * pi) / 2) 0 60), Cmd.none)

type Msg
    = TimeUpdate Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeUpdate deltaT ->
            ((updateModel model deltaT), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs TimeUpdate

view : Model -> Html Msg
view model =
    let newX =
            toString (50 + 45 * cos model.angle)

        newY =
            toString (50 + 45 * sin model.angle)

    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            [ circle [cx "50", cy "50", r "45", fill "#bada55"] []
            , circle [cx newX, cy newY, r "3", fill "#666666"] []]

updateModel : Model -> Time -> Model
updateModel model deltaT =
    { model | timeDiff = deltaT }
    |> updateAngle

updateAngle : Model -> Model
updateAngle model =
    { model | angle = model.angle + (model.timeDiff / (toFloat model.bpm)) }
