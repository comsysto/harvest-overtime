module Main exposing (..)

import Html exposing (..)
import Navigation exposing (Location)


-- Model


type alias Model =
    { location : Location
    }


init : Location -> ( Model, Cmd Msg )
init location =
    ( { location = location
      }
    , Cmd.none
    )



-- Update


type Msg
    = LocationChange Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text model.location.href ]
        ]


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
