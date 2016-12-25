module Main exposing (..)

import HarvestAPI exposing (..)
import Html exposing (..)
import Http
import Navigation exposing (Location)


-- Model


type alias Model =
    { location : Location
    , res : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initCmd =
            case getAccessTokenFromHash location.hash of
                Just token ->
                    Http.send Daily (getDaily token)

                Nothing ->
                    Cmd.none
    in
        ( { location = location
          , res = ""
          }
        , initCmd
        )



-- Update


type Msg
    = LocationChange Location
    | Daily (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )

        Daily (Ok res) ->
            ( { model | res = res }, Cmd.none )

        Daily (Err _) ->
            ( { model | res = "Error" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text model.location.href ]
        , div [] [ text model.res ]
        ]


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
