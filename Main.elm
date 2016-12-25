module Main exposing (..)

import HarvestAPI exposing (..)
import HarvestTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Navigation exposing (Location)


-- Model


type alias Model =
    { location : Location
    , access_token : Maybe String
    , res : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initCmd =
            case getTokenFromHash location.hash of
                Just token ->
                    Http.send Daily (getDaily token)

                Nothing ->
                    Cmd.none
    in
        ( { location = location
          , access_token = Nothing
          , res = ""
          }
        , initCmd
        )



-- Update


type Msg
    = LocationChange Location
    | Daily (Result Http.Error HarvestTypes.Daily)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )

        Daily (Ok daily) ->
            ( { model | res = toString daily }, Cmd.none )

        Daily (Err _) ->
            ( { model | access_token = Nothing }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model.access_token of
        Just token ->
            div []
                [ h3 [] [ text model.location.href ]
                , div [] [ text token ]
                ]

        Nothing ->
            renderLoginButton


renderLoginButton : Html Msg
renderLoginButton =
    div []
        [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ]
        ]


harvestAuthUrl : String
harvestAuthUrl =
    "https://comsysto.harvestapp.com/oauth2/authorize?response_type=token&immediate=true&approval_prompt=auto&client_id=wvIOerEB7xWVfzrSsge3zw&redirect_uri=http%3A%2F%2Flocalhost%3A8000%2FMain.elm"


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
