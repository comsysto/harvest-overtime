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
    , res : Maybe Daily
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        token =
            getTokenFromHash location.hash

        initCmd =
            case token of
                Just aToken ->
                    Http.send Daily (getDailyForDate aToken "50" "2016")

                Nothing ->
                    Cmd.none
    in
        ({ location = location
         , access_token = token
         , res = Nothing
         }
            ! [ Navigation.modifyUrl "#", initCmd ]
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
            ( { model | res = Just daily }, Cmd.none )

        Daily (Err _) ->
            ( { model | access_token = Nothing }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model.access_token of
        Just token ->
            div [ style [ ( "margin", "1rem" ) ] ] (renderDaily model.res)

        Nothing ->
            renderLoginButton


renderDaily : Maybe Daily -> List (Html Msg)
renderDaily daily =
    case daily of
        Just daily ->
            [ h3 [] [ text (toString (.forDay daily)) ] ]
                ++ renderEntries daily
                ++ renderProjectList daily

        Nothing ->
            []


renderEntries : Daily -> List (Html Msg)
renderEntries daily =
    [ h3 [] [ text "Entries" ]
    , ul [] (List.map (\entry -> li [] [ text (.task entry ++ " " ++ toString (.hours entry)) ]) daily.dayEntries)
    ]


renderProjectList : Daily -> List (Html Msg)
renderProjectList daily =
    [ h3 [] [ text "Projects" ]
    , ul [] (List.map (\project -> li [] [ text (.name project) ]) daily.projects)
    ]


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
