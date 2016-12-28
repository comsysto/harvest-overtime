module Main exposing (..)

import Harvest.Api exposing (..)
import Harvest.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Navigation exposing (Location)
import Task


-- Model


type alias Model =
    { location : Location
    , access_token : Maybe String
    , hours : List DailyHours
    , error : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        token =
            getTokenFromHash location.hash

        initCmd =
            case token of
                Just aToken ->
                    Http.toTask (getUserInfo aToken)
                        |> Task.andThen (\who -> Task.succeed who.user.id)
                        |> Task.andThen
                            (\userId ->
                                Http.toTask
                                    (getDailyHoursForDateRange
                                        (toString userId)
                                        "20161219"
                                        "20161225"
                                        aToken
                                    )
                            )

                Nothing ->
                    Task.fail (Http.BadUrl "No Access Token found.")
    in
        ( { location = location
          , access_token = token
          , hours = []
          , error = ""
          }
        , Task.attempt getHours initCmd
        )


getHours : Result Http.Error (List DailyHours) -> Msg
getHours result =
    case result of
        Ok res ->
            Hours res

        Err err ->
            Failed err



-- Update


type Msg
    = LocationChange Location
    | Hours (List DailyHours)
    | Failed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )

        Hours hours ->
            ( { model | hours = hours }, Cmd.none )

        Failed err ->
            ( { model | error = toString err }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model.access_token of
        Just token ->
            div []
                [ div [ style [ ( "margin", "1rem" ) ] ] [ renderHours model.hours ]
                , hr [] []
                , div [ style [ ( "margin", "1rem" ) ] ] [ text model.error ]
                ]

        Nothing ->
            renderLoginButton


renderLoginButton : Html Msg
renderLoginButton =
    div []
        [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ]
        ]


renderHours : List DailyHours -> Html Msg
renderHours hours =
    div []
        [ h2 [] [ text "Hours for the week 2016-12-19 till 2016-12-25 (dates hardcoded)" ]
        , List.map renderHour hours |> ul []
        ]


renderHour : DailyHours -> Html Msg
renderHour hour =
    li []
        [ div []
            [ span [] [ text "Hours: " ]
            , span [] [ text <| toString hour.hours ]
            ]
        , div []
            [ span [] [ text "Spent at: " ]
            , span [] [ text hour.spent_at ]
            ]
        , div []
            [ span [] [ text "Billed: " ]
            , span [] [ text <| toString hour.is_billed ]
            ]
        , div []
            [ span [] [ text "Closed: " ]
            , span [] [ text <| toString hour.is_closed ]
            ]
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
