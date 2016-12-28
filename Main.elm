module Main exposing (..)

import Harvest.Api exposing (..)
import Harvest.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Navigation exposing (Location)
import Task
import Time
import Time.DateTime as Date


-- Model


type alias Model =
    { access_token : Maybe String
    , hours : List DailyHours
    , error : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        token =
            getTokenFromHash location.hash

        loadHoursForCurrentYear =
            case token of
                Just aToken ->
                    Http.toTask (getUserInfo aToken)
                        |> Task.andThen
                            (\who ->
                                Task.map2
                                    (getHoursForCurrentYear aToken)
                                    (Task.succeed who.user.id)
                                    Time.now
                            )
                        |> Task.andThen identity

                Nothing ->
                    Task.fail (Http.BadUrl "No Access Token found.")
    in
        ( { access_token = token
          , hours = []
          , error = ""
          }
        , Task.attempt handleLoadedHours loadHoursForCurrentYear
        )


handleLoadedHours : Result Http.Error (List DailyHours) -> Msg
handleLoadedHours loadedHours =
    case loadedHours of
        Ok res ->
            Hours res

        Err err ->
            Failed err


getHoursForCurrentYear : String -> Int -> Time.Time -> Task.Task Http.Error (List DailyHours)
getHoursForCurrentYear token userId time =
    getDailyHoursForDateRange
        (toString userId)
        (getCurrentYearFromTime time ++ "0101")
        (getCurrentYearFromTime time ++ "1231")
        token
        |> Http.toTask


getCurrentYearFromTime : Time.Time -> String
getCurrentYearFromTime time =
    Date.fromTimestamp time |> Date.year |> toString



-- Update


type Msg
    = LocationChange Location
    | Hours (List DailyHours)
    | Failed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange _ ->
            ( model, Cmd.none )

        Hours hours ->
            ( { model | hours = hours }, Cmd.none )

        Failed err ->
            ( { model | error = toString err }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model.access_token of
        Just token ->
            div [ style [ ( "margin", "1rem" ) ] ]
                [ h3 [] [ text (totalHours model.hours ++ " hours worked") ]
                , div [] [ text model.error ]
                ]

        Nothing ->
            renderLoginButton


renderLoginButton : Html Msg
renderLoginButton =
    div []
        [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ]
        ]


totalHours : List DailyHours -> String
totalHours hours =
    List.map .hours hours |> List.foldl (+) 0 |> toString


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
