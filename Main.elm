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
    { hours : List DailyHours
    , error : Maybe AppError
    }


type AppError
    = NoToken Bool
    | HttpError Http.Error


init : Location -> ( Model, Cmd Msg )
init location =
    let
        loadHoursForCurrentYear =
            Task.mapError NoToken (getTokenFromHash location.hash)
                |> Task.andThen
                    (\token ->
                        Task.mapError HttpError (Http.toTask (getUserInfo token))
                            |> Task.andThen
                                (\who ->
                                    Task.map2
                                        (getHoursForCurrentYear token)
                                        (Task.succeed who.user.id)
                                        Time.now
                                )
                    )
                |> Task.andThen identity
    in
        ( { hours = []
          , error = Nothing
          }
        , Task.attempt handleLoadedHours loadHoursForCurrentYear
        )


handleLoadedHours : Result AppError (List DailyHours) -> Msg
handleLoadedHours loadedHours =
    case loadedHours of
        Ok res ->
            Hours res

        Err err ->
            Failed err


getHoursForCurrentYear : String -> Int -> Time.Time -> Task.Task AppError (List DailyHours)
getHoursForCurrentYear token userId time =
    getDailyHoursForDateRange
        (toString userId)
        (getCurrentYearFromTime time ++ "0101")
        (getCurrentYearFromTime time ++ "1231")
        token
        |> Http.toTask
        |> Task.mapError HttpError


getCurrentYearFromTime : Time.Time -> String
getCurrentYearFromTime time =
    Date.fromTimestamp time |> Date.year |> toString



-- Update


type Msg
    = LocationChange Location
    | Hours (List DailyHours)
    | Failed AppError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange _ ->
            ( model, Cmd.none )

        Hours hours ->
            ( { model | hours = hours, error = Nothing }, Cmd.none )

        Failed err ->
            ( { model | error = Just err }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "1rem" ) ] ]
        (case model.error of
            Just appError ->
                case appError of
                    NoToken _ ->
                        [ div [] [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ] ] ]

                    HttpError err ->
                        [ div [] [ text "Network Error" ] ]

            Nothing ->
                [ h3 [] [ text (totalHours model.hours ++ " hours worked") ] ]
        )


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
