module Main exposing (..)

import Date.Extra
import Harvest.Api exposing (..)
import Harvest.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra
import Navigation exposing (Location)
import Task
import Time
import Time.DateTime as DateTime
import Config as Config


-- Model


type alias Model =
    { hours : List DayEntry
    , error : Maybe AppError
    }


type AppError
    = NoToken String
    | HttpError Http.Error


init : Location -> ( Model, Cmd Msg )
init location =
    let
        authenticationUrl =
            authUrl Config.account Config.clientId Config.redirectUrl

        loadHoursForCurrentYear =
            Task.mapError NoToken (checkAccessTokenAvailable location.hash authenticationUrl)
                |> Task.andThen
                    (\token ->
                        Task.mapError HttpError (getUserInfo Config.account token |> Http.toTask)
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


handleLoadedHours : Result AppError (List DayEntry) -> Msg
handleLoadedHours loadedHours =
    case loadedHours of
        Ok res ->
            Hours res

        Err err ->
            Failed err


getHoursForCurrentYear : String -> Int -> Time.Time -> Task.Task AppError (List DayEntry)
getHoursForCurrentYear token userId time =
    getDailyHoursForDateRange
        Config.account
        (toString userId)
        (getCurrentYearFromTime time ++ "0103")
        (getCurrentYearFromTime time ++ "1231")
        token
        |> Http.toTask
        |> Task.mapError HttpError


getCurrentYearFromTime : Time.Time -> String
getCurrentYearFromTime time =
    DateTime.fromTimestamp time |> DateTime.year |> toString



-- Update


type Msg
    = LocationChange Location
    | Hours (List DayEntry)
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
                    NoToken harvestAuthUrl ->
                        [ div [] [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ] ] ]

                    HttpError err ->
                        [ div [] [ text ("Network Error" ++ toString err) ] ]

            Nothing ->
                [ h3 [] [ text ("2016 Overtime " ++ toString (overtimeHours model.hours) ++ "h " ++ toString ((overtimeHours model.hours) / 8.0) ++ "d") ]
                , ul [] (List.map renderWeek (groupByCalendarWeek model.hours))
                ]
        )


type alias WeekEntry =
    { number : Int, hours : Float, overtime : Float }


overtimeHours : List DayEntry -> Float
overtimeHours hours =
    (List.foldl (+) 0 (List.map .overtime (groupByCalendarWeek hours))) - (overtimeWorked hours)


renderWeek : WeekEntry -> Html msg
renderWeek weekEntry =
    let
        { number, hours, overtime } =
            weekEntry
    in
        li [] [ "Week " ++ toString number ++ ": " ++ toString hours ++ "h " ++ "Overtime:" ++ toString overtime ++ "h" |> text ]


groupByCalendarWeek : List DayEntry -> List WeekEntry
groupByCalendarWeek hours =
    List.indexedMap
        (\i ds -> WeekEntry (i + 1) (totalHours ds) (totalHours ds - 40))
        (List.Extra.groupWhile (\h1 h2 -> Date.Extra.weekNumber h1.spentAt == Date.Extra.weekNumber h2.spentAt) hours)


overtimeWorked : List DayEntry -> Float
overtimeWorked hours =
    List.filter (\hour -> (toString hour.taskId) == overtimeTaskId) hours |> totalHours


totalHours : List DayEntry -> Float
totalHours hours =
    List.map .hours hours |> List.foldl (+) 0


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


overtimeTaskId : String
overtimeTaskId =
    "2842526"
