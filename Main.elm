module Main exposing (..)

import Date exposing (Date)
import Date.Extra
import Harvest.Auth exposing (..)
import Harvest.WhoAmI exposing (getUserInfo)
import Harvest.ReportingAPI exposing (DayEntry, getEntriesByUserForDateRange)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra
import Navigation exposing (Location)
import Task
import Time
import Time.DateTime as DateTime
import Config as Config
import Dict


-- Model


type Model
    = HoursPage (List DayEntry)
    | ErrorPage AppError


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
        ( HoursPage []
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
    let
        from =
            DateTime.fromTimestamp time
                |> DateTime.year
                |> mondayOfTheFirstWeek
                |> Date.Extra.toFormattedString "yyyyMMdd"

        to =
            DateTime.fromTimestamp time
                |> DateTime.year
                |> sundayOfTheLastWeek
                |> Date.Extra.toFormattedString "yyyyMMdd"
    in
        getEntriesByUserForDateRange
            Config.account
            (toString userId)
            from
            to
            token
            Dict.empty
            |> Http.toTask
            |> Task.mapError HttpError



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
            ( HoursPage hours, Cmd.none )

        Failed err ->
            ( ErrorPage err, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "ma3 sans-serif" ]
        (case model of
            ErrorPage appError ->
                case appError of
                    NoToken harvestAuthUrl ->
                        [ div [] [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ] ] ]

                    HttpError err ->
                        [ div [] [ text ("Network Error" ++ toString err) ] ]

            HoursPage hours ->
                let
                    weekEntries =
                        groupByCalendarWeek hours

                    overtimeInHours =
                        overtimeHours weekEntries (overtimeWorked hours)
                in
                    [ h1 [ class "f2 lh-title tc" ] [ text "Harvest Overtime Calculator" ]
                    , div [class "f1 ma2 pa3 bg-light-gray tc shadow-1"] [ text (toString overtimeInHours ++ "h ") ]
                    ]
                        ++ List.map renderWeek weekEntries
        )


type alias WeekEntry =
    { number : Int, overtime : Float }


overtimeHours : List WeekEntry -> Float -> Float
overtimeHours weekEntries overtimeWorked =
    (List.foldl (+) 0 (List.map .overtime weekEntries)) - overtimeWorked


renderWeek : WeekEntry -> Html msg
renderWeek weekEntry =
    let
        { number, overtime } =
            weekEntry
    in
        div [ class "fl w-10 pa2 shadow-1 ma2 grow tc" ]
            [ div [ class "f5" ] [ "Week " ++ toString number |> text ]
            , div [ class "f3" ] [ toString overtime ++ "h" |> text ]
            ]


groupByCalendarWeek : List DayEntry -> List WeekEntry
groupByCalendarWeek hours =
    List.indexedMap
        (\i ds -> WeekEntry (i + 1) (totalHours ds - Config.capacity))
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


mondayOfTheFirstWeek : Int -> Date
mondayOfTheFirstWeek yr =
    let
        d =
            Date.Extra.fromCalendarDate (yr - 1) Date.Jan 1
    in
        if Date.Extra.weekNumber d /= 1 then
            Date.Extra.add Date.Extra.Day (8 - Date.Extra.weekdayNumber d) d
        else
            Date.Extra.add Date.Extra.Day (1 - Date.Extra.weekdayNumber d) d


sundayOfTheLastWeek : Int -> Date
sundayOfTheLastWeek yr =
    let
        d =
            Date.Extra.fromCalendarDate yr Date.Jan 1
    in
        if Date.Extra.weekNumber d /= 1 then
            Date.Extra.add Date.Extra.Day (7 - Date.Extra.weekdayNumber d) d
        else
            Date.Extra.add Date.Extra.Day (0 - Date.Extra.weekdayNumber d) d
