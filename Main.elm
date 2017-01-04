module Main exposing (..)

import Date exposing (Date)
import Date.Extra
import Harvest.Auth exposing (..)
import Harvest.WhoAmI exposing (getUserInfo)
import Harvest.ReportingAPI exposing (DayEntry, getEntriesByUserForDateRange)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Navigation exposing (Location)
import Task
import Time
import Time.DateTime as DateTime
import Config as Config
import Dict


-- Model


type alias Model =
    { error : Maybe AppError, token : Result String String, hours : List DayEntry, year : Int, selectedYear : Int }


type AppError
    = NoToken String
    | HttpError Http.Error


init : Location -> ( Model, Cmd Msg )
init location =
    ( { error = Nothing
      , token = checkAccessTokenAvailable location.hash authenticationUrl
      , hours = []
      , year = 0
      , selectedYear = 0
      }
    , Task.perform CurrentYear getYear
    )


getYear : Task.Task x Int
getYear =
    Time.now |> Task.andThen (\time -> Task.succeed (DateTime.fromTimestamp time |> DateTime.year))


authenticationUrl : String
authenticationUrl =
    authUrl Config.account Config.clientId Config.redirectUrl



-- Update


type Msg
    = LocationChange Location
    | GetHours (List DayEntry)
    | LoadHours Int
    | CurrentYear Int
    | Failed AppError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange _ ->
            ( model, Cmd.none )

        CurrentYear year ->
            ( { model | year = year, selectedYear = year }, updateHoursWithToken model.token year )

        LoadHours year ->
            ( { model | selectedYear = year }, updateHoursWithToken model.token year )

        GetHours hours ->
            ( { model | hours = hours }, Cmd.none )

        Failed err ->
            ( { model | error = Just err }, Cmd.none )


updateHoursWithToken : Result String String -> Int -> Cmd Msg
updateHoursWithToken tokenResult year =
    case tokenResult of
        Ok token ->
            updateHoursForYear token year

        Err authUrl ->
            Task.perform Failed (Task.succeed (NoToken authUrl))


updateHoursForYear : String -> Int -> Cmd Msg
updateHoursForYear token year =
    Task.attempt
        (\res ->
            case res of
                Ok res ->
                    GetHours res

                Err err ->
                    Failed err
        )
        (getUserId token |> Task.andThen (getHoursForEveryCalendarweekInAYear token year))


getHoursForEveryCalendarweekInAYear : String -> Int -> Int -> Task.Task AppError (List DayEntry)
getHoursForEveryCalendarweekInAYear token year userId =
    let
        from =
            year |> mondayOfTheFirstWeek |> Date.Extra.toFormattedString "yyyyMMdd"

        to =
            year |> sundayOfTheLastWeek |> Date.Extra.toFormattedString "yyyyMMdd"
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


getUserId : String -> Task.Task AppError Int
getUserId token =
    Task.mapError HttpError (getUserInfo Config.account token |> Http.toTask)
        |> Task.andThen (\who -> Task.succeed who.user.id)



-- View


view : Model -> Html Msg
view model =
    div [ class "ma3 sans-serif" ]
        (case model.error of
            Just appError ->
                case appError of
                    NoToken harvestAuthUrl ->
                        [ div [] [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ] ] ]

                    HttpError err ->
                        [ div [] [ text ("Network Error" ++ toString err) ] ]

            Nothing ->
                let
                    weekEntries =
                        groupByCalendarWeek model.hours

                    overtimeInHours =
                        overtimeHours weekEntries (overtimeWorked model.hours)
                in
                    [ h1 [ class "f2 lh-title tc" ] [ text "Harvest Overtime Calculator" ]
                    , div [ class "tc" ] (List.map (renderYearButton model.selectedYear) (List.range (model.year - 3) model.year))
                    , div [ class "f1 ma2 pa3 bg-light-gray tc shadow-1" ] [ text (toString overtimeInHours ++ "h ") ]
                    ]
                        ++ List.map renderWeek weekEntries
        )


type alias WeekEntry =
    { number : Int, overtime : Float, compensation : Float }


renderYearButton : Int -> Int -> Html Msg
renderYearButton selectedYear year =
    button
        [ onClick (LoadHours year)
        , class
            ("pa3 ma2 br2 shadow-1 pointer"
                ++ if year == selectedYear then
                    " bg-light-gray"
                   else
                    ""
            )
        , type_ "button"
        ]
        [ year |> toString |> text ]


overtimeHours : List WeekEntry -> Float -> Float
overtimeHours weekEntries overtimeWorked =
    (List.foldl (+) 0 (List.map .overtime weekEntries)) - overtimeWorked


renderWeek : WeekEntry -> Html msg
renderWeek weekEntry =
    let
        { number, overtime, compensation } =
            weekEntry
    in
        div [ class "fl w-10 pa2 shadow-1 ma2 grow tc" ]
            [ div [ class "f5" ] [ "Week " ++ toString number |> text ]
            , div [ class "f3" ]
                ([ span [] [ toString overtime ++ "h" |> text ]
                 , span [ class "red" ]
                    [ (if compensation > 0 then
                        " -" ++ (compensation |> toString) ++ "h"
                       else
                        ""
                      )
                        |> text
                    ]
                 ]
                )
            ]


groupByCalendarWeek : List DayEntry -> List WeekEntry
groupByCalendarWeek hours =
    List.indexedMap
        (\i ds -> WeekEntry (i + 1) (totalHours ds - Config.capacity) (totalOvertimeCompensation ds))
        (List.Extra.groupWhile (\h1 h2 -> Date.Extra.weekNumber h1.spentAt == Date.Extra.weekNumber h2.spentAt) hours)


totalOvertimeCompensation : List DayEntry -> Float
totalOvertimeCompensation hours =
    (List.filter (\d -> d.taskId == Config.overtimeTaskId) hours) |> totalHours


overtimeWorked : List DayEntry -> Float
overtimeWorked hours =
    List.filter (\hour -> hour.taskId == Config.overtimeTaskId) hours |> totalHours


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


mondayOfTheFirstWeek : Int -> Date
mondayOfTheFirstWeek yr =
    let
        d =
            Date.Extra.fromCalendarDate yr Date.Jan 1
    in
        if Date.Extra.weekNumber d /= 1 then
            Date.Extra.add Date.Extra.Day (8 - Date.Extra.weekdayNumber d) d
        else
            Date.Extra.add Date.Extra.Day (1 - Date.Extra.weekdayNumber d) d


sundayOfTheLastWeek : Int -> Date
sundayOfTheLastWeek yr =
    let
        d =
            Date.Extra.fromCalendarDate (yr + 1) Date.Jan 1
    in
        if Date.Extra.weekNumber d /= 1 then
            Date.Extra.add Date.Extra.Day (7 - Date.Extra.weekdayNumber d) d
        else
            Date.Extra.add Date.Extra.Day (0 - Date.Extra.weekdayNumber d) d
