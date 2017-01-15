module Main exposing (..)

import Date exposing (Date)
import Date.Extra
import Harvest.Auth exposing (..)
import Harvest.WhoAmI exposing (getUserInfo)
import Harvest.ReportingAPI exposing (DayEntry, getEntriesByUserForDateRange)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra
import Navigation exposing (Location)
import Task
import Time
import Time.DateTime as DateTime
import Dict


-- Model


type alias Model =
    { error : Maybe AppError
    , token : Result String String
    , hours : List DayEntry
    , year : Int
    , flags : Flags
    , selectedYear : Int
    , weeklyWorkingHours : Float
    }


type alias Flags =
    { redirectUrl : String
    , clientId : String
    , account : String
    , overtimeTaskId : Int
    }


type AppError
    = NoToken String
    | HttpError Http.Error


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    ( { error = Nothing
      , token = checkAccessTokenAvailable location.hash (authUrl flags.account flags.clientId flags.redirectUrl)
      , hours = []
      , year = 0
      , flags = flags
      , selectedYear = 0
      , weeklyWorkingHours = 40
      }
    , Task.perform CurrentYear getYear
    )


getYear : Task.Task x Int
getYear =
    Time.now |> Task.andThen (\time -> Task.succeed (DateTime.fromTimestamp time |> DateTime.year))



-- Update


type Msg
    = LocationChange Location
    | GetHours (List DayEntry)
    | LoadHours Int
    | CurrentYear Int
    | UpdateWorkingHours String
    | Failed AppError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange _ ->
            ( model, Cmd.none )

        CurrentYear year ->
            ( { model | year = year, selectedYear = year }, updateHoursWithToken model.flags.account model.token year )

        LoadHours year ->
            ( { model | selectedYear = year }, updateHoursWithToken model.flags.account model.token year )

        GetHours hours ->
            ( { model | hours = hours }, Cmd.none )

        UpdateWorkingHours workingHoursString ->
            let
                workingHours =
                    case String.toFloat workingHoursString of
                        Ok hours ->
                            hours

                        Err _ ->
                            40
            in
                ( { model | weeklyWorkingHours = workingHours }, Cmd.none )

        Failed err ->
            ( { model | error = Just err }, Cmd.none )


updateHoursWithToken : String -> Result String String -> Int -> Cmd Msg
updateHoursWithToken account tokenResult year =
    case tokenResult of
        Ok token ->
            updateHoursForYear account token year

        Err authUrl ->
            Task.perform Failed (Task.succeed (NoToken authUrl))


updateHoursForYear : String -> String -> Int -> Cmd Msg
updateHoursForYear account token year =
    Task.attempt
        (\res ->
            case res of
                Ok res ->
                    GetHours res

                Err err ->
                    Failed err
        )
        (getUserId account token |> Task.andThen (getHoursForEveryCalendarweekInAYear account token year))


getHoursForEveryCalendarweekInAYear : String -> String -> Int -> Int -> Task.Task AppError (List DayEntry)
getHoursForEveryCalendarweekInAYear account token year userId =
    let
        from =
            year |> mondayOfTheFirstWeek |> Date.Extra.toFormattedString "yyyyMMdd"

        to =
            year |> sundayOfTheLastWeek |> Date.Extra.toFormattedString "yyyyMMdd"
    in
        getEntriesByUserForDateRange
            account
            userId
            from
            to
            token
            Dict.empty
            |> Http.toTask
            |> Task.mapError HttpError


getUserId : String -> String -> Task.Task AppError Int
getUserId account token =
    Task.mapError HttpError (getUserInfo account token |> Http.toTask)
        |> Task.andThen (\who -> Task.succeed who.user.id)



-- View


view : Model -> Html Msg
view model =
    div [ class "ma3 sans-serif" ]
        (case model.error of
            Just appError ->
                case appError of
                    NoToken harvestAuthUrl ->
                        [ div [] [ header [ class "tc ph4" ] [ title, loginButton harvestAuthUrl ] ] ]

                    HttpError err ->
                        [ div [] [ text "Network Error" ] ]

            Nothing ->
                let
                    weekEntries =
                        groupByCalendarWeek model.weeklyWorkingHours model.flags.overtimeTaskId model.hours

                    overtimeInHours =
                        overtimeHours weekEntries (overtimeWorked model.flags.overtimeTaskId model.hours)
                in
                    [ header [ class "tc ph4" ] [ title ]
                    , div [ class "tc" ] (List.map (renderYearButton model.selectedYear) (List.range (model.year - 3) model.year))
                    , weeklyHoursInput
                    , monthlyOvertime overtimeInHours
                    ]
                        ++ List.map renderWeek weekEntries
        )


type alias WeekEntry =
    { number : Int, overtime : Float, compensation : Float }


title : Html msg
title =
    h1 [ class "f3 f2-m f1-l fw2 black-90 mv3" ] [ text "Harvest Overtime Calculator" ]


loginButton : String -> Html msg
loginButton harvestAuthUrl =
    a [ href harvestAuthUrl, class "f6 link dim ba ph3 pv2 mb2 dib black" ] [ text "Login with Harvest" ]


monthlyOvertime : Float -> Html msg
monthlyOvertime overtime =
    div [ class "f3 f2-m f1-l fw2 black-90 pa3 mv3 bg-light-gray tc shadow-1" ] [ text (toString overtime ++ "h ") ]


weeklyHoursInput : Html Msg
weeklyHoursInput =
    div [ class "ma4-l black-80 absolute-l right-0" ]
        [ label [ class "f6 b db mb2 tr-l fw2", for "workingHours" ] [ text "Weekly Working Hours" ]
        , input [ id "workingHours", class "tr-l input-reset ba b--black-20 pa2 mb2 db w-100", type_ "number", placeholder "40", onInput UpdateWorkingHours ] []
        ]


renderYearButton : Int -> Int -> Html Msg
renderYearButton selectedYear year =
    button
        [ onClick (LoadHours year)
        , class
            ("f6 link dim ba ph3 pv2 mb2 ma1 pointer"
                ++ if year == selectedYear then
                    " bg-light-gray"
                   else
                    " bg-white"
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
        div [ class "fl w-10-ns pa2 shadow-1 ma2 grow tc" ]
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


groupByCalendarWeek : Float -> Int -> List DayEntry -> List WeekEntry
groupByCalendarWeek capacity overtimeTaskId hours =
    List.indexedMap
        (\i ds -> WeekEntry (i + 1) (totalHours ds - capacity) (overtimeWorked overtimeTaskId ds))
        (List.Extra.groupWhile (\h1 h2 -> Date.Extra.weekNumber h1.spentAt == Date.Extra.weekNumber h2.spentAt) hours)


overtimeWorked : Int -> List DayEntry -> Float
overtimeWorked overtimeTaskId hours =
    List.filter (\hour -> hour.taskId == overtimeTaskId) hours |> totalHours


totalHours : List DayEntry -> Float
totalHours hours =
    List.map .hours hours |> List.foldl (+) 0


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange
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
