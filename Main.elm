module Main exposing (..)

import Date
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
        loadHoursForCurrentYear =
            Task.mapError NoToken (getTokenFromHash location.hash)
                |> Task.andThen
                    (\token ->
                        Task.mapError HttpError (getUserInfo token |> Http.toTask)
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
        (toString userId)
        (getCurrentYearFromTime time ++ "0101")
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
                [ h3 [] [ text ((totalHours model.hours |> toString) ++ " hours worked") ]
                , h3 [] [ text ((overtimeHours model.hours |> toString) ++ " hours overtime") ]
                , ul [] (List.map (\e -> li [] [ e |> toString |> text ]) (groupByCalendarWeek model.hours))
                ]
        )


groupByCalendarWeek : List DayEntry -> List ( Maybe String, String, Maybe String )
groupByCalendarWeek hours =
    List.map
        (\ds ->
            ( (List.head ds |> Maybe.andThen (\d -> Just d.spentAt) |> Maybe.andThen (\d -> Just (toString (Date.day d) ++ toString (Date.month d) ++ toString (Date.year d))))
            , (List.foldr (+) 0 (List.map .hours ds) |> toString) ++ " hours"
            , (List.head ds |> Maybe.andThen (\d -> Just ("Week: " ++ (toString (Date.Extra.weekNumber d.spentAt)))))
            )
        )
        (List.Extra.groupWhile (\h1 h2 -> Date.Extra.weekNumber h1.spentAt == Date.Extra.weekNumber h2.spentAt) hours)


overtimeHours : List DayEntry -> Float
overtimeHours hours =
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
