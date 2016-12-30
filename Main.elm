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
import Dict exposing (empty, update)


-- Model


type alias Model =
    { hours : List DayEntry
    , error : Maybe AppError
    }


type AppError
    = NoToken String
    | HttpError Http.Error



{- TO BE USED -}


type alias WeeklyHours =
    { hours : Float
    , overtime : Float
    }


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
    Date.fromTimestamp time |> Date.year |> toString



--foldl : (a -> b -> b) -> b -> List a -> b


aggregate : DayEntry -> Dict.Dict Int Float -> Dict.Dict Int Float
aggregate hour dict =
    let
        entry =
            Dict.get hour.weekNumber dict
    in
        case entry of
            Just hrs ->
                Dict.insert hour.weekNumber (hour.hours + hrs) dict

            Nothing ->
                Dict.insert hour.weekNumber hour.hours dict


weeklyHours : List DayEntry -> Dict.Dict Int Float
weeklyHours hours =
    List.foldl aggregate Dict.empty hours



{- TO BE USED -}


calculateHours : DayEntry -> WeeklyHours -> WeeklyHours
calculateHours dayEntry weekHrs =
    if (toString dayEntry.taskId) == overtimeTaskId then
        { weekHrs | hours = weekHrs.hours + dayEntry.hours, overtime = weekHrs.overtime - dayEntry.hours }
    else
        { weekHrs | hours = weekHrs.hours + dayEntry.hours, overtime = weekHrs.overtime - dayEntry.hours }



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
                , h3 [] [ text ((Debug.log "Weeks" (weeklyHours model.hours) |> Dict.size |> toString) ++ " weeks") ]
                ]
        )


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
