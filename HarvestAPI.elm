module HarvestAPI exposing (getDaily, getTokenFromHash, getDailyForDate, getUserInfo, getDailyHoursForDateRange)

import Dict
import HarvestTypes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


-- Timesheets


getDaily : String -> Request Daily
getDaily token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://comsysto.harvestapp.com/daily?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson daily
        , timeout = Nothing
        , withCredentials = False
        }


getDailyForDate : String -> String -> String -> Request Daily
getDailyForDate token dayOfYear year =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url =
            "https://comsysto.harvestapp.com/daily/"
                ++ dayOfYear
                ++ "/"
                ++ year
                ++ "?access_token="
                ++ token
        , body = emptyBody
        , expect = expectJson daily
        , timeout = Nothing
        , withCredentials = False
        }


getDailyHoursForDateRange : String -> String -> String -> String -> Request (List DailyHours)
getDailyHoursForDateRange user from to token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url =
            "https://comsysto.harvestapp.com/people/"
                ++ user
                ++ "/entries?from="
                ++ from
                ++ "&to="
                ++ to
                ++ "&access_token="
                ++ token
        , body = emptyBody
        , expect = expectJson hours
        , timeout = Nothing
        , withCredentials = False
        }


getUserInfo : String -> Request WhoAmI
getUserInfo token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url =
            "https://comsysto.harvestapp.com/account/who_am_i?access_token="
                ++ token
        , body = emptyBody
        , expect = expectJson whoAmI
        , timeout = Nothing
        , withCredentials = False
        }


daily : Decoder Daily
daily =
    decode Daily
        |> required "day_entries" (list dayEntry)
        |> required "for_day" string
        |> required "projects" (list project)


dayEntry : Decoder DayEntry
dayEntry =
    decode DayEntry
        |> required "project_id" string
        |> required "project" string
        |> required "user_id" int
        |> required "spent_at" string
        |> required "task_id" string
        |> required "task" string
        |> required "client" string
        |> required "id" int
        |> required "notes" string
        |> required "created_at" string
        |> required "updated_at" string
        |> required "hours_without_timer" float
        |> required "hours" float


project : Decoder Project
project =
    decode Project
        |> required "id" int
        |> required "client_id" int
        |> required "client" string
        |> required "client_currency" string
        |> required "client_currency_symbol" string
        |> required "name" string
        |> required "code" string
        |> required "billable" bool


hours : Decoder (List DailyHours)
hours =
    list (field "day_entry" dailyHours)


dailyHours : Decoder DailyHours
dailyHours =
    decode DailyHours
        |> required "id" int
        |> required "notes" (nullable string)
        |> required "spent_at" string
        |> required "hours" float
        |> required "is_closed" bool
        |> required "is_billed" bool


user : Decoder User
user =
    decode User
        |> required "id" int
        |> required "email" string
        |> required "admin" bool
        |> required "first_name" string
        |> required "last_name" string
        |> required "avatar_url" string


company : Decoder Company
company =
    decode Company
        |> required "base_uri" string
        |> required "full_domain" string
        |> required "name" string


whoAmI : Decoder WhoAmI
whoAmI =
    decode WhoAmI
        |> required "user" user
        |> required "company" company



-- Token


getTokenFromHash : String -> Maybe String
getTokenFromHash s =
    let
        params =
            parseUrlParams s
    in
        Dict.get "access_token" params


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.map parseSingleParam
        |> Dict.fromList


parseSingleParam : String -> ( String, String )
parseSingleParam p =
    let
        s =
            String.split "=" p
    in
        case s of
            [ key, val ] ->
                ( key, val )

            _ ->
                ( "", "" )
