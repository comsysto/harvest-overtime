module HarvestAPI exposing (getDaily, getTokenFromHash, getDailyForDate, getUserInfo, getDailyHoursForDateRange)

import Dict
import HarvestTypes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)


-- Timesheets


getDaily : String -> Request Daily
getDaily token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://comsysto.harvestapp.com/daily?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson decodeDaily
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
        , expect = expectJson decodeDaily
        , timeout = Nothing
        , withCredentials = False
        }


getDailyHoursForDateRange : String -> String -> String -> String -> Request Hours
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
        , expect = expectJson decodeHours
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
        , expect = expectJson decodeWhoAmI
        , timeout = Nothing
        , withCredentials = False
        }


decodeDaily : Decoder Daily
decodeDaily =
    map3 Daily (field "day_entries" (list dayEntry)) (field "for_day" string) (field "projects" (list project))


dayEntry : Decoder DayEntry
dayEntry =
    map3 DayEntry (field "task" string) (field "notes" string) (field "hours" float)


project : Decoder Project
project =
    map3 Project (field "id" int) (field "name" string) (field "billable" bool)


decodeHours : Decoder Hours
decodeHours =
    map Hours (field "day_entries" (list decodeDailyHours))


decodeDailyHours : Decoder DailyHours
decodeDailyHours =
    map6 DailyHours
        (field "id" int)
        (field "notes" string)
        (field "spent_at" string)
        (field "hours" float)
        (field "is_closed" bool)
        (field "is_billed" bool)


decodeUser : Decoder User
decodeUser =
    map6 User
        (field "id" int)
        (field "email" string)
        (field "admin" bool)
        (field "first_name" string)
        (field "last_name" string)
        (field "avatar_url" string)


decodeCompany : Decoder Company
decodeCompany =
    map3 Company
        (field "base_uri" string)
        (field "full_domain" string)
        (field "name" string)


decodeWhoAmI : Decoder WhoAmI
decodeWhoAmI =
    map2 WhoAmI
        (field "user" decodeUser)
        (field "company" decodeCompany)



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
