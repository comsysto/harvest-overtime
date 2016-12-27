module Harvest.Api exposing (getDaily, getTokenFromHash, getDailyForDate, getUserInfo, getDailyHoursForDateRange)

import Harvest.Decoder exposing (..)
import Dict
import Http exposing (..)
import Harvest.Types exposing (..)


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
