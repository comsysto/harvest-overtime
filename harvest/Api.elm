module Harvest.Api exposing (checkAccessTokenAvailable, authUrl, getDailyHoursForDateRange)

import Dict
import Harvest.Decoder exposing (..)
import Harvest.Types exposing (..)
import Http exposing (..)
import Task


-- Timesheets


getDailyHoursForDateRange : String -> String -> String -> String -> String -> Request (List DayEntry)
getDailyHoursForDateRange accountId userId from to token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = urlDailyHours accountId userId from to token
        , body = emptyBody
        , expect = expectJson hours
        , timeout = Nothing
        , withCredentials = False
        }



-- Harvest URLs


urlDailyHours : String -> String -> String -> String -> String -> String
urlDailyHours accountId userId from to token =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/people/"
        ++ userId
        ++ "/entries?from="
        ++ from
        ++ "&to="
        ++ to
        ++ "&access_token="
        ++ token


authUrl : String -> String -> String -> String
authUrl accountId clientId redirectUrl =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/oauth2/authorize?response_type=token&immediate=true&approval_prompt=auto&client_id="
        ++ clientId
        ++ "&redirect_uri="
        ++ (Http.encodeUri redirectUrl)



-- Token


checkAccessTokenAvailable : String -> String -> Task.Task String String
checkAccessTokenAvailable urlHashToParse authenticationUrl =
    case Dict.get "access_token" (parseUrlParams urlHashToParse) of
        Just a ->
            Task.succeed a

        Nothing ->
            Task.fail authenticationUrl


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
