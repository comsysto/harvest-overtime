module HarvestAPI exposing (getDaily, getTokenFromHash, getDailyForDate)

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


decodeDaily : Decoder Daily
decodeDaily =
    map3 Daily (field "day_entries" (list dayEntry)) (field "for_day" string) (field "projects" (list project))


dayEntry : Decoder DayEntry
dayEntry =
    map3 DayEntry (field "id" int) (field "notes" string) (field "hours" int)


project : Decoder Project
project =
    map3 Project (field "id" int) (field "name" string) (field "billable" bool)



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
