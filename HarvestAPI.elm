module HarvestAPI exposing (getAccessTokenFromHash, getDaily, getTokenFromHash)

import Http exposing (..)
import Regex exposing (..)
import Dict


-- Timesheets


getDaily : String -> Request String
getDaily token =
    request
        { method = "GET"
        , headers =
            [ header "Accept" "application/json"
            ]
        , url = "https://comsysto.harvestapp.com/daily?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- Token (Thomas' sersion)


getAccessTokenFromHash : String -> Maybe String
getAccessTokenFromHash hash =
    hash
        |> extractAccessToken
        |> List.head
        |> Maybe.andThen getSubmatches
        |> Maybe.andThen List.head
        |> join


extractAccessToken : String -> List Match
extractAccessToken hash =
    find (AtMost 1) (regex "access_token=(.*)&") hash


getSubmatches : Match -> Maybe (List (Maybe String))
getSubmatches match =
    Just (.submatches match)


join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing



-- Token (Sekib's version)


getTokenFromHash : String -> Maybe String
getTokenFromHash s =
    let
        params =
            parseUrlParams s
    in
        Dict.get "access_token" params



--|> Maybe.withDefault ""


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
