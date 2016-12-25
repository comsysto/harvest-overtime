module HarvestAPI exposing (getAccessTokenFromHash, getDaily)

import Http exposing (..)
import Regex exposing (..)


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



-- Token


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
