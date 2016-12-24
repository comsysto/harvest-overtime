module Token exposing (getAccessTokenFromHash)

import Regex exposing (..)


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
