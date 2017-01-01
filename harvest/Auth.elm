module Harvest.Auth exposing (checkAccessTokenAvailable, authUrl)

import Dict
import Http exposing (..)
import Task


-- Harvest URLs


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



{- Helpers -}


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
