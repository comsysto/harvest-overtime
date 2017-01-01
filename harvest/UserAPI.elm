module Harvest.UserAPI
    exposing
        ( User
        , SimpleUser
        , allUsers
        , getUser
        , createUser
        , updateUser
        , deleteUser
        , toggleUser
        , usersDecoder
        , userDecoder
        )

import Date exposing (Date)


-- import Date.Extra exposing (toFormattedString)

import Json.Encode as JE
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


type alias User =
    { id : Int
    , email : String
    , firstName : String
    , lastName : String
    , timezone : String
    , telephone : Maybe String
    , department : Maybe String
    , isAdmin : Bool
    , isActive : Bool
    , isContractor : Bool
    , hasAccessToAllFutureProjects : Bool
    , wantsNewsletter : Bool
    , defaultHourlyRate : Float
    , costRate : Maybe Float
    , identityAccountId : Int
    , identityUserId : Int
    , weeklyCapacity : Int {- visible for admins only -}
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


type alias SimpleUser =
    { email : String
    , firstName : String
    , lastName : String
    , timezone : Maybe String
    , telephone : Maybe String
    , department : Maybe String
    , isAdmin : Maybe Bool
    , isActive : Maybe Bool
    , isContractor : Maybe Bool
    , hasAccessToAllFutureProjects : Maybe Bool
    , defaultHourlyRate : Maybe Float
    , costRate : Maybe Float
    }


usersDecoder : Decoder (List User)
usersDecoder =
    list (field "user" userDecoder)


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "id" int
        |> required "email" string
        |> required "first_name" string
        |> required "last_name" string
        |> required "timezone" string
        |> required "telephone" (nullable string)
        |> required "department" (nullable string)
        |> required "is_admin" bool
        |> required "is_active" bool
        |> required "is_contractor" bool
        |> required "has_access_to_all_future_projects" bool
        |> required "wants_newsletter" bool
        |> required "default_hourly_rate" float
        |> required "cost_rate" (nullable float)
        |> required "identity_account_id" int
        |> required "identity_user_id" int
        |> optional "weekly_capacity" int 0
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


allUsers : String -> String -> Dict String String -> Request (List User)
allUsers accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson usersDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getUser : String -> Int -> String -> Request User
getUser accountId userId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/people/" ++ (toString userId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson userDecoder
        , timeout = Nothing
        , withCredentials = False
        }


createUser : String -> String -> SimpleUser -> Request String
createUser accountId token user =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/people?access_token=" ++ token
    in
        request
            { method = "POST"
            , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
            , url = url
            , body = jsonBody <| encodeSimpleUser user
            , expect = expectString
            , timeout = Nothing
            , withCredentials = False
            }


updateUser : String -> User -> String -> Request String
updateUser accountId user token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/people/" ++ (toString user.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeUser user
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


deleteUser : String -> Int -> String -> Request String
deleteUser accountId userId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/people/" ++ (toString userId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }


toggleUser : String -> Int -> String -> Request String
toggleUser accountId userId token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/people/" ++ (toString userId) ++ "/toggle?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeUser : User -> JE.Value
encodeUser u =
    JE.object
        [ ( "user"
          , JE.object
                [ ( "email", JE.string u.email )
                , ( "first_name", JE.string u.firstName )
                , ( "last_name", JE.string u.lastName )
                , ( "timezone", JE.string u.timezone )
                , ( "telephone", JEE.maybe JE.string u.telephone )
                , ( "department", JEE.maybe JE.string u.department )
                , ( "is_admin", JE.bool u.isAdmin )
                , ( "is_active", JE.bool u.isActive )
                , ( "is_contractor", JE.bool u.isContractor )
                , ( "has_access_to_all_future_projects", JE.bool u.hasAccessToAllFutureProjects )
                , ( "wants_newsletter", JE.bool u.wantsNewsletter )
                , ( "default_hourly_rate", JE.float u.defaultHourlyRate )
                , ( "cost_rate", JEE.maybe JE.float u.costRate )
                ]
          )
        ]


encodeSimpleUser : SimpleUser -> JE.Value
encodeSimpleUser u =
    JE.object
        [ ( "user"
          , JE.object
                [ ( "email", JE.string u.email )
                , ( "first_name", JE.string u.firstName )
                , ( "last_name", JE.string u.lastName )
                , ( "department", JEE.maybe JE.string u.department )
                , ( "timezone", JEE.maybe JE.string u.timezone )
                , ( "telephone", JEE.maybe JE.string u.telephone )
                , ( "is_admin", JEE.maybe JE.bool u.isAdmin )
                , ( "is_active", JEE.maybe JE.bool u.isActive )
                , ( "is_contractor", JEE.maybe JE.bool u.isContractor )
                , ( "has_access_to_all_future_projects", JEE.maybe JE.bool u.hasAccessToAllFutureProjects )
                , ( "default_hourly_rate", JEE.maybe JE.float u.defaultHourlyRate )
                , ( "cost_rate", JEE.maybe JE.float u.costRate )
                ]
          )
        ]


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/people?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
