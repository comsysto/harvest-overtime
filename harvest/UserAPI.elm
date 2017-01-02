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
        , Assignment
        , assignmentsDecoder
        , assignmentDecoder
        , getUsersAssignedToProject
        , getUserAssignment
        , assignUserToAProject
        , removeUserFromProject
        , updateAssignment
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


type alias Assignment =
    { id : Int
    , userId : Int
    , projectId : Int
    , isProjectManager : Bool
    , deactivated : Bool
    , hourlyRate : Float
    , budget : Maybe Float
    , estimate : Maybe Float
    , createdAt : Date
    , updatedAt : Date
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


assignmentsDecoder : Decoder (List Assignment)
assignmentsDecoder =
    list (field "user_assignment" assignmentDecoder)


assignmentDecoder : Decoder Assignment
assignmentDecoder =
    decode Assignment
        |> required "id" int
        |> required "user_id" int
        |> required "project_id" int
        |> required "is_project_manager" bool
        |> required "deactivated" bool
        |> required "hourly_rate" float
        |> required "budget" (nullable float)
        |> required "estimate" (nullable float)
        |> required "created_at" date
        |> required "updated_at" date


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



{- User Assignments -}


getUsersAssignedToProject : String -> Int -> String -> Dict String String -> Request (List Assignment)
getUsersAssignedToProject accountId projectId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = assignmentUrl accountId projectId token params
        , body = emptyBody
        , expect = expectJson assignmentsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getUserAssignment : String -> Int -> Int -> String -> Request Assignment
getUserAssignment accountId projectId assignmentId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "user_assignments/" ++ (toString assignmentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson assignmentDecoder
        , timeout = Nothing
        , withCredentials = False
        }


assignUserToAProject : String -> Int -> Int -> String -> Request String
assignUserToAProject accountId userId projectId token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/user_assignments?access_token=" ++ token
        , body = jsonBody <| encodeUserAssignment userId
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/user_assignments/{USER_ASSIGNMENT_ID}


removeUserFromProject : String -> Int -> Int -> String -> Request String
removeUserFromProject accountId projectId assignmentId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/user_assignments/" ++ (toString assignmentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- PUT https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/user_assignments/{USER_ASSIGNMENT_ID}


updateAssignment : String -> Assignment -> String -> Request String
updateAssignment accountId assignment token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString assignment.projectId) ++ "/user_assignments/" ++ (toString assignment.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeAssignment assignment
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeAssignment : Assignment -> JE.Value
encodeAssignment assignment =
    JE.object
        [ ( "user"
          , JE.object
                [ ( "user_id", JE.int assignment.userId )
                , ( "project_id", JE.int assignment.projectId )
                , ( "is_project_manager", JE.bool assignment.isProjectManager )
                , ( "deactivated", JE.bool assignment.deactivated )
                , ( "hourly_rate", JE.float assignment.hourlyRate )
                ]
          )
        ]


encodeUserAssignment : Int -> JE.Value
encodeUserAssignment userId =
    JE.object
        [ ( "user"
          , JE.object
                [ ( "id", JE.int userId ) ]
          )
        ]


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


assignmentUrl : String -> Int -> String -> Dict String String -> String
assignmentUrl accountId projectId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/user_assignments?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/people?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
