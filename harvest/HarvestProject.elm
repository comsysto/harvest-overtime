module HarvestProject exposing (Project, getProject, getAllProjects)

import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


type alias Project =
    { id : Int
    , clientId : Int
    , name : String
    , code : Maybe String
    , active : Bool
    , billable : Bool
    , billBy : String
    , hourlyRate : Maybe Float
    , budget : Maybe Float
    , budgetBy : String
    , notifyWhenOverBudget : Bool
    , overBudgetNotificationPercentage : Float
    , overBudgetNotifiedAt : Maybe Date
    , showBudgetToAll : Bool
    , createdAt : Date
    , updatedAt : Date
    , startsOn : Date
    , endsOn : Maybe Date
    , estimate : Maybe Float
    , estimateBy : String
    , hintEarliestRecordAt : Date
    , hintLatestRecordAt : Date
    , notes : Maybe String
    , costBudget : Maybe Float
    , costBudgetIncludeExpenses : Bool
    }


projectDecoder : Decoder Project
projectDecoder =
    decode Project
        |> required "id" int
        |> required "client_id" int
        |> required "name" string
        |> required "code" (nullable string)
        |> required "active" bool
        |> required "billable" bool
        |> required "bill_by" string
        |> required "hourly_rate" (nullable float)
        |> required "budget" (nullable float)
        |> required "budget_by" string
        |> required "notify_when_over_budget" bool
        |> required "over_budget_notification_percentage" float
        |> required "over_budget_notified_at" (nullable date)
        |> required "show_budget_to_all" bool
        |> required "created_at" date
        |> required "updated_at" date
        |> required "starts_on" date
        |> required "ends_on" (nullable date)
        |> required "estimate" (nullable float)
        |> required "estimate_by" string
        |> required "hint_earliest_record_at" date
        |> required "hint_latest_record_at" date
        |> required "notes" (nullable string)
        |> required "cost_budget" (nullable float)
        |> required "cost_budget_include_expenses" bool


projectsDecoder : Decoder (List Project)
projectsDecoder =
    list (field "project" projectDecoder)


getProject : String -> String -> String -> Request Project
getProject accountId projectId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ projectId ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson projectDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getAllProjects : String -> String -> Request (List Project)
getAllProjects accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson projectsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/projects?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
