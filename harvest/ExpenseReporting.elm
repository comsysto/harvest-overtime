module Harvest.ExpenseReporting
    exposing
        ( Expense
        , getExpensesByUserForDateRange
        , getExpensesForProjectTimeframe
        )

import Dict exposing (Dict)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)


type alias Expense =
    { id : Int
    , totalCost : Float
    , units : Float
    , createdAt : Date
    , updatedAt : Date
    , projectId : Int
    , expenseCategoryId : Int
    , userId : Int
    , spentAt : Date
    , isClosed : Bool
    , notes : Maybe String
    , invoiceId : Int
    , billable : Bool
    , companyId : Int
    , hasReceipt : Bool
    , receiptUrl : Maybe String
    , isLocked : Bool
    , locked_reason : Maybe String
    }


getExpensesByUserForDateRange : String -> String -> String -> String -> String -> Dict String String -> Request (List Expense)
getExpensesByUserForDateRange accountId userId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForUser accountId userId from to token params
        , body = emptyBody
        , expect = expectJson expenses
        , timeout = Nothing
        , withCredentials = False
        }


getExpensesForProjectTimeframe : String -> String -> String -> String -> String -> Dict String String -> Request (List Expense)
getExpensesForProjectTimeframe accountId projectId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForProject accountId projectId from to token params
        , body = emptyBody
        , expect = expectJson expenses
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


expenses : Decoder (List Expense)
expenses =
    list (field "expense" expense)


expense : Decoder Expense
expense =
    decode Expense
        |> required "id" int
        |> required "total_cost" float
        |> required "units" float
        |> required "created_at" date
        |> required "updated_at" date
        |> required "project_id" int
        |> required "expense_category_id" int
        |> required "user_id" int
        |> required "spent_at" date
        |> required "is_closed" bool
        |> required "notes" (nullable string)
        |> required "invoice_id" int
        |> required "billable" bool
        |> required "company_id" int
        |> required "has_receipt" bool
        |> required "receiptUrl" (nullable string)
        |> required "is_locked" bool
        |> required "locked_reason" (nullable string)



{- Helpers -}


createUrlForProject : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForProject accountId projectId from to token params =
    let
        url =
            urlForProject accountId projectId from to token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


createUrlForUser : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForUser accountId userId from to token params =
    let
        url =
            urlForUser accountId userId from to token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


urlForProject : String -> String -> String -> String -> String -> String
urlForProject accountId projectId from to token =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/projects/"
        ++ projectId
        ++ "/expenses?from="
        ++ from
        ++ "&to="
        ++ to
        ++ "&access_token="
        ++ token


urlForUser : String -> String -> String -> String -> String -> String
urlForUser accountId userId from to token =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/people/"
        ++ userId
        ++ "/expenses?from="
        ++ from
        ++ "&to="
        ++ to
        ++ "&access_token="
        ++ token
