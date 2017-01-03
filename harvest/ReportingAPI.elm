module Harvest.ReportingAPI
    exposing
        ( DayEntry
        , Expense
        , getEntriesByUserForDateRange
        , getEntriesForProjectTimeframe
        , getExpensesByUserForDateRange
        , getExpensesForProjectTimeframe
        , dayEntry
        , hours
        , expenses
        , expense
        )

import Dict exposing (Dict)
import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)


{-
   We use same object for both

   GET https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/entries?from=YYYYMMDD&to=YYYYMMDD

   and

   GET https://YOURACCOUNT.harvestapp.com/people/{USER_ID}/entries?from=YYYYMMDD&to=YYYYMMDD

   Later one doesn't have hours_with_timer field so we set it to 0
-}


type alias DayEntry =
    { id : Int
    , userId : Int
    , projectId : Int
    , taskId : Int
    , notes : Maybe String
    , spentAt : Date
    , hours : Float
    , adjustmentRecord : Bool
    , timerStartedAt : Maybe String
    , isClosed : Bool
    , isBilled : Bool
    , hoursWithTimer : Float
    , createdAt : Date
    , updatedAt : Date
    }


type alias Expense =
    { id : Int
    , userId : Int
    , projectId : Int
    , invoiceId : Int
    , companyId : Int
    , totalCost : Float
    , units : Float
    , expenseCategoryId : Int
    , spentAt : Date
    , isClosed : Bool
    , notes : Maybe String
    , billable : Bool
    , hasReceipt : Bool
    , receiptUrl : Maybe String
    , isLocked : Bool
    , locked_reason : Maybe String
    , createdAt : Date
    , updatedAt : Date
    }



{-
   GET https://YOURACCOUNT.harvestapp.com/people/{USER_ID}/entries?from=YYYYMMDD&to=YYYYMMDD
-}


getEntriesByUserForDateRange : String -> String -> String -> String -> String -> Dict String String -> Request (List DayEntry)
getEntriesByUserForDateRange accountId userId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForEntriesByUser accountId userId from to token params
        , body = emptyBody
        , expect = expectJson hours
        , timeout = Nothing
        , withCredentials = False
        }



{-
   GET https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/entries?from=YYYYMMDD&to=YYYYMMDD
-}


getEntriesForProjectTimeframe : String -> String -> String -> String -> String -> Dict String String -> Request (List DayEntry)
getEntriesForProjectTimeframe accountId projectId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForEntriesByProject accountId projectId from to token params
        , body = emptyBody
        , expect = expectJson hours
        , timeout = Nothing
        , withCredentials = False
        }


getExpensesByUserForDateRange : String -> String -> String -> String -> String -> Dict String String -> Request (List Expense)
getExpensesByUserForDateRange accountId userId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForExpensesByUser accountId userId from to token params
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
        , url = createUrlForExpensesByProject accountId projectId from to token params
        , body = emptyBody
        , expect = expectJson expenses
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


dayEntry : Decoder DayEntry
dayEntry =
    decode DayEntry
        |> required "id" int
        |> required "user_id" int
        |> required "project_id" int
        |> required "task_id" int
        |> required "notes" (nullable string)
        |> required "spent_at" date
        |> required "hours" float
        |> required "adjustment_record" bool
        |> required "timer_started_at" (nullable string)
        |> required "is_closed" bool
        |> required "is_billed" bool
        |> optional "hours_with_timer" float 0
        |> required "created_at" date
        |> required "updated_at" date


hours : Decoder (List DayEntry)
hours =
    list (field "day_entry" dayEntry)


expenses : Decoder (List Expense)
expenses =
    list (field "expense" expense)


expense : Decoder Expense
expense =
    decode Expense
        |> required "id" int
        |> required "user_id" int
        |> required "project_id" int
        |> required "invoice_id" int
        |> required "company_id" int
        |> required "total_cost" float
        |> required "units" float
        |> required "expense_category_id" int
        |> required "spent_at" date
        |> required "is_closed" bool
        |> required "notes" (nullable string)
        |> required "billable" bool
        |> required "has_receipt" bool
        |> required "receiptUrl" (nullable string)
        |> required "is_locked" bool
        |> required "locked_reason" (nullable string)
        |> required "created_at" date
        |> required "updated_at" date



{- Helpers for Harvest URLs -}


createUrlForEntriesByProject : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForEntriesByProject accountId projectId from to token params =
    let
        url =
            urlForEntriesByProject accountId projectId from to token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


createUrlForEntriesByUser : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForEntriesByUser accountId projectId from to token params =
    let
        url =
            urlForEntriesByUser accountId projectId from to token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


urlForEntriesByProject : String -> String -> String -> String -> String -> String
urlForEntriesByProject accountId projectId from to token =
    "https://"
        ++ accountId
        ++ ".harvestapp.com/projects/"
        ++ projectId
        ++ "/entries?from="
        ++ from
        ++ "&to="
        ++ to
        ++ "&access_token="
        ++ token


urlForEntriesByUser : String -> String -> String -> String -> String -> String
urlForEntriesByUser accountId userId from to token =
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


createUrlForExpensesByProject : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForExpensesByProject accountId projectId from to token params =
    let
        url =
            urlForProject accountId projectId from to token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


createUrlForExpensesByUser : String -> String -> String -> String -> String -> Dict String String -> String
createUrlForExpensesByUser accountId userId from to token params =
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
