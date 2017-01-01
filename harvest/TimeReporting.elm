module Harvest.TimeReporting
    exposing
        ( DayEntry
        , getEntriesByUserForDateRange
        , getEntriesForProjectTimeframe
        , dayEntry
        , hours
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
    , notes : Maybe String
    , spentAt : Date
    , hours : Float
    , userId : Int
    , projectId : Int
    , taskId : Int
    , createdAt : String
    , updatedAt : String
    , adjustmentRecord : Bool
    , timerStartedAt : Maybe String
    , isClosed : Bool
    , isBilled : Bool
    , hoursWithTimer : Float
    }



{-
   GET https://YOURACCOUNT.harvestapp.com/people/{USER_ID}/entries?from=YYYYMMDD&to=YYYYMMDD
-}


getEntriesByUserForDateRange : String -> String -> String -> String -> String -> Dict String String -> Request (List DayEntry)
getEntriesByUserForDateRange accountId userId from to token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrlForUser accountId userId from to token params
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
        , url = createUrlForProject accountId projectId from to token params
        , body = emptyBody
        , expect = expectJson hours
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


dayEntry : Decoder DayEntry
dayEntry =
    decode DayEntry
        |> required "id" int
        |> required "notes" (nullable string)
        |> required "spent_at" date
        |> required "hours" float
        |> required "user_id" int
        |> required "project_id" int
        |> required "task_id" int
        |> required "created_at" string
        |> required "updated_at" string
        |> required "adjustment_record" bool
        |> required "timer_started_at" (nullable string)
        |> required "is_closed" bool
        |> required "is_billed" bool
        |> optional "hours_with_timer" float 0


hours : Decoder (List DayEntry)
hours =
    list (field "day_entry" dayEntry)



{- Helpers for Harvest URLs -}


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
createUrlForUser accountId projectId from to token params =
    let
        url =
            urlForUser accountId projectId from to token

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
        ++ "/entries?from="
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
        ++ "/entries?from="
        ++ from
        ++ "&to="
        ++ to
        ++ "&access_token="
        ++ token
