module Harvest.Types exposing (..)

import Date exposing (Date)


-- Timesheets


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
    }



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


type alias Project =
    { id : Int
    , clientId : Int
    , client : String
    , clientCurrency : String
    , clientCurrencySymbol : String
    , name : String
    , code : String
    , billable : Bool
    }
