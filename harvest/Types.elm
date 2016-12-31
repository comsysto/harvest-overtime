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


type alias ProjectManager =
    { isProjectManager : Bool
    , canSeeRates : Bool
    , canCreateProjects : Bool
    , canCreateInvoices : Bool
    }


type alias User =
    { timezone : String
    , timezoneIdentifier : String
    , timezoneUtcOffset : Int
    , id : Int
    , email : String
    , admin : Bool
    , firstName : String
    , lastName : String
    , avatarUrl : String
    , projectManager : ProjectManager
    , timestampTimers : Bool
    }


type alias Modules =
    { expenses : Bool
    , invoices : Bool
    , estimates : Bool
    , approval : Bool
    }


type alias Company =
    { baseUri : String
    , fullDomain : String
    , name : String
    , active : Bool
    , weekStartDay : String
    , timeFormat : String
    , clock : String
    , decimalSymbol : String
    , colorScheme : String
    , modules : Modules
    , thousandsSeparator : String
    , planType : String
    }


type alias WhoAmI =
    { user : User
    , company : Company
    }
