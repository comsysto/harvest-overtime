module Harvest.Types exposing (..)

import Date exposing (Date)


-- Timesheets


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
    }


type alias DayEntry =
    { projectId : String
    , project : String
    , userId : Int
    , spentAt : String
    , taskId : String
    , task : String
    , client : String
    , id : Int
    , notes : String
    , createdAt : String
    , updatedAt : String
    , hoursWithoutTimer : Float
    , hours : Float
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



{-
   Type for this URL:
   http://help.getharvest.com/api/reports-api/reports/time-reports/#all-entries-by-user-for-timeframe
-}


type alias DailyHours =
    { id : Int
    , notes : Maybe String
    , spent_at : Date
    , hours : Float
    , is_closed : Bool
    , is_billed : Bool
    , taskId : Int
    , projectId : Int
    }



{-
   WhoAmI

   GET https://YOURACCOUNT.harvestapp.com/account/who_am_i
-}


type alias User =
    { id : Int
    , email : String
    , admin : Bool
    , first_name : String
    , last_name : String
    , avatar_url : String
    }


type alias Company =
    { base_uri : String
    , full_domain : String
    , name : String
    }


type alias WhoAmI =
    { user : User
    , company : Company
    }
