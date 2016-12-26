module HarvestTypes exposing (..)

-- Timesheets


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
    }


type alias DayEntry =
    { task : String
    , notes : String
    , hours : Float
    }


type alias Project =
    { id : Int
    , name : String
    , billable : Bool
    }



{-
   Type for this URL:
   http://help.getharvest.com/api/reports-api/reports/time-reports/#all-entries-by-user-for-timeframe
-}


type alias Hours =
    { dailyHours : List DailyHours
    }


type alias DailyHours =
    { id : Int
    , notes : String
    , spent_at : String
    , hours : Float
    , is_closed : Bool
    , is_billed : Bool
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
