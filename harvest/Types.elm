module Harvest.Types exposing (..)

-- Timesheets


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
    }


type alias DayEntry =
    { projectId : Int
    , userId : Int
    , spentAt : String
    , taskId : Int
    , id : Int
    , notes : Maybe String
    , hours : Float
    , weekNumber : Int
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
