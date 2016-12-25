module HarvestTypes exposing (..)

-- Timesheets


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
    }


type alias DayEntry =
    { id : Int
    , notes : String
    , hours : Int
    }


type alias Project =
    { id : Int
    , name : String
    , billable : Bool
    }
