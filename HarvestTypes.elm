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
