module Types exposing (..)

import Harvest.TimeReporting exposing (DayEntry)


{- TIHS IS UNUSED !!!! -}
{- DELETE THID FILE !!!! -}


type alias Daily =
    { dayEntries : List DayEntry
    , forDay : String
    , projects : List Project
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
