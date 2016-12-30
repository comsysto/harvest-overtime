module Harvest.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Harvest.Types exposing (..)
import DateUtil exposing (parseDateFromISO8601, weekNumber)


daily : Decoder Daily
daily =
    decode Daily
        |> required "day_entries" (list dayEntry)
        |> required "for_day" string
        |> required "projects" (list project)


dayEntry : Decoder DayEntry
dayEntry =
    map8 DayEntry
        (field "project_id" int)
        (field "user_id" int)
        (field "spent_at" string)
        (field "task_id" int)
        (field "id" int)
        (maybe (field "notes" string))
        (field "hours" float)
        ((field "spent_at" string)
            |> andThen decodeWeekNumber
        )


project : Decoder Project
project =
    decode Project
        |> required "id" int
        |> required "client_id" int
        |> required "client" string
        |> required "client_currency" string
        |> required "client_currency_symbol" string
        |> required "name" string
        |> required "code" string
        |> required "billable" bool


hours : Decoder (List DayEntry)
hours =
    list (field "day_entry" dayEntry)


user : Decoder User
user =
    decode User
        |> required "id" int
        |> required "email" string
        |> required "admin" bool
        |> required "first_name" string
        |> required "last_name" string
        |> required "avatar_url" string


company : Decoder Company
company =
    decode Company
        |> required "base_uri" string
        |> required "full_domain" string
        |> required "name" string


whoAmI : Decoder WhoAmI
whoAmI =
    decode WhoAmI
        |> required "user" user
        |> required "company" company


decodeWeekNumber : String -> Decoder Int
decodeWeekNumber isoDate =
    let
        weekNr =
            weekNumber <| parseDateFromISO8601 isoDate
    in
        (succeed weekNr)
