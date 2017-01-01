module Harvest.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Harvest.Types exposing (..)
import Json.Decode.Extra exposing (date)


daily : Decoder Daily
daily =
    decode Daily
        |> required "day_entries" (list dayEntry)
        |> required "for_day" string
        |> required "projects" (list project)


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
