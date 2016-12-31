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


projectManager : Decoder ProjectManager
projectManager =
    decode ProjectManager
        |> required "is_project_manager" bool
        |> required "can_see_rates" bool
        |> required "can_create_projects" bool
        |> required "can_create_invoices" bool


user : Decoder User
user =
    decode User
        |> required "timezone" string
        |> required "timezone_identifier" string
        |> required "timezone_utc_offset" int
        |> required "id" int
        |> required "email" string
        |> required "admin" bool
        |> required "first_name" string
        |> required "last_name" string
        |> required "avatar_url" string
        |> required "project_manager" projectManager
        |> required "timestamp_timers" bool


modules : Decoder Modules
modules =
    decode Modules
        |> required "expenses" bool
        |> required "invoices" bool
        |> required "estimates" bool
        |> required "approval" bool


company : Decoder Company
company =
    decode Company
        |> required "base_uri" string
        |> required "full_domain" string
        |> required "name" string
        |> required "active" bool
        |> required "week_start_day" string
        |> required "time_format" string
        |> required "clock" string
        |> required "decimal_symbol" string
        |> required "color_scheme" string
        |> required "modules" modules
        |> required "thousands_separator" string
        |> required "plan_type" string


whoAmI : Decoder WhoAmI
whoAmI =
    decode WhoAmI
        |> required "user" user
        |> required "company" company
