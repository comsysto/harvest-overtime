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
        |> required "project_id" int
        |> required "user_id" int
        |> required "spent_at" date
        |> required "task_id" int
        |> required "id" int
        |> required "notes" (nullable string)
        |> required "created_at" string
        |> required "updated_at" string
        |> required "hours" float


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
