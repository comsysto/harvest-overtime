module Harvest.TimesheetAPI
    exposing
        ( DayEntry
        , dayEntryDecoder
        , createEntry
        , getEntryForCurrentDay
        , getEntryForDayOfYear
        , getEntryById
        , deleteEntry
        , toggleEntry
        , updateEntry
        )

--import Dict exposing (Dict)

import Date exposing (Date)
import Date.Extra exposing (toFormattedString)
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Encode as JE
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)


-- TimesheetAPI


type alias DayEntry =
    { id : Int
    , userId : Int
    , projectId : Int
    , taskId : Int
    , notes : Maybe String
    , spentAt : Date
    , hours : Float
    , project : Maybe String
    , client : Maybe String
    , task : Maybe String
    , adjustmentRecord : Maybe Bool
    , isBilled : Maybe Bool
    , isClosed : Maybe Bool
    , timerStartedAt : Maybe Date
    , hoursWithTimer : Maybe Float
    , hoursWithoutTimer : Maybe Float
    , startedAt : Maybe Date
    , endedAt : Maybe Date
    , createdAt : Date
    , updatedAt : Date
    }


type alias SimpleDayEntry =
    { notes : Maybe String
    , projectId : Int
    , taskId : Int
    , spentAt : Date
    , hours : Maybe Float
    , startedAt : Maybe Date
    , endedAt : Maybe Date
    }



-- POST https://YOURACCOUNT.harvestapp.com/daily/add


createEntry : String -> String -> SimpleDayEntry -> Request DayEntry
createEntry accountId token entry =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/daily/add?access_token=" ++ token
    in
        request
            { method = "POST"
            , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
            , url = url
            , body = jsonBody <| encodeSimpleDayEntry entry
            , expect = expectJson dayEntryDecoder
            , timeout = Nothing
            , withCredentials = False
            }



-- GET https://YOURACCOUNT.harvestapp.com/daily?slim=1


getEntryForCurrentDay : String -> String -> Request DayEntry
getEntryForCurrentDay accountId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily?slim=1&access_token=" ++ token
        , body = emptyBody
        , expect = expectJson dayEntryDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/daily/{DAY_OF_THE_YEAR}/{YEAR}


getEntryForDayOfYear : String -> Int -> Int -> String -> Maybe Int -> Request DayEntry
getEntryForDayOfYear accountId day year token userId =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = getUrlForCurrentDay accountId day year token userId
        , body = emptyBody
        , expect = expectJson dayEntryDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/daily/show/{DAY_ENTRY_ID}


getEntryById : String -> Int -> String -> Request DayEntry
getEntryById accountId entryId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/show/" ++ (toString entryId) ++ "&access_token=" ++ token
        , body = emptyBody
        , expect = expectJson dayEntryDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- DELETE https://YOURACCOUNT.harvestapp.com/daily/delete/{DAY_ENTRY_ID}


deleteEntry : String -> Int -> String -> Request String
deleteEntry accountId entryId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/" ++ (toString entryId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/daily/timer/{DAY_ENTRY_ID}


toggleEntry : String -> Int -> String -> Request String
toggleEntry accountId entryId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/timer/" ++ (toString entryId) ++ "&access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Decoders -}


dayEntryDecoder : Decoder DayEntry
dayEntryDecoder =
    decode DayEntry
        |> required "id" int
        |> required "user_id" int
        |> required "project_id" int
        |> required "task_id" int
        |> required "notes" (nullable string)
        |> required "spent_at" date
        |> required "hours" float
        |> required "project" (nullable string)
        |> required "client" (nullable string)
        |> required "task" (nullable string)
        |> required "adjustment_record" (nullable bool)
        |> required "is_closed" (nullable bool)
        |> required "is_billed" (nullable bool)
        |> required "timer_started_at" (nullable date)
        |> required "hours_with_timer" (nullable float)
        |> required "hours_without_timer" (nullable float)
        |> required "started_at" (nullable date)
        |> required "tended_at" (nullable date)
        |> required "created_at" date
        |> required "updated_at" date



-- POST https://YOURACCOUNT.harvestapp.com/daily/update/{DAY_ENTRY_ID}


updateEntry : String -> DayEntry -> String -> Request String
updateEntry accountId entry token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/daily/update/" ++ (toString entry.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeDayEntry entry
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeSimpleDayEntry : SimpleDayEntry -> JE.Value
encodeSimpleDayEntry c =
    JE.object
        [ ( "notes", JEE.maybe JE.string c.notes )
        , ( "project_id", JE.int c.projectId )
        , ( "task_id", JE.int c.taskId )
        , ( "spent_at", JE.string (toFormattedString "yyyy-MM-dd" c.spentAt) )
        , ( "hours", JEE.maybe JE.float c.hours )
        , ( "started_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        , ( "ended_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        ]


extractTime : Date -> String
extractTime aDate =
    toFormattedString "h:mm a" aDate


timeOfDay : Maybe Date -> Maybe String
timeOfDay aDate =
    case aDate of
        Just d ->
            Just (extractTime d)

        Nothing ->
            Nothing


encodeDayEntry : DayEntry -> JE.Value
encodeDayEntry c =
    JE.object
        [ ( "notes", JEE.maybe JE.string c.notes )
        , ( "project_id", JE.int c.projectId )
        , ( "task_id", JE.int c.taskId )
        , ( "spent_at", JE.string (toFormattedString "yyyy-MM-dd" c.spentAt) )
        , ( "started_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        , ( "ended_at", JEE.maybe JE.string (timeOfDay c.startedAt) )
        ]


getUrlForCurrentDay : String -> Int -> Int -> String -> Maybe Int -> String
getUrlForCurrentDay accountId day year token userId =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/daily/" ++ (toString day) ++ "/" ++ (toString year) ++ "?access_token=" ++ token
    in
        case userId of
            Just id ->
                url ++ "&of_user" ++ (toString id)

            Nothing ->
                url
