module DateUtil exposing (..)

import ISO8601 exposing (..)


epoch : Time
epoch =
    { year = 1970, month = 1, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0, offset = ( 0, 0 ) }


parseDateFromISO8601 : String -> Time
parseDateFromISO8601 stringDate =
    case fromString stringDate of
        Ok date ->
            date

        Err error ->
            epoch


firstThursdayOfTheYear : Time -> Time
firstThursdayOfTheYear current =
    let
        janFirst =
            { epoch | year = (year current) }

        adjustedDay =
            (4 - (day janFirst) + 7) % 7
    in
        case weekday janFirst of
            Thu ->
                janFirst

            _ ->
                { janFirst | day = adjustedDay }


weekNumber : Time -> Int
weekNumber date =
    let
        dayNr =
            weekdayAsInt date

        -- thursday of the current week
        thursday =
            { date | day = (day date) - dayNr + 3 }

        --first thursday of the year
        firstThursday =
            firstThursdayOfTheYear date

        wd =
            (toTime thursday) - (toTime firstThursday) |> toFloat
    in
        {-
           The weeknumber is the number of weeks between the
           first thursday of the year and the thursday in the target week
           604800000 = 7 * 24 * 3600 * 1000
        -}
        1 + floor (wd / 604800000)


weekdayAsInt : Time -> Int
weekdayAsInt date =
    case weekday date of
        Mon ->
            1

        Tue ->
            2

        Wed ->
            3

        Thu ->
            4

        Fri ->
            5

        Sat ->
            6

        Sun ->
            7
