module Imf.DateTime exposing
    ( toPosix, fromPosix
    , parser
    )

{-| Convert
[Internet Message Format](https://tools.ietf.org/html/rfc5322#section-3.3)
date and time strings to and from
[POSIX times](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

Supported Internet Message Format specifications are
[RFC5322](https://tools.ietf.org/html/rfc5322#section-3.3),
as well as the obsolete
[RFC2822](https://tools.ietf.org/html/rfc2822#section-3.3)
and
[RFC822](https://www.w3.org/Protocols/rfc822/#z28).

@docs toPosix, fromPosix
@docs parser

-}

import DateFormat as Format exposing (format, text)
import Dict
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompIf
        , chompWhile
        , end
        , getChompedString
        , map
        , oneOf
        , problem
        , spaces
        , succeed
        , symbol
        )
import Parser.Dict exposing (fromDict)
import Time exposing (Month(..), Weekday(..))
import Time.Extra exposing (Interval(..))



-- PARSER


{-| Convert from an Internet Message Format date and time string to a
[POSIX time](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

Regardless of which time zone the string uses, this function normalizes it and
returns a time in UTC.

In accordance to
[RFC2822](https://tools.ietf.org/html/rfc2822#section-4.3)
then
[RFC5322](https://tools.ietf.org/html/rfc5322#section-4.3):

  - Military time zones as defined in
    [RFC822](https://www.w3.org/Protocols/rfc822/#z28) and unknown uppercase
    alphabetic ones are considered equivalent to `-0000` UTC offset.
  - Two digit years between 50 and 99 or three digits years are interpreted by
    adding 1900. Two digit years between 00 and 49 are interpreted by
    adding 2000.

If parsing fails, a `List Parser.DeadEnd` is returned. Install `elm/parser` and
see [`Parser.DeadEnd`](https://package.elm-lang.org/packages/elm/parser/latest/Parser#DeadEnd)
for producing helpful error messages.

Examples:

    import Time

    Imf.DateTime.toPosix "1 Jan 70 00:00:00 GMT"
    --> Ok (Time.millisToPosix 0)


    Imf.DateTime.toPosix "Wed, 31 Dec 69 19:00:00 EST"
    --> Ok (Time.millisToPosix 0)

-}
toPosix : String -> Result (List Parser.DeadEnd) Time.Posix
toPosix str =
    Parser.run
        (succeed identity
            |= parser
            |. end
        )
        str


{-| Internet Message Format date and time
[Parser](https://package.elm-lang.org/packages/elm/parser/latest/Parser#Parser)
for [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).
-}
parser : Parser Time.Posix
parser =
    succeed finalize
        |. oneOf
            [ succeed ()
                |. day
                |. spaces
                |. symbol ","
            , succeed ()
            ]
        |. spaces
        |= date
        |. spaces
        |= time
        |. spaces
        |= zone


finalize : Date -> Time -> Int -> Time.Posix
finalize d t minuteOffset =
    Time.Extra.partsToPosix Time.utc
        { year = d.year
        , month = d.month
        , day = d.day
        , hour = t.hour
        , minute = t.minute
        , second = t.second
        , millisecond = 0
        }
        |> Time.Extra.add Minute -minuteOffset Time.utc



-- DAY


day : Parser Weekday
day =
    [ ( "Mon", Mon )
    , ( "Tue", Tue )
    , ( "Wed", Wed )
    , ( "Thu", Thu )
    , ( "Fri", Fri )
    , ( "Sat", Sat )
    , ( "Sun", Sun )
    ]
        |> Dict.fromList
        |> fromDict



-- DATE


type alias Date =
    { day : Int
    , month : Month
    , year : Int
    }


date : Parser Date
date =
    succeed Date
        |= weekday
        |. spaces
        |= month
        |. spaces
        |= year


weekday : Parser Int
weekday =
    chompWhile Char.isDigit
        |> getChompedString
        |> andThen checkWeekday


checkWeekday : String -> Parser Int
checkWeekday str =
    case ( String.length str, String.toInt str ) of
        ( 1, Just n ) ->
            succeed n

        ( 2, Just n ) ->
            succeed n

        _ ->
            problem "Invalid week day"


month : Parser Month
month =
    [ ( "Jan", Jan )
    , ( "Feb", Feb )
    , ( "Mar", Mar )
    , ( "Apr", Apr )
    , ( "May", May )
    , ( "Jun", Jun )
    , ( "Jul", Jul )
    , ( "Aug", Aug )
    , ( "Sep", Sep )
    , ( "Oct", Oct )
    , ( "Nov", Nov )
    , ( "Dec", Dec )
    ]
        |> Dict.fromList
        |> fromDict


year : Parser Int
year =
    chompWhile Char.isDigit
        |> getChompedString
        |> andThen checkYear


checkYear : String -> Parser Int
checkYear str =
    case ( String.length str, String.toInt str ) of
        ( 2, Just n ) ->
            if n >= 50 then
                succeed (1900 + n)

            else
                succeed (2000 + n)

        ( 3, Just n ) ->
            succeed (1900 + n)

        ( 4, Just n ) ->
            succeed n

        _ ->
            problem "Invalid year"



-- TIME


type alias Time =
    { hour : Int
    , minute : Int
    , second : Int
    }


time : Parser Time
time =
    succeed Time
        |= digit2 "hour"
        |. symbol ":"
        |= digit2 "minutes"
        |= oneOf
            [ succeed identity
                |. symbol ":"
                |= digit2 "seconds"
            , succeed 0
            ]



-- ZONE


{-| See <https://tools.ietf.org/html/rfc5322#section-4.3>
for the interpretation of obsolete and unknown formats.
-}
zone : Parser Int
zone =
    oneOf
        [ offset
        , timezone
        ]


offset : Parser Int
offset =
    succeed toOffset
        |= oneOf
            [ map (always 1) (symbol "+")
            , map (always -1) (symbol "-")
            ]
        |= digit2 "zone local hours differential"
        |= digit2 "zone local minutes differential"


toOffset : Int -> Int -> Int -> Int
toOffset sign hourOffset minuteOffset =
    sign * (hourOffset * 60 + minuteOffset)


timezone : Parser Int
timezone =
    chompWhile Char.isUpper
        |> getChompedString
        |> andThen checkZone


checkZone : String -> Parser Int
checkZone str =
    case str of
        "EST" ->
            succeed (-5 * 60)

        "EDT" ->
            succeed (-4 * 60)

        "CST" ->
            succeed (-6 * 60)

        "CDT" ->
            succeed (-5 * 60)

        "MST" ->
            succeed (-7 * 60)

        "MDT" ->
            succeed (-6 * 60)

        "PST" ->
            succeed (-8 * 60)

        "PDT" ->
            succeed (-7 * 60)

        _ ->
            let
                len =
                    String.length str
            in
            if len > 0 && len <= 5 then
                succeed 0

            else
                problem "Invalid time zone"



-- 2DIGIT


digit2 : String -> Parser Int
digit2 desc =
    succeed ()
        |. chompIf Char.isDigit
        |. chompIf Char.isDigit
        |> getChompedString
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem ("Invalid two digit " ++ desc)
            )



-- FORMATTER


{-| Convert a
[`Time.Zone`](https://package.elm-lang.org/packages/elm/time/latest/Time#Zone)
and a
[`Time.Posix`](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)
to an
[Internet Message Format](https://tools.ietf.org/html/rfc5322#section-3.3)
date and time string.

  - The day of week abbreviation will always be prefixed.
  - A four digit year is always used.
  - A signed four digit numerical UTC offset is always used.

Examples:

    import Time

    epoch : Time.Posix
    epoch =
        Time.millisToPosix 0

    Imf.DateTime.fromPosix Time.utc epoch
    --> "Thu, 01 Jan 1970 00:00:00 +0000"

    Imf.DateTime.fromPosix (Time.customZone 60 []) epoch
    --> "Thu, 01 Jan 1970 01:00:00 +0100"

    Imf.DateTime.fromPosix
        (Time.customZone -300 [])
        (Time.millisToPosix 1357891860000)
    --> "Fri, 11 Jan 2013 03:11:00 -0500"

-}
fromPosix : Time.Zone -> Time.Posix -> String
fromPosix tz posix =
    format
        [ text (formatDay tz posix)
        , text ", "
        , Format.dayOfMonthFixed
        , text " "
        , Format.monthNameAbbreviated
        , text " "
        , Format.yearNumber
        , text " "
        , Format.hourMilitaryFixed
        , text ":"
        , Format.minuteFixed
        , text ":"
        , Format.secondFixed
        , text " "
        , text (formatZone tz posix)
        ]
        tz
        posix


formatDay : Time.Zone -> Time.Posix -> String
formatDay tz posix =
    format [ Format.dayOfWeekNameFull ] tz posix
        |> String.left 3


formatZone : Time.Zone -> Time.Posix -> String
formatZone tz posix =
    let
        minuteOffset =
            Time.Extra.toOffset tz posix

        hour =
            (abs minuteOffset // 60)
                |> String.fromInt
                |> String.padLeft 2 '0'

        minute =
            (modBy 60 <| abs minuteOffset)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    String.concat
        [ if minuteOffset >= 0 then
            "+"

          else
            "-"
        , hour
        , minute
        ]
