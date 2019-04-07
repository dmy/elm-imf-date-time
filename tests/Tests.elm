module Tests exposing (fuzzy, values)

import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Imf.DateTime
import Test exposing (..)
import Time exposing (Month(..))
import Time.Extra exposing (partsToPosix)


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


est : Time.Zone
est =
    Time.customZone -300 []


values : Test
values =
    describe "Imf.DateTime"
        [ describe "toPosix"
            [ test "Epoch" <|
                \_ ->
                    Imf.DateTime.toPosix "Thu, 01 Jan 1970 00:00:00 GMT"
                        |> Expect.equal (Ok epoch)
            , test "Short Epoch" <|
                \_ ->
                    Imf.DateTime.toPosix "1 Jan 70 00:00:00 GMT"
                        |> Expect.equal (Ok epoch)
            , test "2-digit 50" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 50 00:00:00 GMT"
                        |> Result.map (Time.toYear Time.utc)
                        |> Expect.equal (Ok 1950)
            , test "2-digit 99" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 99 00:00:00 GMT"
                        |> Result.map (Time.toYear Time.utc)
                        |> Expect.equal (Ok 1999)
            , test "2-digit 00" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 00 00:00:00 GMT"
                        |> Result.map (Time.toYear Time.utc)
                        |> Expect.equal (Ok 2000)
            , test "2-digit 49" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 49 00:00:00 GMT"
                        |> Result.map (Time.toYear Time.utc)
                        |> Expect.equal (Ok 2049)
            , test "UT" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 UT"
                        |> Expect.equal (Ok epoch)
            , test "EST" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 EST"
                        |> Expect.equal (Ok <| Time.millisToPosix (5 * 3600000))
            , test "EDT" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 EDT"
                        |> Expect.equal (Ok <| Time.millisToPosix (4 * 3600000))
            , test "CST" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 CST"
                        |> Expect.equal (Ok <| Time.millisToPosix (6 * 3600000))
            , test "CDT" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 CDT"
                        |> Expect.equal (Ok <| Time.millisToPosix (5 * 3600000))
            , test "MST" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 MST"
                        |> Expect.equal (Ok <| Time.millisToPosix (7 * 3600000))
            , test "MDT" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 MDT"
                        |> Expect.equal (Ok <| Time.millisToPosix (6 * 3600000))
            , test "PST" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 PST"
                        |> Expect.equal (Ok <| Time.millisToPosix (8 * 3600000))
            , test "PDT" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 PDT"
                        |> Expect.equal (Ok <| Time.millisToPosix (7 * 3600000))
            , test "Ignore military time zone A" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 A"
                        |> Expect.equal (Ok epoch)
            , test "Ignore invalid military time zone J" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 J"
                        |> Expect.equal (Ok epoch)
            , test "Ignore military time zone M" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 M"
                        |> Expect.equal (Ok epoch)
            , test "Ignore military time zone N" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 N"
                        |> Expect.equal (Ok epoch)
            , test "Ignore military time zone Y" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 Y"
                        |> Expect.equal (Ok epoch)
            , test "Military time zone Z" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 Z"
                        |> Expect.equal (Ok epoch)
            , test "Unknown tolerated zone CEST" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 CEST"
                        |> Expect.equal (Ok epoch)
            , test "Local differential +00:00" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 +0000"
                        |> Expect.equal (Ok epoch)
            , test "Local differential +23:59" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 +2359"
                        |> Expect.equal (Ok <| Time.millisToPosix -(23 * 3600000 + 59 * 60000))
            , test "Local differential -23:59" <|
                \_ ->
                    Imf.DateTime.toPosix "01 Jan 70 00:00:00 -2359"
                        |> Expect.equal (Ok <| Time.millisToPosix (23 * 3600000 + 59 * 60000))
            ]
        , describe "fromPosix"
            [ test "Epoch" <|
                \_ ->
                    Imf.DateTime.fromPosix Time.utc epoch
                        |> Expect.equal "Thu, 01 Jan 1970 00:00:00 +0000"
            , test "4-digit 1950" <|
                \_ ->
                    Imf.DateTime.fromPosix Time.utc
                        (partsToPosix Time.utc
                            { year = 1950
                            , month = Jan
                            , day = 1
                            , hour = 0
                            , minute = 0
                            , second = 0
                            , millisecond = 0
                            }
                        )
                        |> Expect.equal "Sun, 01 Jan 1950 00:00:00 +0000"
            , test "4-digit 1999" <|
                \_ ->
                    Imf.DateTime.fromPosix Time.utc
                        (partsToPosix Time.utc
                            { year = 1999
                            , month = Jan
                            , day = 1
                            , hour = 0
                            , minute = 0
                            , second = 0
                            , millisecond = 0
                            }
                        )
                        |> Expect.equal "Fri, 01 Jan 1999 00:00:00 +0000"
            , test "Numerical UTC offset" <|
                \_ ->
                    Imf.DateTime.fromPosix est epoch
                        |> Expect.equal "Wed, 31 Dec 1969 19:00:00 -0500"
            , test "Local differential +23:59" <|
                \_ ->
                    Imf.DateTime.fromPosix (Time.customZone (23 * 60 + 59) [])
                        (Time.millisToPosix -(23 * 3600000 + 59 * 60000))
                        |> Expect.equal "Thu, 01 Jan 1970 00:00:00 +2359"
            , test "Local differential -23:59" <|
                \_ ->
                    Imf.DateTime.fromPosix (Time.customZone -(23 * 60 + 59) [])
                        (Time.millisToPosix (23 * 3600000 + 59 * 60000))
                        |> Expect.equal "Thu, 01 Jan 1970 00:00:00 -2359"
            ]
        ]


fuzzy : Test
fuzzy =
    fuzz int "(fromPosix Time.utc >> toPosix) with rounded seconds" <|
        \seconds ->
            let
                time =
                    Time.millisToPosix (seconds * 1000)
            in
            (Imf.DateTime.fromPosix Time.utc >> Imf.DateTime.toPosix) time
                |> Expect.equal (Ok time)
