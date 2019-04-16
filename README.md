# **Internet Message Format** Date & Time
 [![Elm package](https://img.shields.io/elm-package/v/dmy/elm-imf-date-time.svg)](https://package.elm-lang.org/packages/dmy/elm-imf-date-time/latest/) [![Build Status](https://travis-ci.com/dmy/elm-imf-date-time.svg?branch=master)](https://travis-ci.com/dmy/elm-imf-date-time)

Convert
[Internet Message Format](https://tools.ietf.org/html/rfc5322#section-3.3)
date and time strings to and from 
[POSIX times](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

```sh
    elm install dmy/elm-imf-date-time
```

Valid strings include date and time strings conforming to
[RFC5322](https://tools.ietf.org/html/rfc5322#section-3.3),
as well as the obsolete
[RFC2822](https://tools.ietf.org/html/rfc2822#section-3.3)
and
[RFC822](https://www.w3.org/Protocols/rfc822/#z28), however obsolete formats
are interpreted following 
[RFC5322 recommendations](https://tools.ietf.org/html/rfc5322#section-4.3).

Produced strings conform to
[RFC5322](https://tools.ietf.org/html/rfc5322#section-3.3),
without using any
obsolete format.

These date and time formats are found for example in
[HTTP](https://tools.ietf.org/html/rfc7231#section-7.1.1.1),
[SMTP](https://tools.ietf.org/html/rfc5321)
and
[RSS](http://www.rssboard.org/rss-specification).

New Internet protocols should most likely NOT use this format.  
Alternatives are
[POSIX times](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)
as advised
[here](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/)
or
[ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
following recommendations from 
[RFC3339](https://tools.ietf.org/html/rfc3339).

## Examples

```elm
import Imf.DateTime
import Time

epoch : Time.Posix
epoch = Time.millisToPosix 0

Imf.DateTime.fromPosix Time.utc epoch
--> "Thu, 01 Jan 1970 00:00:00 +0000"

Imf.DateTime.toPosix "1 Jan 70 00:00:00 GMT"
--> Ok epoch

est : Time.Zone
est = Time.customZone -300 []

Imf.DateTime.toPosix "Fri, 11 Jan 2013 08:11:00 GMT"
    |>  Result.map (Imf.DateTime.fromPosix est)
--> Ok ("Fri, 11 Jan 2013 03:11:00 -0500")
```
