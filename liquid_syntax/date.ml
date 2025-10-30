open CalendarLib
module Timezone = Timezone

type t = Calendar.t

let set_timezone (tz : Timezone.t) =
  let info = Timezone.info_from_timezone tz in
  let calendar_tz = Time_Zone.UTC_Plus (Float.to_int info.utc_offset) in
  Time_Zone.change calendar_tz

let iso_format = "%FT%T%:z"
let now = Calendar.now

let now_as_string fmat =
  let now = Calendar.now () in
  let formatted = Printer.Calendar.sprint fmat now in
  formatted

let format_date_string date_str fmat =
  let date = Printer.Date.from_string date_str in
  let formatted = Printer.Date.sprint fmat date in
  formatted

let as_string date fmat = Printer.Calendar.sprint fmat date
let as_iso_string date = Printer.Calendar.sprint iso_format date

(*
ISO:
2022-04-22T11:55:56-04:00

FORMATTERS:
- [%%]: a literal [%]
- [%a]: short day name (by using a short version of [day_name])
- [%A]: day name (by using [day_name])
- [%b]: short month name (by using a short version of [month_name])
- [%B]: month name (by using [month_name])
- [%c]: shortcut for [%a %b %d %H:%M:%S %Y]
- [%C]: century: as %Y without the two last digits (since version 2.01)
- [%d]: day of month (01..31)
- [%D]: shortcut for [%m/%d/%y]
- [%e]: same as [%_d]
- [%F]: shortcut for [%Y-%m-%d]: ISO-8601 notation (since version 2.01)
- [%h]: same as [%b]
- [%H]: hour (00..23)
- [%I]: hour (01..12)
- [%i]: same as [%F]; deprecated since 2.01
- [%j]: day of year (001..366)
- [%k]: same as [%_H]
- [%l]: same as [%_I]
- [%m]: month (01..12)
- [%M]: minute (00..59)
- [%n]: a newline (same as [\n])
- [%p]: AM or PM
- [%P]: am or pm (same as %p in lowercase) (since version 2.01)
- [%r]: shortcut for [%I:%M:%S %p]
- [%R]: shortcut for [%H:%M] (since version 2.01)
- [%s]: number of seconds since 1970/1/1 (since version 2.01)
- [%S]: second (00..60)
- [%t]: a horizontal tab (same as [\t])
- [%T]: shortcut for [%H:%M:%S]
- [%V]: week number of year (01..53)
- [%w]: day of week (1..7)
- [%W]: same as [%V]
- [%y]: last two digits of year (00..99)
- [%Y]: year (four digits)
- [%z]: time zone in the form +hhmm (e.g. -0400) (since version 2.01)
- [%:z]: time zone in the form +hh:mm (e.g. -04:00) (since version 2.01)
- [%::z]: time zone in the form +hh:mm:ss (e.g. -04:00:00)
(since version 2.01)
- [%:::z]: time zone in the form +hh (e.g. -04) (since version 2.01)
By default, date pads numeric fields with zeroes. Two special modifiers
between [`%'] and a numeric directive are recognized:
- ['-' (hyphen)]: do not pad the field
- ['_' (underscore)]: pad the field with spaces
- ['0' (zero)]: pad the field with zeroes (default) (since version 2.01)
- ['^']: use uppercase if possible (since version 2.01)
Padding is only available for printers, not for parsers.
*)
