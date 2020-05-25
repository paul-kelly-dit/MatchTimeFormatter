package com.pp.timeformatting.utils

import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

object DateFormatter {
  val DateFormat = new DateTimeFormatterBuilder()
    .appendPattern("m:ss.SSS")
    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
    .toFormatter

}
