package com.pp.timeformatting.utils

import java.time.{Duration, LocalTime}
import java.time.temporal.ChronoUnit

object LocalTimeOps {

  def roundUpMillis(timeEntry: LocalTime): LocalTime = {

    // bit hacky and no doubt a better way
    val firstValueInMillis = Integer.parseInt(s"${timeEntry.getNano.toString.charAt(0)}")
    val roundUp = if (firstValueInMillis >= 5) 1 else 0
    timeEntry.truncatedTo(ChronoUnit.SECONDS).plusSeconds(roundUp)
  }

  def timeDifferenceFormatted(timeEntry: LocalTime, firstHalfInMinutes: LocalTime) = {
    val duration = Duration.between(firstHalfInMinutes, timeEntry)

    f"+${duration.toMinutesPart}%02d:${duration.toSecondsPart}%02d";
  }
}
