package com.pp.timeformatting

import java.time.{Duration, LocalTime}

import com.pp.timeformatting.RuleEvaluator.evaluate
import com.pp.timeformatting.models.{ErrorFn, GameRule}

class TimeEntryProcessor(gameRules: Seq[GameRule], errorFn: ErrorFn) {

  def processEntry(timeEntry: String): String = {

    splitInitialTimeEntry(timeEntry) match {
      case Some((matchCode, matchTimeAsString)) =>  {
        val matchTime = covertStringTimeEntryToLocalTime(matchTimeAsString)
        val timeEntry = TimeEntry(matchCode, matchTime)
        evaluate(gameRules, timeEntry, errorFn)
      }
      case _ => errorFn()
    }
  }

  private def splitInitialTimeEntry(shortTimeEntry: String) = {
    val Re = """\[(.+)\] (.+)""".r
    if (Re.matches(shortTimeEntry)) {
      val Re(matchCode, matchTime) = shortTimeEntry
      Some(matchCode, matchTime)
    } else None
  }

  private def tokenizeString(short: String) = {
    val Re = """(.+):(.+)\.(.+)""".r
    val Re(minutes, seconds, millis) = short
    (Integer.parseInt(minutes), Integer.parseInt(seconds), Integer.parseInt(millis))
  }

  private def covertStringTimeEntryToLocalTime(matchTimeAsString: String) = {
    val (minutes, seconds, millis) = tokenizeString(matchTimeAsString)
    val hours = minutes / 60
    val min = minutes % 60

    val durationAsString =
      if (hours > 0)
        s"PT${hours}H${min}M$seconds.${millis}S"
      else
        s"PT${min}M$seconds.${millis}S"

    LocalTime.MIN.plus(
      Duration.parse(durationAsString)
    )
  }

}
