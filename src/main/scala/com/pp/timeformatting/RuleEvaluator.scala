package com.pp.timeformatting

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import com.pp.timeformatting.models.{ErrorFn, FirstHalf, FullTime, GameRule, HalfTime, PreMatch, SecondHalf}
import com.pp.timeformatting.utils.LocalTimeOps
import com.pp.timeformatting.utils.LocalTimeOps.timeDifferenceFormatted

import scala.annotation.tailrec

case class TimeEntry(symbol: String, time: LocalTime)

object RuleEvaluator {

  val preMatchTime = LocalTime.of(0, 0, 0, 0)
  val firstHalfInMinutes = LocalTime.of(0, 45, 0, 0)
  val secondHalfInMinutes = LocalTime.of(1, 30, 0, 0)

  def `It is pre-match` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {

      if (timeEntry.symbol.equals(PreMatch) && timeEntry.time.equals(preMatchTime)) {
        val formatted = s"${preMatchTime.format(DateTimeFormatter.ofPattern("mm:ss"))} - ${mappings.toLongVersion(timeEntry.symbol)}"
        Some(formatted)
      } else
        None
    }
  }

  def `It is halftime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {

      if (timeEntry.symbol.equals(HalfTime) && timeEntry.time.equals(firstHalfInMinutes)) {
        val formatted = outputForNormalTime(timeEntry.symbol, timeEntry.time)
        Some(formatted)
      } else
        None
    }
  }

  def `It is fulltime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {

      if (timeEntry.symbol.equals(FullTime) && timeEntry.time.equals(secondHalfInMinutes)) {

        val formatted = secondHalfFormattedTimeEntry(timeEntry.symbol, secondHalfInMinutes)
//        }
        Some(formatted)
      } else
        None
    }
  }

  def `It is first half and has not gone into overtime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {
      import LocalTimeOps._
      val roundUpToSeconds = roundUpMillis(timeEntry.time)
      if (timeEntry.symbol.equals(FirstHalf) && roundUpToSeconds.isBefore(firstHalfInMinutes)) {
        val formatted = outputForNormalTime(timeEntry.symbol, roundUpToSeconds)
        Some(formatted)
      } else
        None
    }
  }

  def `It is first half and has gone into overtime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {
      import LocalTimeOps._
      val roundedUpTimeEntry = roundUpMillis(timeEntry.time)
      if (timeEntry.symbol.equals(FirstHalf) && timeEntry.time.isAfter(firstHalfInMinutes)) {
        val formatted: String = {
          val timeDifference = timeDifferenceFormatted(roundedUpTimeEntry, firstHalfInMinutes)
          val formatted = s"${firstHalfInMinutes.format(DateTimeFormatter.ofPattern("mm:ss"))} $timeDifference - ${mappings.toLongVersion(timeEntry.symbol)}"
          formatted
        }
        Some(formatted)
      } else
        None
    }
  }

  def `It is the second half and it has not gone into overtime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {
      import LocalTimeOps._

      val roundUpToSeconds = roundUpMillis(timeEntry.time)
      if (timeEntry.symbol.equals(SecondHalf) && roundUpToSeconds.isAfter(firstHalfInMinutes) && roundUpToSeconds.isBefore(secondHalfInMinutes)) {
        val formatted = outputForNormalTime(timeEntry.symbol, roundUpToSeconds)
        Some(formatted)
      } else
        None
    }
  }

  def `It is the second half and it has gone into overtime` = new GameRule {
    def evaluate(timeEntry: TimeEntry): Option[String] = {
      import LocalTimeOps._

      val roundedUpTimeEntry = roundUpMillis(timeEntry.time)

      if (timeEntry.symbol.equals(SecondHalf) && roundedUpTimeEntry.isAfter(secondHalfInMinutes)) {
        val formatted: String = secondHalfFormattedTimeEntry(timeEntry.symbol, roundedUpTimeEntry)
        Some(formatted)
      } else
        None
    }
  }

  private def secondHalfFormattedTimeEntry(timeEntryCode: String, roundedUpTimeEntry: LocalTime) = {
    val formatted = {
      val hour = secondHalfInMinutes.getHour
      val newMinutes = (hour * 60) + secondHalfInMinutes.getMinute

      val timeDifference = timeDifferenceFormatted(roundedUpTimeEntry, secondHalfInMinutes)
      val newFormattedDateString = s"$newMinutes:00"

      s"$newFormattedDateString $timeDifference - ${mappings.toLongVersion(timeEntryCode)}"
    }
    formatted
  }

  def evaluate(rules: Seq[GameRule], entry: TimeEntry, errorFn: ErrorFn): String =
    evaluate(rules, entry, lastResult = None, errorFn)

  @tailrec
  private def evaluate(rules: Seq[GameRule], context: TimeEntry, lastResult: Option[String], errorFn: ErrorFn): String = rules match {
    case Nil => lastResult.getOrElse(errorFn())
    case _ if lastResult.isDefined => lastResult.get
    case head :: tail => {
      evaluate(tail, context, head.evaluate(context), errorFn)
    }
  }

  private def outputForNormalTime(symbol: String, roundUpToSeconds: LocalTime) =
    s"${roundUpToSeconds.format(DateTimeFormatter.ofPattern("mm:ss"))} - ${mappings.toLongVersion(symbol)}"
}