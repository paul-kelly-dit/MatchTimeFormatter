package com.pp.timeformatting

import com.pp.timeformatting.RuleEvaluator._
import com.pp.timeformatting.models.{ErrorFn, GameRule}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TimeEntryProcessorSpec extends AnyWordSpec with Matchers {

  val errorFn: ErrorFn = () => "INVALID"
  private val allGameRules: Seq[GameRule] = List(
    `It is halftime`,
    `It is fulltime`,
    `It is first half and has not gone into overtime`,
    `It is first half and has gone into overtime`,
    `It is the second half and it has not gone into overtime`,
    `It is the second half and it has gone into overtime`
  )

  val differentErrorFn: ErrorFn = () => "DIFFERENT_INVALID"
  private val NoRules: Seq[GameRule] = Nil

  "TimeEntryProcessor" when {
    val timeEntryProcessor = new TimeEntryProcessor(allGameRules, errorFn)
    "processEntry is called" should {
      "parse the initial time entry, pass through rules engine and return a result" in {
        timeEntryProcessor.processEntry("[H1] 0:15.025") shouldBe("00:15 - FIRST_HALF")
      }
    }


    "no rules in rule engine and processEntry is called" should {

      val timeEntryProcessorNoRules = new TimeEntryProcessor(NoRules, errorFn)

      "return INVALID for all entries" in {
        val testEntries = List(
          "[PM] 0:00.000",
          "[H1] 3:07.513",
          "[H1] 45:00.001",
          "[PM] -10:00.000",
          "FOO")

        val expectedResponses = List(
          "INVALID",
          "INVALID",
          "INVALID",
          "INVALID",
          "INVALID"
        )
        testEntries.map(timeEntryProcessorNoRules.processEntry) should contain theSameElementsAs(expectedResponses)
      }
    }

    "different error message is entered" should {
      "return this updated message DIFFERENT_INVALID for all entries" in {

        val timeEntryProcessorNoRulesDifferentErrorFn = new TimeEntryProcessor(NoRules, differentErrorFn)

        val testEntries = List(
          "[PM] 0:00.000",
          "[H1] 3:07.513",
          "[H1] 45:00.001",
          "[PM] -10:00.000",
          "FOO")

        val expectedResponses = List(
          "DIFFERENT_INVALID",
          "DIFFERENT_INVALID",
          "DIFFERENT_INVALID",
          "DIFFERENT_INVALID",
          "DIFFERENT_INVALID"
        )
        testEntries.map(timeEntryProcessorNoRulesDifferentErrorFn.processEntry) should contain theSameElementsAs(expectedResponses)

      }
    }

  }
}
