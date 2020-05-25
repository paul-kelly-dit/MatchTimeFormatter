package com.pp.timeformatting

import java.time.LocalTime

import com.pp.timeformatting.RuleEvaluator.{firstHalfInMinutes, preMatchTime, secondHalfInMinutes}
import com.pp.timeformatting.models._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

case class RuleEvaluatorTester(rules: List[GameRule]) {
  lazy val when = rules
}

object RuleEvaluatorTester {
  def rule(rule: GameRule) = RuleEvaluatorTester(List(rule))
}

class RuleEvaluatorSpec extends AnyWordSpec with Matchers {

  val errorFn = () => "INVALID"

  "RuleEvaluator" when {

    "pre-match" should {
      "return converted game code '00:00 – PRE_MATCH' if `It is pre-match` rule is satisfied with a valid pre-match time and code" in {
        val validTimeEntry = TimeEntry(PreMatch, preMatchTime)
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is pre-match`), validTimeEntry, errorFn) shouldBe ("00:00 - PRE_MATCH")
      }

      "return `INVALID` if `It is pre-match` rule is not satisfied with a valid pre-match code but invalid time" in {
        val invalidTimeEntry = TimeEntry(PreMatch, LocalTime.of(0, 37, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is pre-match`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is pre-match` rule is not satisfied with an invalid pre-match code but valid time" in {
        val invalidTimeEntry = TimeEntry(FirstHalf, LocalTime.of(0, 0, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is pre-match`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "half-time" should {
      "return converted game code '45:00 - HALF_TIME' if `It is halftime` rule is satisfied with a valid half-time and code" in {
        val validTimeEntry = TimeEntry(HalfTime, firstHalfInMinutes)
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is halftime`), validTimeEntry, errorFn) shouldBe ("45:00 - HALF_TIME")
      }

      "return `INVALID` if `It is halftime` rule is not satisfied with a valid half-time code but invalid time" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(0, 37, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is halftime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is halftime` rule is not satisfied with an invalid half-time code but valid time" in {
        val invalidTimeEntry = TimeEntry(FirstHalf, LocalTime.of(0, 45, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is halftime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "full-time" should {
      "return converted game code '90:00 +00:00 – FULL_TIME' if `It is full-time` rule is satisfied with a valid fule-time and code" in {
        val validTimeEntry = TimeEntry(FullTime, secondHalfInMinutes)
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is fulltime`), validTimeEntry, errorFn) shouldBe ("90:00 +00:00 - FULL_TIME")
      }

      "return `INVALID` if `It is fulltime` rule is not satisfied with a valid full-time code but invalid time" in {
        val invalidTimeEntry = TimeEntry(FullTime, LocalTime.of(1, 23, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is halftime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is fulltime` rule is not satisfied with an invalid full-time code but valid time" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(1, 30, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is halftime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "first half and not gone into overtime" should {
      "return converted game code '03:08 – FIRST_HALF' if `It is first half and has not gone into overtime` rule is satisfied with a valid first-half time and code" in {
        val validTimeEntry = TimeEntry(FirstHalf, LocalTime.of(0, 3, 7, 513))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has not gone into overtime`), validTimeEntry, errorFn) shouldBe ("03:08 - FIRST_HALF")
      }

      "return `INVALID` if `It is first half and has not gone into overtime` rule is not satisfied with an invalid first-half time and valid code" in {
        val invalidTimeEntry = TimeEntry(FirstHalf, LocalTime.of(1, 23, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is first half and has not gone into overtime` rule is not satisfied with a valid first-half time and invalid code" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(0, 30, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "first half and has gone into overtime" should {
      "return converted game code '03:08 – FIRST_HALF' if `It is first half and has gone into overtime` rule is satisfied with a valid first-half time and code" in {
        val validTimeEntry = TimeEntry(FirstHalf, LocalTime.of(0, 45, 57, 513))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has gone into overtime`), validTimeEntry, errorFn) shouldBe ("45:00 +00:58 - FIRST_HALF")
      }

      "return `INVALID` if `It is first half and has gone into overtime` rule is not satisfied with an invalid first-half time and valid code" in {
        val invalidTimeEntry = TimeEntry(FirstHalf, LocalTime.of(1, 23, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is first half and has gone into overtime` rule is not satisfied with a valid first-half time and invalid code" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(0, 46, 1, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is first half and has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "second half and not gone into overtime" should {
      "return converted game code '45:01 – SECOND_HALF' if `It is second half and has not gone into overtime` rule is satisfied with a valid second-half time and code" in {
        val validTimeEntry = TimeEntry(SecondHalf, LocalTime.of(0, 45, 0, 500))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has not gone into overtime`), validTimeEntry, errorFn) shouldBe ("45:01 - SECOND_HALF")
      }

      "return `INVALID` if `It is second half and has not gone into overtime` rule is not satisfied with an invalid second-half time and valid code" in {
        val invalidTimeEntry = TimeEntry(SecondHalf, LocalTime.of(0, 23, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is second half and has not gone into overtime` rule is not satisfied with a valid second-half time and invalid code" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(0, 55, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has not gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

    "second half and has gone into overtime" should {
      "return converted game code '03:08 – FIRST_HALF' if `It is second half and has gone into overtime` rule is satisfied with a valid second-half time and code" in {
        val validTimeEntry = TimeEntry(SecondHalf, LocalTime.of(1, 30, 0, 908))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has gone into overtime`), validTimeEntry, errorFn) shouldBe ("90:00 +00:01 - SECOND_HALF")
      }

      "return `INVALID` if `It is first half and has gone into overtime` rule is not satisfied with an invalid first-half time and valid code" in {
        val invalidTimeEntry = TimeEntry(SecondHalf, LocalTime.of(1, 23, 0, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }

      "return `INVALID` if `It is first half and has gone into overtime` rule is not satisfied with a valid first-half time and invalid code" in {
        val invalidTimeEntry = TimeEntry(HalfTime, LocalTime.of(1, 32, 1, 0))
        RuleEvaluator.evaluate(List(RuleEvaluator.`It is the second half and it has gone into overtime`), invalidTimeEntry, errorFn) shouldBe ("INVALID")
      }
    }

  }
}
