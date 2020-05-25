package com.pp.timeformatting

import com.pp.timeformatting.models.{ErrorFn, gameRules}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AcceptanceTestSpec extends AnyWordSpec with Matchers {

  val errorFn: ErrorFn = () => "INVALID"
  // run all test cases
  val testEntries = List(
    "[PM] 0:00.000",
    "[H1] 0:15.025",
    "[H1] 3:07.513",
    "[H1] 45:00.001",
    "[H1] 46:15.752",
    "[HT] 45:00.000",
    "[H2] 45:00.500",
    "[H2] 90:00.908",
    "[FT] 90:00.000",
    "90:00",
    "[H3] 90:00.000",
    "[PM] -10:00.000",
    "FOO")

  val timeEntryProcessor = new TimeEntryProcessor(gameRules, errorFn)

  "Acceptance Test Suite" when {
    "executed" should {
      "run through all test cases" in {
        val expectedResponses = List(
          "00:00 - PRE_MATCH",
          "00:15 - FIRST_HALF",
          "03:08 - FIRST_HALF",
          "45:00 +00:00 - FIRST_HALF",
          "45:00 +01:16 - FIRST_HALF",
          "45:00 - HALF_TIME",
          "45:01 - SECOND_HALF",
          "90:00 +00:01 - SECOND_HALF",
          "90:00 +00:00 - FULL_TIME",
          "INVALID",
          "INVALID",
          "INVALID",
          "INVALID")

        testEntries.map(timeEntryProcessor.processEntry) should contain theSameElementsAs(expectedResponses)
      }
    }
  }
}
