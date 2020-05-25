package com.pp.timeformatting


import com.pp.timeformatting.RuleEvaluator._
import com.pp.timeformatting.models.{ErrorFn, GameRule, gameRules}

import scala.collection.JavaConverters._


object Bootstrap extends App {


  val errorFn: ErrorFn = () => "INVALID"

  val testEntries = List(
    "[PM] 0:00.000",
    "[H1] 0:15.025",
    "FOO")

  val timeEntryProcessor = new TimeEntryProcessor(gameRules, errorFn)

  println("TimeEntry Processor running - sample run, full suite in AcceptanceTestSpec")

  println(s"Input:${testEntries.mkString(",")}")
  println(s"Output:${testEntries.map(timeEntryProcessor.processEntry).mkString(",")}")
}
