package com.pp.timeformatting

import com.pp.timeformatting.RuleEvaluator.{`It is first half and has gone into overtime`, `It is first half and has not gone into overtime`, `It is fulltime`, `It is halftime`, `It is pre-match`, `It is the second half and it has gone into overtime`, `It is the second half and it has not gone into overtime`}

object models {

  val PreMatch = "PM"
  val FirstHalf = "H1"
  val HalfTime = "HT"
  val SecondHalf = "H2"
  val FullTime = "FT"

  val gameRules: Seq[GameRule] = List(
    `It is pre-match`,
    `It is halftime`,
    `It is fulltime`,
    `It is first half and has not gone into overtime`,
    `It is first half and has gone into overtime`,
    `It is the second half and it has not gone into overtime`,
    `It is the second half and it has gone into overtime`
  )

  type ErrorFn = () => String

  trait GameRule {
    def evaluate(context: TimeEntry): Option[String]
  }
}

