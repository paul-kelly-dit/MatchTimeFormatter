package com.pp.timeformatting.utils

import scala.util.matching.Regex

object RegexOps {
  implicit class RichRegex(val r: Regex) extends AnyVal {
    def matches(s: String) = r.pattern.matcher(s).matches
  }
}
