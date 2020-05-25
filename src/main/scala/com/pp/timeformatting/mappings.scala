package com.pp.timeformatting

object mappings {

  def toLongVersion(shortVersion: String): String = shortVersion match {
    case "PM" => "PRE_MATCH"
    case "H1" => "FIRST_HALF"
    case "HT" => "HALF_TIME"
    case "H2" => "SECOND_HALF"
    case "FT" => "FULL_TIME"
  }
}
