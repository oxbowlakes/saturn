package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class DateSpec extends Specification {

  "mkString is yyyy/MM/dd where / is Sep" in {
    Date(2010, 6, 5).mkString("") must_== "20100605"
    Date(2010, 6, 5).mkString("-") must_== "2010-06-05"
    Date(2010, 6, 5).mkString("/") must_== "2010/06/05"

  }

  "toString is yyyy-MM-dd" in {
    Date(2010, 6, 5).toString must_== "2010-06-05"
    Date(2010, 2, 15).toString must_== "2010-02-15"
    Date(2010, 10, 31).toString must_== "2010-10-31"
  }

  "default format is yyyy-MM-dd" in {
    def fmt(d : Date) = "%s".format(d)

    fmt(Date(2010, 1, 3)) must_== "2010-01-03"
    fmt(Date(2010, 11, 9)) must_== "2010-11-09"
  }

  "alt format is yyyyMMdd" in {
    def alt(d : Date) = "%#s".format(d)

    alt(Date(2010, 1, 3)) must_== "20100103"
    alt(Date(2010, 11, 9)) must_== "20101109"
    alt(Date(2010, 12, 19)) must_== "20101219"
  }

  "Date is ordered" in {
    Date(2010, 6, 5) must_== Date(2010, 6, 5)
    Date(2010, 6, 6) < Date(2010, 6, 7) must beTrue
    Date(2010, 6, 6) > Date(2010, 6, 7) must beFalse
    Date(2010, 6, 6) == Date(2010, 6, 7) must beFalse
  }

  "DayOfWeek is correct" in {
    Date(2010, 9, 4).dayOfWeek must_== Sat
    Date(2010, 9, 4).isWeekday must beFalse
    Date(2010, 9, 5).dayOfWeek must_== Sun
    Date(2010, 9, 5).isWeekday must beFalse
    Date(2010, 9, 6).dayOfWeek must_== Mon
    Date(2010, 9, 6).isWeekday must beTrue
    Date(2010, 9, 7).dayOfWeek must_== Tue
    Date(2010, 9, 7).isWeekday must beTrue
    Date(2010, 9, 8).dayOfWeek must_== Wed
    Date(2010, 9, 8).isWeekday must beTrue
    Date(2010, 9, 9).dayOfWeek must_== Thu
    Date(2010, 9, 9).isWeekday must beTrue
    Date(2010, 9, 10).dayOfWeek must_== Fri
    Date(2010, 9, 10).isWeekday must beTrue
  }
  
  "Addition and Subtraction is correct" in {
    (Date(2010, 9, 4) + 1) must_== Date(2010, 9, 5)
    (Date(2010, 9, 5) - 1) must_== Date(2010, 9, 4)
    (Date(2010, 9, 4) + 2) must_== Date(2010, 9, 6)
    (Date(2010, 9, 5) - 2) must_== Date(2010, 9, 3)
    (Date(2010, 9, 4) + 3) must_== Date(2010, 9, 7)
    (Date(2010, 9, 5) - 3) must_== Date(2010, 9, 2)
    (Date(2010, 9, 4) + 4) must_== Date(2010, 9, 8)
    (Date(2010, 9, 5) - 4) must_== Date(2010, 9, 1)
    (Date(2010, 9, 4) + 5) must_== Date(2010, 9, 9)
    (Date(2010, 9, 5) - 5) must_== Date(2010, 8, 31)
    (Date(2010, 9, 4) + 6) must_== Date(2010, 9, 10)
    (Date(2010, 9, 5) - 6) must_== Date(2010, 8, 30)
    (Date(2010, 9, 4) + 7) must_== Date(2010, 9, 11)
    (Date(2010, 9, 5) - 7) must_== Date(2010, 8, 29)
  }
}