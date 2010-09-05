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
  
  "copy works with defaults" in {
    (Date(2010, 4, 10) copy (y=2011)) must_== Date(2011, 4 ,10)
    (Date(2010, 4, 10) copy (y=2011, m=May)) must_== Date(2011, 5 ,10)
    (Date(2010, 4, 10) copy (y=2011, m=May, d=12)) must_== Date(2011, 5 ,12)
  }

  "throws IllegalArgumentException with nonsensical info" in {
    Date(2010, -1, 1) must throwA[IllegalArgumentException]
    Date(2010, 1, -1) must throwA[IllegalArgumentException]
    Date(2010, 0, 1) must throwA[IllegalArgumentException]
    Date(2010, 1, 0) must throwA[IllegalArgumentException]
    Date(2010, 1, 32) must throwA[IllegalArgumentException]
    Date(2010, 2, 29) must throwA[IllegalArgumentException]
    Date(2008, 2, 30) must throwA[IllegalArgumentException]
    Date(2008, 3, 32) must throwA[IllegalArgumentException]
    Date(2008, 4, 31) must throwA[IllegalArgumentException]
    Date(2008, 5, 32) must throwA[IllegalArgumentException]
    Date(2008, 6, 31) must throwA[IllegalArgumentException]
    Date(2008, 7, 32) must throwA[IllegalArgumentException]
    Date(2008, 8, 32) must throwA[IllegalArgumentException]
    Date(2008, 9, 31) must throwA[IllegalArgumentException]
    Date(2008, 10, 32) must throwA[IllegalArgumentException]
    Date(2008, 11, 31) must throwA[IllegalArgumentException]
    Date(2008, 12, 32) must throwA[IllegalArgumentException]
    Date(2008, 13, 1) must throwA[IllegalArgumentException]
  }

  "last day of month is correct in" {
    (Jan lastDay 2010) mustBe 31
    (Feb lastDay 2010) mustBe 28
    (Feb lastDay 2008) mustBe 29
    (Feb lastDay 2000) mustBe 29
    (Feb lastDay 1900) mustBe 28
    (Mar lastDay 2008) mustBe 31
    (Apr lastDay 2008) mustBe 30
    (May lastDay 2008) mustBe 31
    (Jun lastDay 2008) mustBe 30
    (Jul lastDay 2008) mustBe 31
    (Aug lastDay 2008) mustBe 31
    (Sep lastDay 2008) mustBe 30
    (Oct lastDay 2008) mustBe 31
    (Nov lastDay 2008) mustBe 30
    (Dec lastDay 2008) mustBe 31
    5 //WTF? why do I need this here?
  }

  "month next works" in {
    Jan.next must_== Feb
    Feb.next must_== Mar
    Mar.next must_== Apr
    Apr.next must_== May
    May.next must_== Jun
    Jun.next must_== Jul
    Jul.next must_== Aug
    Aug.next must_== Sep
    Sep.next must_== Oct
    Oct.next must_== Nov
    Nov.next must_== Dec
    Dec.next must_== Jan
  }
  "month prev works" in {
    Jan.prev must_== Dec
    Feb.prev must_== Jan
    Mar.prev must_== Feb
    Apr.prev must_== Mar
    May.prev must_== Apr
    Jun.prev must_== May
    Jul.prev must_== Jun
    Aug.prev must_== Jul
    Sep.prev must_== Aug
    Oct.prev must_== Sep
    Nov.prev must_== Oct
    Dec.prev must_== Nov
  }
}