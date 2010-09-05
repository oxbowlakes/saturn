package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class FormatsSpec extends Specification {

  "dateformat must satisfy equality" in {
    DateFormat("yyyy/MMM-dd") must_== DateFormat("yyyy/MMM-dd")
  }

  "dateformat must parse dates" in {
    DateFormat("yyyy/MMM/dd").parse("2010/Jun/20") must_== Right(Date(2010, Jun, 20))
    DateFormat("MM-dd-yy").parse("12-05-04") must_== Right(Date(2004, Dec, 5))
  }

  "dateformat must format dates" in {
    DateFormat("MMM/dd', 'yyyy").format(Date(2011,Jul, 12)) must_== "Jul/12, 2011"
  }

  "timeformat must satisfy equality" in {
    TimeOfDayFormat("HH-mm.ss") must_== TimeOfDayFormat("HH-mm.ss")
  }

  "timeformat must parse times" in {
    TimeOfDayFormat("HH.mm").parse("15.45") must_== Right(TimeOfDay(15, 45))
    TimeOfDayFormat("HH.mm:ss").parse("15.45:12") must_== Right(TimeOfDay(15, 45, 12))
    TimeOfDayFormat("HH.mm:ss-SSS").parse("15.45:12-345") must_== Right(TimeOfDay(15, 45, 12, 345))
  }
  
  "timeformat must format times" in {
    TimeOfDayFormat("HH.mm").format(TimeOfDay(10, 34, 59, 874)) must_== "10.34"
    TimeOfDayFormat("HH.mm.ss").format(TimeOfDay(10, 34, 59, 874)) must_== "10.34.59"
    TimeOfDayFormat("HH.mm.ss:SSS").format(TimeOfDay(10, 34, 59, 874)) must_== "10.34.59:874"

  }
}