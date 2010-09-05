package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class InstantSpec extends Specification {

  "instant should follow equality" in {
    import scala.compat._
    val l = Platform.currentTime
    Instant(l) must_== Instant(l)
  }

  "timeofday should follow equality" in {
    TimeOfDay(12, 23, 34, 678) == TimeOfDay(12, 23, 34, 678) must beTrue
    TimeOfDay(12, 23, 34, 678) == TimeOfDay(12, 23, 34, 679) must beFalse
    TimeOfDay(12, 23, 34, 678) == TimeOfDay(12, 23, 33, 678) must beFalse
    TimeOfDay(12, 23, 34, 678) == TimeOfDay(12, 22, 34, 678) must beFalse
    TimeOfDay(12, 23, 34, 678) == TimeOfDay(11, 23, 34, 678) must beFalse
  }

  "timeofday should be ordered" in {
    (TimeOfDay(12, 23, 34, 678) < TimeOfDay(12, 23, 34, 679)) must beTrue
    (TimeOfDay(12, 23, 33, 678) < TimeOfDay(12, 23, 34, 678)) must beTrue
    (TimeOfDay(12, 22, 33, 678) < TimeOfDay(12, 23, 34, 678)) must beTrue
    (TimeOfDay(11, 22, 33, 678) < TimeOfDay(12, 23, 34, 678)) must beTrue
  }

  "timeofday should be valid" in {
    TimeOfDay(-1, 0) must throwA[IllegalArgumentException]
    TimeOfDay(24, 0) must throwA[IllegalArgumentException]
    TimeOfDay(1, -1) must throwA[IllegalArgumentException]
    TimeOfDay(1, 60) must throwA[IllegalArgumentException]
    TimeOfDay(1, 59, -1) must throwA[IllegalArgumentException]
    TimeOfDay(1, 59, 60) must throwA[IllegalArgumentException]
    TimeOfDay(1, 59, 50, -1) must throwA[IllegalArgumentException]
    TimeOfDay(1, 59, 50, 1000) must throwA[IllegalArgumentException]


  }
}