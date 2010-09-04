package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class ConversionsSpec extends Specification {
  import Conversions._
  "String should convert to a date" in {
    "20100905".toDate must_== Date(2010, Sep, 5) 
    "2010-09-05".toDate must_== Date(2010, Sep, 5)
    "2010-Sep-05".toDate must_== Date(2010, Sep, 5)
  }

  "String should convert to a time" in {
    "16:04".toTimeOfDay must_== TimeOfDay(16, 4)
    "16:04:37".toTimeOfDay must_== TimeOfDay(16, 4, 37)
    "16:04:37.876".toTimeOfDay must_== TimeOfDay(16, 4, 37, 876)
  }
}