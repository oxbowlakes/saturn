package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class BeansSpec extends Specification {
  "property editor should convert dates" in {
    val ed = new DateEditor
    ed setAsText "20100609"
    ed.getValue must_== Date(2010, Jun, 9)

    ed setValue Date(2010, Sep, 6)
    ed.getAsText must_== "2010-09-06"
  }

  "time editor should convert times" in {
    val ed = new DateEditor
    ed setAsText "13:04:56"
    ed.getValue must_== TimeOfDay(13, 4, 56)

    ed setValue TimeOfDay(16, 45)
    ed.getAsText must_== "16:45:00.000"
    
  }
}