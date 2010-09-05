package oxbow.saturn

import org.specs._
/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

class AdjustersSpec extends Specification {
  import adjusters._
  import Adjuster._
  "weekday adjusters should work" in {
    (Date(2010, Sep, 3) >> Weekday(Next)) must_== Date(2010, Sep, 3)
    (Date(2010, Sep, 4) >> Weekday(Next)) must_== Date(2010, Sep, 6)
    (Date(2010, Sep, 5) >> Weekday(Next)) must_== Date(2010, Sep, 6)
    (Date(2010, Sep, 6) >> Weekday(Next)) must_== Date(2010, Sep, 6)

    (Date(2010, Sep, 3) >> Weekday(Prev)) must_== Date(2010, Sep, 3)
    (Date(2010, Sep, 4) >> Weekday(Prev)) must_== Date(2010, Sep, 3)
    (Date(2010, Sep, 5) >> Weekday(Prev)) must_== Date(2010, Sep, 3)
    (Date(2010, Sep, 6) >> Weekday(Prev)) must_== Date(2010, Sep, 6)
  }

  "day adjusters should work" in {
    (Date(2010, Sep, 4) >> ToMon(Next)) must_== Date(2010, Sep, 6)
    (Date(2010, Sep, 4) >> ToMon(Prev)) must_== Date(2010, Aug, 30)
    (Date(2010, Sep, 4) >> ToTue(Next)) must_== Date(2010, Sep, 7)
    (Date(2010, Sep, 4) >> ToTue(Prev)) must_== Date(2010, Aug, 31)
    (Date(2010, Sep, 4) >> ToWed(Next)) must_== Date(2010, Sep, 8)
    (Date(2010, Sep, 4) >> ToWed(Prev)) must_== Date(2010, Sep, 1)
    (Date(2010, Sep, 4) >> ToThu(Next)) must_== Date(2010, Sep, 9)
    (Date(2010, Sep, 4) >> ToThu(Prev)) must_== Date(2010, Sep, 2)
    (Date(2010, Sep, 4) >> ToFri(Next)) must_== Date(2010, Sep, 10)
    (Date(2010, Sep, 4) >> ToFri(Prev)) must_== Date(2010, Sep, 3)
    (Date(2010, Sep, 4) >> ToSat(Next)) must_== Date(2010, Sep, 11)
    (Date(2010, Sep, 4) >> ToSat(Prev)) must_== Date(2010, Sep, 4)
    (Date(2010, Sep, 5) >> ToSun(Next)) must_== Date(2010, Sep, 5)
    (Date(2010, Sep, 5) >> ToSun(Prev)) must_== Date(2010, Sep, 5)
    (Date(2010, Sep, 6) >> ToSun(Next)) must_== Date(2010, Sep, 12)
    (Date(2010, Sep, 6) >> ToSun(Prev)) must_== Date(2010, Sep, 5)
  }
}