package oxbow.saturn

/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */
object Adjuster {
  /**
   * The Direction trait states whether an adjustment should be made "Next" (i.e. moving forwards into the
   * future), or "Prev" (i.e. moving backwards into the past). This is useful for, finding the next tuesday after a
   * given date
   */
  sealed trait Direction

  /**
   * Next - adjustment should move date into the future
   */
  case object Next extends Direction

  /**
   * Prev - adjustment should move date into the past
   */
  case object Prev extends Direction
}

/**
 * An {{Adjuster}} is simply an entity which takes a Date as input and returns
 * a (possibly different date as output). A Date can be adjusted via an {{Adjuster}}
 * using the following syntax:
 * {{{
 * val d2 = d1 >> adj
 * }}}
 */
trait Adjuster extends (Date => Date) {
  def apply(d : Date) = adjust(d)

  /**
   * Implementations of {{Adjuster}} must provide this method to state how a
   * date is transformed into another date
   */
  def adjust(in : Date) : Date
}

/**
 * This object contains some standard adjusters that are likely to be useful. The convention is
 * that, if a condition is satisfied for the target date, then this is returned. For example, if 2010-09-06
 * is a monday, then {{2010-09-06 >> ToMon(Next)}} will return the input unmodified.
 */
object adjusters {
  import Adjuster._

  /**
   * Day of week aduster for Monday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToMon(d : Direction) = ToDay(Mon)(d)

  /**
   * Adjusts to the following Monday
   */
  val NextMon = ToMon(Next)
  /**
   * Adjusts to the previous Monday
   */
  val PrevMon = ToMon(Prev)
  /**
   * Day of week aduster for Tuesday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToTue(d : Direction) = ToDay(Tue)(d)
  /**
   * Adjusts to the following Tuesday
   */
  val NextTue = ToTue(Next)
  /**
   * Adjusts to the previous Tuesday
   */
  val PrevTue = ToTue(Prev)
  /**
   * Day of week aduster for Wednesday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToWed(d : Direction) = ToDay(Wed)(d)
  /**
   * Adjusts to the following Wednesday
   */
  val NextWed = ToWed(Next)
  /**
   * Adjusts to the previous Wednesday
   */
  val PrevWed = ToWed(Prev)
  /**
   * Day of week aduster for Thursday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToThu(d : Direction) = ToDay(Thu)(d)
  /**
   * Adjusts to the following Thursday
   */
  val NextThu = ToThu(Next)
  /**
   * Adjusts to the previous Thursday
   */
  val PrevThu = ToThu(Prev)
  /**
   * Day of week aduster for Friday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToFri(d : Direction) = ToDay(Fri)(d)
  /**
   * Adjusts to the following Friday
   */
  val NextFri = ToFri(Next)
  /**
   * Adjusts to the previous Friday
   */
  val PrevFri = ToFri(Prev)
  /**
   * Day of week aduster for Saturday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToSat(d : Direction) = ToDay(Sat)(d)
  /**
   * Adjusts to the following Saturday
   */
  val NextSat = ToSat(Next)
  /**
   * Adjusts to the previous Saturday
   */
  val PrevSat = ToSat(Prev)
  /**
   * Day of week aduster for Sunday - the supplied direction will dictate whether
   * any adjustment moves into the future or past
   */
  def ToSun(d : Direction) = ToDay(Sun)(d)
  /**
   * Adjusts to the following Sunday
   */
  val NextSun = ToSun(Next)
  /**
   * Adjusts to the previous Sunday
   */
  val PrevSun = ToSun(Prev)

  /**
   * Adjust to the specific day of week, returning the target if this already satisfies
   * the criteria. Example:
   * {{{
   * val d2 = d1 >> ToDay(Mon)(Next)
   * }}}
   * This is intended to be used programmatically - for test usage, it may be easier to write
   * either of:
   * {{{
   * val d2 = d1 >> ToMon(Next)
   * val d2 = d1 >> NextMon
   * }}}
   */
  def ToDay(day : DayOfWeek)(d : Direction) = new Adjuster {
    private def nudge(in : Date) = d match {
      case Next => in + 1
      case Prev => in - 1
    }
    def adjust(in : Date) : Date = if (in.dayOfWeek == day) in else adjust(nudge(in))
  }

  /**
   * Adjust to the weekday according to the direction. Example:
   * {{{
   * val d2 = d1 >> Weekday(Next)
   * val d3 = d1 >> Weekday(Prev)
   * }}}
   */
  def Weekday(d : Direction) = new Adjuster {
    private def nudge(in : Date) = d match {
      case Next => in + 1
      case Prev => in - 1
    }
    def adjust(in : Date) : Date = if (in.isWeekday) in else adjust(nudge(in))
  }

  /**
   *   Adjust to next weekday if the supplied date is not a weekday
   */
  val NextWeekday = new Adjuster {
    def adjust(in: Date) : Date = if (in.isWeekday) in else adjust(in + 1)
  }
  /**
   * Adjust to next weekday if the supplied date is not a weekday
   */
  val PrevWeekday = new Adjuster {
    def adjust(in: Date) : Date = if (in.isWeekday) in else adjust(in - 1)
  }

  /**
   * Adjusts to the end of the month which the date is in
   */
  def MonthEnd(dir : Direction) = new Adjuster {
    def prevEom(in : Date) = in.month match {
      case Jan => in.copy(y=in.year - 1, Dec, 31)
      case _   => val m_ = in.month.prev; in.copy(m = m_, d=m_.lastDay(in.year))
    }
    def adjust(in: Date) = (dir -> in.month.lastDay(in.year)) match {
      case (_, i) if i == in.day => in //ALREADY LAST DAY OF MONTH
      case (Next, j)             => in.copy(d = j)
      case (Prev, j)             => prevEom(in)
    }
  }

  /**
   * Adjusts to the end of the month which the date is in
   */
  def MonthStart(dir : Direction) = new Adjuster {
    def adjust(in: Date) = (dir -> in.month) match {
      case (Next, Dec) => Date(in.year + 1, Jan, 1)
      case (Next, _)   => in.copy(m=in.month.next, d = 1)
      case (Prev, _)   => in.copy(d=1)
    }
  }

  /**
   * Adjusts to the end of the current year, if forwards, or the previous year if backwards
   */
  def YearEnd(dir : Direction) = new Adjuster {
    def adjust(in : Date) = {
      if (in.month == Dec && in.day == 31)
        in
      else dir match {
        case Next => in.copy(m = Dec, d = 31)
        case Prev => Date(in.year - 1, Dec, 31)
      }
    }
  }
}