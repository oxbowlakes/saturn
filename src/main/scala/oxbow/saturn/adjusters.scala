package oxbow.saturn

/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */
trait Adjuster {
  def adjust(in : Date) : Date
}

object adjusters {
  /**
   * Adjust to next weekday if the supplied date is not a weekday
   */
  val WeekdayForward = new Adjuster {
    def adjust(in: Date) : Date = if (in.isWeekday) in else adjust(in + 1)
  }
  /**
   * Adjust to next weekday if the supplied date is not a weekday
   */
  val WeekdayBackward = new Adjuster {
    def adjust(in: Date) : Date = if (in.isWeekday) in else adjust(in - 1)
  }

  val MonthEnd = new Adjuster {
    def adjust(in: Date) = in.month.lastDay(in.year) match {
      case i if i == in.day => in
      case j                => in.copy(d = j)
    }
  }
}