package oxbow.saturn

/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */
object Adjuster {
  sealed trait Direction
  case object Next extends Direction
  case object Prev extends Direction
}
trait Adjuster {
  def adjust(in : Date) : Date
}

object adjusters {
  import Adjuster._

  def ToMon(d : Direction) = ToDay(Mon)(d)
  def ToTue(d : Direction) = ToDay(Tue)(d)
  def ToWed(d : Direction) = ToDay(Wed)(d)
  def ToThu(d : Direction) = ToDay(Thu)(d)
  def ToFri(d : Direction) = ToDay(Fri)(d)
  def ToSat(d : Direction) = ToDay(Sat)(d)
  def ToSun(d : Direction) = ToDay(Sun)(d)

  def ToDay(day : DayOfWeek)(d : Direction) = new Adjuster {
    private def nudge(in : Date) = d match {
      case Next => in + 1
      case Prev => in - 1
    }
    def adjust(in : Date) : Date = if (in.dayOfWeek == day) in else adjust(nudge(in))
  }

  def Weekday(d : Direction) = new Adjuster {
    private def nudge(in : Date) = d match {
      case Next => in + 1
      case Prev => in - 1
    }
    def adjust(in : Date) : Date = if (in.isWeekday) in else adjust(nudge(in))
  }
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