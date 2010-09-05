package oxbow.saturn

import compat.Platform
import java.util.{Formatter, Formattable, TimeZone, Date => JDate, Calendar => JCal}

/**
 * Companion for DayOfWeek
 */
object DayOfWeek {

  def forJavaCalendarIndex(i : Int) = i match {
      case JCal.MONDAY    => Mon
      case JCal.TUESDAY   => Tue
      case JCal.WEDNESDAY => Wed
      case JCal.THURSDAY  => Thu
      case JCal.FRIDAY    => Fri
      case JCal.SATURDAY  => Sat
      case JCal.SUNDAY    => Sun
    }

  def dayOfWeekIndex(d : DayOfWeek) : Int = d match {
    case Mon => 1
    case Tue => 2
    case Wed => 3
    case Thu => 4
    case Fri => 5
    case Sat => 6
    case Sun => 7
  }
}

sealed trait DayOfWeek extends Ordered[DayOfWeek] {
  import DayOfWeek._
  def index = dayOfWeekIndex(this)

  def compare(that: DayOfWeek) = this.index compare that.index

  def isWeekday = this match {
    case Mon | Tue | Wed | Thu | Fri => true
    case _                           => false
  }
}

case object Mon extends DayOfWeek
case object Tue extends DayOfWeek
case object Wed extends DayOfWeek
case object Thu extends DayOfWeek
case object Fri extends DayOfWeek
case object Sat extends DayOfWeek
case object Sun extends DayOfWeek

sealed trait Month extends Ordered[Month] {
  import Month._
  def index = monthIndex(this)

  def compare(that: Month) = index compare that.index
  def next = this match {
    case Dec => Jan
    case _   => Month.forIndex(index + 1)
  }
  
  def prev = this match {
   case Jan => Dec
   case _   => Month.forIndex(index - 1)
  }

  def lastDay(year : Int) : Int = this match {
      case Jan | Mar | May | Jul | Aug | Oct | Dec  => 31
      case Apr | Jun | Sep | Nov                    => 30
      case Feb if Date.isLeapYear(year)             => 29
      case Feb                                      => 28
    }

  private[saturn] def validate(day : Int, year : Int) = {
    require(day > 0, "Day must be a positive value: " +day)
    require(day <= lastDay(year), "Day must be less than [%d] in %s".format(lastDay(year), this))
  }
}
@serializable
@SerialVersionUID(1L)
case object Jan extends Month

@serializable
@SerialVersionUID(1L)
case object Feb extends Month

@serializable
@SerialVersionUID(1L)
case object Mar extends Month

@serializable
@SerialVersionUID(1L)
case object Apr extends Month

@serializable
@SerialVersionUID(1L)
case object May extends Month

@serializable
@SerialVersionUID(1L)
case object Jun extends Month

@serializable
@SerialVersionUID(1L)
case object Jul extends Month

@serializable
@SerialVersionUID(1L)
case object Aug extends Month

@serializable
@SerialVersionUID(1L)
case object Sep extends Month

@serializable
@SerialVersionUID(1L)
case object Oct extends Month

@serializable
@SerialVersionUID(1L)
case object Nov extends Month

@serializable
@SerialVersionUID(1L)
case object Dec extends Month

object Month {
  def javaCalendarMonthIndex(month : Month) : Int = monthIndex(month) - 1

  def forJavaCalendarMonthIndex(i : Int) = forIndex(i + 1)


  def forIndex(i : Int) = i match {
    case 1 => Jan
    case 2 => Feb
    case 3 => Mar
    case 4 => Apr
    case 5 => May
    case 6 => Jun
    case 7 => Jul
    case 8 => Aug
    case 9 => Sep
    case 10 => Oct
    case 11 => Nov
    case 12 => Dec
    case _ => throw new IllegalArgumentException("Bad month index [%d]. Must be 1-12".format(i))
  }

  def monthIndex(month : Month) = month match {
    case Jan => 1
    case Feb => 2
    case Mar => 3
    case Apr => 4
    case May => 5
    case Jun => 6
    case Jul => 7
    case Aug => 8
    case Sep => 9
    case Oct => 10
    case Nov => 11
    case Dec => 12
  }
}


object Date {
  private[this] val formats = ("yyyy-MM-dd" :: "yyyyMMdd" :: "yyyy-MMM-dd" :: "yyyyMMMdd" :: "dd-MMM-yyyy" :: "ddMMMyyyy" :: Nil).map(DateFormat(_))
  def parse(str : String) : Option[Date] = formats.view.map(f => f synchronized { f.parse(str) }).find(_.isRight).flatMap(_.right.toOption) 

  def apply(y : Int, m : Int, d : Int) : Date = new Date(y, Month.forIndex(m), d)
  def apply(y : Int, m : Month, d : Int) : Date = new Date(y, m, d)
  def systemDate(zone : TimeZone = TimeZone.getDefault) : Date = {
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(Platform.currentTime)
    Date(cal.get(JCal.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JCal.MONTH)), cal.get(JCal.DAY_OF_MONTH))
  }

  private[this] val LeapYearCalc = new java.util.GregorianCalendar
  def isLeapYear(year : Int) : Boolean = LeapYearCalc.isLeapYear(year)
}

@serializable
@SerialVersionUID(1L)
class Date(val year : Int, val month : Month, val day : Int) extends Ordered[Date] with Formattable {
  validate(year, month, day)

  def copy(y : Int = year, m : Month = month, d : Int = day) = Date(y, m, d)

  private def validate(year : Int, month : Month, day : Int) = {
    require(year >= 0, "Year must be positive: " + year)
    month.validate(day, year)
  }

  def toJavaDate(time : TimeOfDay)(zone : TimeZone = TimeZone.getDefault) = toInstant(time)(zone).toJavaDate

  def toInstant(time : TimeOfDay)(zone : TimeZone = TimeZone.getDefault) = {
    val cal = JCal.getInstance(zone)
    cal.set(JCal.YEAR, year)
    cal.set(JCal.MONTH, Month.javaCalendarMonthIndex(month))
    cal.set(JCal.DAY_OF_MONTH, day)

    //NOW ADD
    cal.set(JCal.HOUR_OF_DAY, time.hour)
    cal.set(JCal.MINUTE, time.minute)
    cal.set(JCal.SECOND, time.second)
    cal.set(JCal.MILLISECOND, time.millisecond)
    Instant(cal.getTimeInMillis)
  }

  def -(days : Int) = minus(days)
  def +(days : Int) = plus(days)
  def plus(days : Int) : Date = {
    val cal = JCal.getInstance
    cal.set(JCal.YEAR, year)
    cal.set(JCal.MONTH, Month.javaCalendarMonthIndex(month))
    cal.set(JCal.DAY_OF_MONTH, day)

    //NOW ADD
    cal.add(JCal.DATE, days)

    import Date._
    Date(cal.get(JCal.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JCal.MONTH)), cal.get(JCal.DAY_OF_MONTH))
  }

  def >>(a : Adjuster) = adjust(a) 
  def adjust(a : Adjuster) : Date = a adjust this

  def minus(days : Int) = plus(-days)

  def compare(that: Date) = {
    var cf = year compare that.year
    if (cf == 0) {
      cf = month compare that.month
      if (cf == 0) {
        cf = day compare that.day
      }
    }
    cf
  }

  def dayOfWeek = {
    val cal = JCal.getInstance
    cal.set(JCal.YEAR, year)
    cal.set(JCal.MONTH, Month.javaCalendarMonthIndex(month))
    cal.set(JCal.DAY_OF_MONTH, day)

    DayOfWeek.forJavaCalendarIndex(cal.get(JCal.DAY_OF_WEEK))
  }

  def isWeekend = !isWeekday

  def isWeekday = dayOfWeek isWeekday

  def isLeapYear = Date.isLeapYear(year)

  override def toString = mkString("-")

  def mkString(sep : String) = {
    val sb = new StringBuilder
    sb ++= year.toString ++= sep
    if (month.index < 10) sb += '0'
    sb ++= month.index.toString ++= sep
    if (day < 10) sb += '0'
    sb ++= day.toString
    sb.mkString

  }

  /**
   * Two Dates are equal if they have the same year, month and day
   */
  override def equals(o : Any) = o match {
    case other : Date => year == other.year && month == other.month && day == other.day
    case _            => false
  }

  override def hashCode = {
    var hash = (year ##)
    hash = (hash * 31) + (month ##)
    hash = (hash * 31) + (day ##)
    hash
  }

  def formatTo(formatter: Formatter, flags: Int, width: Int, precision: Int) = {
    import java.util.FormattableFlags._
    val sb = new StringBuilder
    sb ++= (if ((flags & ALTERNATE) == ALTERNATE) mkString("") else mkString("-"))
    val len = sb.length
    if (len < width) {
      (0 until (width - len)) foreach { i =>
        if ((flags & LEFT_JUSTIFY) == LEFT_JUSTIFY)
          sb :+ ' '
        else
          ' ' +: sb
      }
    }
    formatter.format(sb.mkString)
  }

  def after(d : Date) = this > d
  def before(d : Date) = this < d

  def next = plus(1)
  def previous = minus(1)
}