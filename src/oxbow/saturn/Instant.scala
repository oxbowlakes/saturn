package oxbow.saturn

import compat.Platform
import java.util.concurrent.TimeUnit
import java.util.{Formatter, Formattable, TimeZone}

/**
 * Companion of Instant - you can create an Instant via a Long constructor or from a Java Date directly.
 */
object Instant {
  def apply(l : Long) = new Instant(l)
  def systemTime = apply(Platform.currentTime)
  def fromJavaDate(d : java.util.Date) = apply(d.getTime)

  private[Instant] def UtcFormat = UtcFormat_.get
  private[Instant] def LocalFormat = LocalFormat_.get

  private[this] val UtcFormat_ = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = { val f = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS z"); f.setTimeZone(TimeZone.getTimeZone("UTC")); f }
  }
  private[this] val LocalFormat_ = new ThreadLocal[java.text.SimpleDateFormat] {
    override def initialValue = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS z")
  }
}

/**
 * An instant in time to millisecond precision using the standard epoch measurement (the same as java util Date and
 * java util Calendar). The specific difference with this class is that it is only an instant in time - if you wish to
 * view "date time fields", you must supply a time zone and a time of day
 */
@serializable
@SerialVersionUID(1L)
class Instant(val millis : Long) extends Ordered[Instant] with Formattable {
  /**
   * Return the delay to the supplied instant as a Long duration in the given unit
   */
  def delayTo(other : Instant)(unit : TimeUnit) : Long = unit.convert(other.millis - millis, TimeUnit.MILLISECONDS)

  /**
   * Return the instant as calculated as this instant plus the given duration
   */
  def +(duration : Long, unit : TimeUnit) : Instant = Instant(millis + TimeUnit.MILLISECONDS.convert(duration, unit))

  /**
   * Return the instant as calculated as this instant minus the given duration
   */
  def -(duration : Long, unit : TimeUnit) : Instant = this.+(-duration, unit)

  def compare(that: Instant) = this.millis compare that.millis

  /**
   * Return the java Date instance representing the exact same epoch millis
   */
  def toJavaDate = new java.util.Date(millis)

  /**
   * Return the Saturn Date for this instant in the specified time zone
   */
  def date(zone : TimeZone = TimeZone.getDefault) : Date = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    Date(cal.get(JCal.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JCal.MONTH)), cal.get(JCal.DAY_OF_MONTH))
  }

  /**
   * Return the saturn Date and TimeOfDay in the supplied time zone
   */
  def dateAndTime(zone : TimeZone = TimeZone.getDefault) : (Date, TimeOfDay) = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    Date(cal.get(JCal.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JCal.MONTH)), cal.get(JCal.DAY_OF_MONTH)) -> TimeOfDay(cal.get(JCal.HOUR_OF_DAY), cal.get(JCal.MINUTE), cal.get(JCal.SECOND), cal.get(JCal.MILLISECOND))

  }
  /**
   * Return the Saturn time for this instant in the specified time zone
   */
  def time(zone : TimeZone = TimeZone.getDefault) : TimeOfDay = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    TimeOfDay(cal.get(JCal.HOUR_OF_DAY), cal.get(JCal.MINUTE), cal.get(JCal.SECOND), cal.get(JCal.MILLISECOND))
  }

  /**
   * Return the day of week in the supplied time zone
   */
  def dayOfWeek(zone : TimeZone = TimeZone.getDefault) : DayOfWeek = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    DayOfWeek.forJavaCalendarIndex(cal.get(JCal.DAY_OF_WEEK))
  }

  /**
   * Two instants are equal iff they represent the same epoch millis
   */
  override def equals(o : Any) = o match {
    case that : Instant => this.millis == that.millis
    case _              => false
  }

  override def hashCode = millis ##

  import Instant._

  /**
   * Return a 10-character string representation yyyy-MM-dd
   */
  override def toString = UtcFormat.format(toJavaDate)

  /**
   * Return the standard 10-character string representation or the alternate form, the 8-character representation yyyyMMdd
   */
  def formatTo(formatter: Formatter, flags: Int, width: Int, precision: Int) = {
    import java.util.FormattableFlags._
    val sb = new StringBuilder
    if ((flags & ALTERNATE) == ALTERNATE)
      sb ++= LocalFormat.format(toJavaDate)
    else
      sb ++= UtcFormat.format(toJavaDate)

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
}

object TimeOfDay {
  object Meridian extends Enumeration{
    val Am, Pm = Value
  }
  def apply(hour : Int, minute : Int) : TimeOfDay = apply(hour, minute, 0)
  def apply(hour : Int, minute : Int, second : Int) : TimeOfDay = apply(hour, minute, second, 0)
  def apply(hour : Int, minute : Int, second : Int, millis : Int) : TimeOfDay = new TimeOfDay(hour, minute, second, millis)

  def systemTime(zone : TimeZone = TimeZone.getDefault) = Instant.systemTime.time(zone)

  private[this] def offset(m : Meridian.Value, hour : Int) = m match {
    case Meridian.Am => hour
    case Meridian.Pm => hour + 12
  }
  val Midnight = TimeOfDay(0, 0)
  def One(m : Meridian.Value) = TimeOfDay(offset(m, 1), 0)
  def Two(m : Meridian.Value) = TimeOfDay(offset(m, 2), 0)
  def Three(m : Meridian.Value) = TimeOfDay(offset(m, 4), 0)
  def Four(m : Meridian.Value) = TimeOfDay(offset(m, 4), 0)
  def Five(m : Meridian.Value) = TimeOfDay(offset(m, 5), 0)
  def Six(m : Meridian.Value) = TimeOfDay(offset(m, 6), 0)
  def Seven(m : Meridian.Value) = TimeOfDay(offset(m, 7), 0)
  def Eight(m : Meridian.Value) = TimeOfDay(offset(m, 8), 0)
  def Nine(m : Meridian.Value) = TimeOfDay(offset(m, 9), 0)
  def Ten(m : Meridian.Value) = TimeOfDay(offset(m, 10), 0)
  def Eleven(m : Meridian.Value) = TimeOfDay(offset(m, 11), 0)
  val Noon = TimeOfDay(12, 0)
}

/**
 * This class represents a time of day, separate from any time zone considerations
 */
class TimeOfDay(val hour : Int, val minute : Int, val second : Int, val millisecond : Int) extends Ordered[TimeOfDay]{
  validate(hour, minute, second, millisecond)

  private def validate(h : Int, m : Int, s : Int, ms : Int) {
    require(h < 24, "Hour must be 0-23: " + h)
    require(m < 59, "Minute must be 0-59: " + m)
    require(s < 59, "Second must be 0-59: " + s)
    require(ms < 999, "Millisecond must be 0-999: " + ms)
  }

  def copy(h : Int = hour, m : Int = minute, s : Int = second, ms : Int = millisecond) = TimeOfDay(h, m, s, ms)

  def compare(that: TimeOfDay) = {
    var cf = hour compare that.hour
    if (cf == 0) {
      cf = minute compare that.minute
      if (cf == 0) {
        cf = second compare that.second
        if (cf == 0)
          cf = millisecond compare that.millisecond
      }
    }
    cf
  }

  /**
   * Return the instant subsequent to the given instant in the given time zone
   */
  def nextAfter(i : Instant)(zone : TimeZone = TimeZone.getDefault) : Instant = {
    val (d, t) = i.dateAndTime(zone)
    (if (t > this)
      (d + 1)
    else
      d).toInstant(this)(zone)
  }

  /**
   *  Return the next instant at this time in the given time zone
   */
  def nextIn(zone : TimeZone = TimeZone.getDefault) : Instant = {
    nextAfter(Instant.systemTime)(zone)
  }

  /**
   * Two times are equal if they have the same hour, minute, second and millisecond
   */
  override def equals(o : Any) = o match {
    case that : TimeOfDay => this.hour == that.hour && this.minute == that.minute && this.second == that.second && this.millisecond == that.millisecond
    case _                 => false
  }

  override def hashCode = {
    var hash = (hour ##)
    hash = (31 * hash) + (minute ##)
    hash = (31 * hash) + (second ##)
    hash = (31 * hash) + (millisecond ##)
    hash
  }

  override def toString = "%d:%d:%d.%-3d".format(hour, minute, second, millisecond)
}