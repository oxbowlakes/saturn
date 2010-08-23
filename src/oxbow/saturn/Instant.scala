package oxbow.saturn

import compat.Platform
import java.util.concurrent.TimeUnit
import java.util.{Formatter, Formattable, TimeZone}

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
@serializable
@SerialVersionUID(1L)
class Instant(val millis : Long) extends Ordered[Instant] with Formattable {
  def delayTo(other : Instant)(unit : TimeUnit) : Long = unit.convert(other.millis - millis, TimeUnit.MILLISECONDS)

  def +(duration : Long, unit : TimeUnit) : Instant = Instant(millis + TimeUnit.MILLISECONDS.convert(duration, unit))
  def -(duration : Long, unit : TimeUnit) : Instant = this.+(-duration, unit)

  def compare(that: Instant) = this.millis compare that.millis

  def toJavaDate = new java.util.Date(millis)

  def date(zone : TimeZone = TimeZone.getDefault) : Date = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    Date(cal.get(JCal.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JCal.MONTH)), cal.get(JCal.DAY_OF_MONTH))
  }

  def time(zone : TimeZone = TimeZone.getDefault) : TimeOfDay = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    TimeOfDay(cal.get(JCal.HOUR_OF_DAY), cal.get(JCal.MINUTE), cal.get(JCal.SECOND), cal.get(JCal.MILLISECOND))
  }

  def dayOfWeek(zone : TimeZone = TimeZone.getDefault) : DayOfWeek = {
    import java.util.{Calendar => JCal}
    val cal = JCal.getInstance(zone)
    cal.setTimeInMillis(millis)
    DayOfWeek.forJavaCalendarIndex(cal.get(JCal.DAY_OF_WEEK))
  }

  override def equals(o : Any) = o match {
    case that : Instant => this.millis == that.millis
    case _              => false
  }

  override def hashCode = millis ##

  import Instant._
  override def toString = UtcFormat.format(toJavaDate)

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

class TimeOfDay(val hour : Int, val minutes : Int, val seconds : Int, val milliseconds : Int) extends Ordered[TimeOfDay]{

  def copy(h : Int = hour, m : Int = minutes, s : Int = seconds, ms : Int = milliseconds) = TimeOfDay(h, m, s, ms)

  def compare(that: TimeOfDay) = {
    var cf = hour compare that.hour
    if (cf == 0) {
      cf = minutes compare that.minutes
      if (cf == 0) {
        cf = seconds compare that.seconds
        if (cf == 0)
          cf = milliseconds compare that.milliseconds
      }
    }
    cf
  }

  def next(zone : TimeZone = TimeZone.getDefault) : Instant = {

    val now = Instant.systemTime
    if (now.time(zone) > this)
      (now.date(zone) + 1).toInstant(this)(zone)
    else
      (now.date(zone)).toInstant(this)(zone)
  }


  override def toString = "%d:%d:%d.%-3d".format(hour, minutes, seconds, milliseconds)
}