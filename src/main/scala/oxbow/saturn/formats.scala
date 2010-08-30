package oxbow.saturn

import java.lang.String
import java.util.TimeZone
import java.text._

private[saturn] trait Zoned {
  private[saturn] val Utc = TimeZone.getTimeZone("UTC")
}

object DateFormat extends Zoned {
  def apply(pattern : String) =  {
    val f = new SimpleDateFormat(pattern)
    f.setTimeZone(Utc)
    new DateFormat(f)
  }
}
/**
 * @author Chris Marshall
 */
@serializable
@SerialVersionUID(1L)
class DateFormat private(val inner : SimpleDateFormat) extends Format {
  import DateFormat._

  def pattern = inner.toPattern

  def parse(str : String) : Either[ParseException, Date] = try { Right(parseObject(str).asInstanceOf[Date]) } catch { case p : ParseException => Left(p)}

  def parseObject(source: String, pos: ParsePosition) = {
    import java.util.{Calendar => JC, Date => JD}
    def from(d : JD) = {
      val cal = JC.getInstance(Utc)
      cal.setTime(d)
      Date(cal.get(JC.YEAR), Month.forJavaCalendarMonthIndex(cal.get(JC.MONTH)), cal.get(JC.DAY_OF_MONTH))
    }
    Option(inner.parse(source, pos)).map(from(_)).orNull
  }

  def format(obj: AnyRef, toAppendTo: StringBuffer, pos: FieldPosition) = {
    import java.util.{Calendar => JC}
    def jdate(d : Date) = {
      val cal = JC.getInstance(Utc)
      cal.set(JC.YEAR, d.year)
      cal.set(JC.MONTH, Month.javaCalendarMonthIndex(d.month))
      cal.set(JC.DAY_OF_MONTH, d.day)
      cal.getTime
    }
    obj match {
      case d : Date => inner.format(jdate(d), toAppendTo, pos)
      case _        => throw new IllegalArgumentException("Not a Date: " + obj)
    }
  }

  override def hashCode = inner.hashCode
  override def equals(other : Any) = other match {
    case that : DateFormat => this.inner == that.inner
    case _                 => false
  }
}

object TimeOfDayFormat extends Zoned {
  def apply(pattern: String) = new TimeOfDayFormat(new SimpleDateFormat(pattern))
}

@serializable
@SerialVersionUID(1L)
class TimeOfDayFormat private(val inner : SimpleDateFormat) extends Format {
  import TimeOfDayFormat._

  def parse(str : String) : Either[ParseException, TimeOfDay] = try { Right(parseObject(str).asInstanceOf[TimeOfDay]) } catch { case p : ParseException => Left(p)}

  def pattern = inner toPattern
  def parseObject(source: String, pos: ParsePosition) = {
    import java.util.{Calendar => JC, Date => JD}
    def from(d : JD) = {
      val cal = JC.getInstance(Utc)
      cal.setTime(d)
      TimeOfDay(cal.get(JC.HOUR_OF_DAY), cal.get(JC.MINUTE), cal.get(JC.SECOND), cal.get(JC.MILLISECOND))
    }

    Option(inner.parse(source, pos)).map(from(_)).orNull
  }

  def format(obj: AnyRef, toAppendTo: StringBuffer, pos: FieldPosition) = {
    import java.util.{Calendar => JC}
    def jdate(t : TimeOfDay) = {
      val cal = JC.getInstance(Utc)
      cal.set(JC.HOUR_OF_DAY, t.hour)
      cal.set(JC.MINUTE, t.minute)
      cal.set(JC.SECOND, t.second)
      cal.set(JC.MILLISECOND, t.millisecond)
      cal.getTime
    }
    obj match {
      case t : TimeOfDay => inner.format(jdate(t), toAppendTo, pos)
      case _             => throw new IllegalArgumentException("Not a Time: " + obj)
    }
  }
  override def hashCode = inner.hashCode
  override def equals(other : Any) = other match {
    case that : TimeOfDayFormat => this.inner == that.inner
    case _                      => false
  }
}

object InstantFormat {
  def apply(pattern : String, zone : TimeZone = TimeZone.getDefault) = {
    val f = new SimpleDateFormat
    f.setTimeZone(zone)
    new InstantFormat(f)
  }
}
@serializable
@SerialVersionUID(1L)
class InstantFormat private(val inner : SimpleDateFormat) extends Format {
  def pattern = inner toPattern
  def parseObject(source: String, pos: ParsePosition) = {
    Option(inner.parse(source, pos)).map(d => Instant(d.getTime)).orNull
  }

  def format(obj: AnyRef, toAppendTo: StringBuffer, pos: FieldPosition) = {
    import java.util.{Date => JD}
    def jdate(i : Instant) = new JD(i.millis)
    obj match {
      case i : Instant => inner.format(jdate(i), toAppendTo, pos)
      case _           => throw new IllegalArgumentException("Not an Instant: " + obj)
    }
  }

  override def hashCode = inner hashCode
  override def equals(other : Any) = other match {
    case that : InstantFormat => this.inner == that.inner
    case _                    => false
  }
}