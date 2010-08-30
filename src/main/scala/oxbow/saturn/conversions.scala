package oxbow.saturn

/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

object Conversions {
  implicit def string2richstring(s : String) = new {
    def toDate = Date.parse(s) getOrElse (throw new IllegalArgumentException("Cannot parse [" + s + "] as a Date"))
    def toTimeOfDay = TimeOfDay.parse(s) getOrElse (throw new IllegalArgumentException("Cannot parse [" + s + "] as a TimeOfDay"))
  }
}