package oxbow.saturn

import java.beans.PropertyEditorSupport
import java.lang.String

/**
 * Copyright oxbowlakes 2010+
 * @author Chris Marshall
 */

@serializable
@SerialVersionUID(1L)
class DateEditor extends PropertyEditorSupport {
  private[this] def date = getValue.asInstanceOf[Date]
  override def getAsText = date.toString

  override def setAsText(text: String) = setValue(Date.parse(text).getOrElse(throw new IllegalArgumentException("Cannot convert date: " + text)))
}

@serializable
@SerialVersionUID(1L)
class DateFormatEditor extends PropertyEditorSupport {
  private[this] def format = getValue.asInstanceOf[DateFormat]
  override def getAsText = format.pattern

  override def setAsText(text: String) = setValue(DateFormat(text))
}

@serializable
@SerialVersionUID(1L)
class TimeOfDayEditor extends PropertyEditorSupport {
  private[this] def time = getValue.asInstanceOf[TimeOfDay]
  override def getAsText = time.toString

  override def setAsText(text: String) = setValue(TimeOfDay.parse(text).getOrElse(throw new IllegalArgumentException("Unable to convert time: "  + text)))
}
@serializable
@SerialVersionUID(1L)
class TimeOfDayFormatEditor extends PropertyEditorSupport {
  private[this] def format = getValue.asInstanceOf[TimeOfDayFormat]
  override def getAsText = format.pattern

  override def setAsText(text: String) = setValue(TimeOfDayFormat(text))
}

@serializable
@SerialVersionUID(1L)
class InstantFormatEditor extends PropertyEditorSupport {
  private[this] def format = getValue.asInstanceOf[InstantFormat]
  override def getAsText = format.pattern

  override def setAsText(text: String) = setValue(InstantFormat(text))
}