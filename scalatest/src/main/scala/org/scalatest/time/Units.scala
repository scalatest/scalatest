/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.time

import org.scalatest.Resources

/**
 * Defines a family of singleton objects representing units of time.
 *
 * <p>
 * The singleton objects that extend this abstract class may be passed
 * to the constructor of <a href="Span.html"><code>Span</code></a> to specify
 * units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Second)
 * </pre>
 */
sealed abstract class Units {
  private[scalatest] def singularMessageFun(lengthString: String): String
  private[scalatest] def pluralMessageFun(lengthString: String): String
}

/**
 * Indicates units for a single nanosecond.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify nanosecond units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Nanosecond)
 * </pre>
 */
case object Nanosecond extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularNanosecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralNanosecondUnits(lengthString)
}

/**
 * Indicates nanosecond units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify nanosecond units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Nanoseconds)
 * </pre>
 */
case object Nanoseconds extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularNanosecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralNanosecondUnits(lengthString)
}

/**
 * Indicates units for a single microsecond.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify microsecond units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Microsecond)
 * </pre>
 */
case object Microsecond extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMicrosecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMicrosecondUnits(lengthString)
}

/**
 * Indicates microsecond units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify microsecond units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Microseconds)
 * </pre>
 */
case object Microseconds extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMicrosecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMicrosecondUnits(lengthString)
}

/**
 * Indicates units for a single millisecond.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify millisecond units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Millisecond)
 * </pre>
 */
case object Millisecond extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMillisecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMillisecondUnits(lengthString)
}

/**
 * Indicates millisecond units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify millisecond units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Milliseconds)
 * </pre>
 */
case object Milliseconds extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMillisecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMillisecondUnits(lengthString)
}
/**
 * Indicates millisecond units (shorthand form).
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify millisecond units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Millis)
 * </pre>
 *
 * <p>
 * <em>Note: <code>Millis</code> is merely a shorthand for <a href="Milliseconds$.html"><code>Milliseconds</code></a>.
 * When passed to <code>Span</code>, <code>Millis</code> means exactly the same thing as
 * <code>Milliseconds</code>.</em>
 * </p>
 */
case object Millis extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMillisecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMillisecondUnits(lengthString)
}

/**
 * Indicates units for a single second.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify second units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Second)
 * </pre>
 */
case object Second extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularSecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralSecondUnits(lengthString)
}

/**
 * Indicates second units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify second units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Seconds)
 * </pre>
 */
case object Seconds extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularSecondUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralSecondUnits(lengthString)
}

/**
 * Indicates units for a single minute.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify minute units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Minute)
 * </pre>
 */
case object Minute extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMinuteUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMinuteUnits(lengthString)
}

/**
 * Indicates minute units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify minute units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Minutes)
 * </pre>
 */
case object Minutes extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularMinuteUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralMinuteUnits(lengthString)
}

/**
 * Indicates units for a single hour.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify hour units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Hour)
 * </pre>
 */
case object Hour extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularHourUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralHourUnits(lengthString)
}

/**
 * Indicates hour units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify hour units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Hours)
 * </pre>
 */
case object Hours extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularHourUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralHourUnits(lengthString)
}

/**
 * Indicates units for a single day.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify day units of time, so long as the value passed to <code>Span</code> is 1. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(1, Day)
 * </pre>
 */
case object Day extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularDayUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralDayUnits(lengthString)
}

/**
 * Indicates day units.
 *
 * <p>
 * This singleton object may be passed to the constructor of <a href="Span.html"><code>Span</code></a> to
 * specify day units of time. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * Span(10, Days)
 * </pre>
 */
case object Days extends Units {
  private[scalatest] def singularMessageFun(lengthString: String): String = Resources.singularDayUnits(lengthString)
  private[scalatest] def pluralMessageFun(lengthString: String): String = Resources.pluralDayUnits(lengthString)
}

