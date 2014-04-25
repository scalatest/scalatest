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
package org.scalatest

import java.util.ResourceBundle
import java.text.MessageFormat
import org.scalactic.Prettifier

/**
 * Grab a resource intended for use in a failure message. For each argument passed,
 * convert it to a string by calling decorateToStringValue, which will do things such
 * as highlight differences in two strings that were supposd to be equal.
 *
 * @author Bill Venners
 */
private[scalatest] object FailureMessages {
  
  def decorateToStringValue(o: Any): String = Prettifier.default(o)

  def apply(resourceName: String): String = Resources(resourceName)
  def apply(resourceName: String, args: Any*): String =
    Resources(resourceName, args.map((arg: Any) => decorateToStringValue(arg)): _*)

}

// This is used to pass a string to the FailureMessages apply method
// but prevent it from being quoted. This is useful when using a string
// to talk about method names, for example.
private[scalatest] class UnquotedString(s: String) {
  override def toString = s
  override def equals(other: Any): Boolean =
    other match {
      case that: UnquotedString => s == that.toString
      case _ => false
    }
  override def hashCode: Int = s.hashCode
}

private[scalatest] object UnquotedString {
  def apply(s: String) = new UnquotedString(s)
}

