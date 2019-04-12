/*
 * Copyright 2001-2019 Artima, Inc.
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
package org.scalatestplus.scalacheck

import java.util.ResourceBundle
import java.text.MessageFormat

private[scalacheck] object Resources {

  lazy val resourceBundle = ResourceBundle.getBundle("org.scalatestplus.scalacheck.MessageBundle")

  def makeString(resourceName: String, args: Array[Any]): String = {
    val raw = resourceBundle.getString(resourceName)
    formatString(raw, args)
  }

  def formatString(rawString: String, args: Array[Any]): String = {
    val msgFmt = new MessageFormat(rawString)
    msgFmt.format(args.toArray)
  }

  def propCheckLabel(): String = resourceBundle.getString("propCheckLabel")

  def rawPropCheckLabel: String = resourceBundle.getString("propCheckLabel")

  def propCheckLabels(): String = resourceBundle.getString("propCheckLabels")

  def rawPropCheckLabels: String = resourceBundle.getString("propCheckLabels")

  def propertyException(param0: Any): String = makeString("propertyException", Array(param0))

  def rawPropertyException: String = resourceBundle.getString("propertyException")

  def propCheckExhausted(param0: Any, param1: Any): String = makeString("propCheckExhausted", Array(param0, param1))

  def rawPropCheckExhausted: String = resourceBundle.getString("propCheckExhausted")

  def propCheckExhaustedAfterOne(param0: Any): String = makeString("propCheckExhaustedAfterOne", Array(param0))

  def rawPropCheckExhaustedAfterOne: String = resourceBundle.getString("propCheckExhaustedAfterOne")

  def propertyFailed(param0: Any): String = makeString("propertyFailed", Array(param0))

  def rawPropertyFailed: String = resourceBundle.getString("propertyFailed")

  def propertyCheckSucceeded(): String = resourceBundle.getString("propertyCheckSucceeded")

  def rawPropertyCheckSucceeded: String = resourceBundle.getString("propertyCheckSucceeded")

  def thrownExceptionsLocation(param0: Any): String = makeString("thrownExceptionsLocation", Array(param0))

  def rawThrownExceptionsLocation: String = resourceBundle.getString("thrownExceptionsLocation")

  def occurredOnValues(): String = resourceBundle.getString("occurredOnValues")

  def rawOccurredOnValues: String = resourceBundle.getString("occurredOnValues")

  def thrownExceptionsMessage(param0: Any): String = makeString("thrownExceptionsMessage", Array(param0))

  def rawThrownExceptionsMessage: String = resourceBundle.getString("thrownExceptionsMessage")
}
