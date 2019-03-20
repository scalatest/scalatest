/*
 * Copyright 2001-2018 Artima, Inc.
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
package org.scalatestplus.testng

import java.util.ResourceBundle
import java.text.MessageFormat

private[testng] object Resources {

  lazy val resourceBundle = ResourceBundle.getBundle("org.scalatestplus.testng.MessageBundle")

  def makeString(resourceName: String, args: Array[Any]): String = {
    val raw = resourceBundle.getString(resourceName)
    formatString(raw, args)
  }

  def formatString(rawString: String, args: Array[Any]): String = {
    val msgFmt = new MessageFormat(rawString)
    msgFmt.format(args.toArray)
  }

  def testSucceededIconChar(): String = resourceBundle.getString("testSucceededIconChar")

  def rawTestSucceededIconChar: String = resourceBundle.getString("testSucceededIconChar")

  def iconPlusShortName(param0: Any, param1: Any): String = makeString("iconPlusShortName", Array(param0, param1))

  def rawIconPlusShortName: String = resourceBundle.getString("iconPlusShortName")

  def testNotFound(param0: Any): String = makeString("testNotFound", Array(param0))

  def rawTestNotFound: String = resourceBundle.getString("testNotFound")

  def testNGConfigFailed(): String = resourceBundle.getString("testNGConfigFailed")

  def rawTestNGConfigFailed: String = resourceBundle.getString("testNGConfigFailed")

}
