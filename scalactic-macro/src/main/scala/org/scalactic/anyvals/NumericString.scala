/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

private[scalactic] final class NumericString private (val value: String) extends AnyVal {
  override def toString: String = s"NumericString($value)"
}

private[scalactic] object NumericString {
  def from(value: String): Option[NumericString] =
    if (NumericStringMacro.isValid(value)) Some(new NumericString(value)) else None

  def ensuringValid(value: String): NumericString =
    if (NumericStringMacro.isValid(value)) new NumericString(value) else {
      throw new AssertionError(s"$value was not a valid NumericString")
    }

  import scala.language.experimental.macros
  def apply(value: String): NumericString = macro NumericStringMacro.apply
}

