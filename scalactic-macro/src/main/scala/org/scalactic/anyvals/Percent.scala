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

// These are here emporaritly for testing, because need to have the compile separate. Can't I just move them?
private[scalactic] final class Percent private (val value: Int) extends AnyVal {
  override def toString: String = s"Percent($value)"
}

private[scalactic] object Percent {

  def from(value: Int): Option[Percent] =
    if (PercentMacro.isValid(value)) Some(new Percent(value)) else None

  def ensuringValid(value: Int): Percent =
    if (PercentMacro.isValid(value)) new Percent(value) else {
      throw new AssertionError(s"$value was not a valid Percent")
    }

  import scala.language.experimental.macros
  def apply(value: Int): Percent = macro PercentMacro.apply
}

