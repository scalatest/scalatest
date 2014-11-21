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

final class OddInt private (val value: Int) extends AnyVal {
  override def toString: String = s"OddInt($value)"
}

object OddInt {
  def from(value: Int): Option[OddInt] =
    if (value % 2 == 1) Some(new OddInt(value)) else None
  import scala.language.experimental.macros
  def apply(value: Int): OddInt = macro OddIntMacro.apply
}

