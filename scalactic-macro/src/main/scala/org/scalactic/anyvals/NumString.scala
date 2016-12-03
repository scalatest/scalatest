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

private[scalactic] final class NumString private (val value: String) extends AnyVal {
  override def toString: String = s"NumString($value)"
}

private[scalactic] object NumString {
  def from(value: String): Option[NumString] =
    if (NumStringMacro.isValid(value)) Some(new NumString(value)) else None

  def ensuringValid(value: String): NumString =
    if (NumStringMacro.isValid(value)) new NumString(value) else {
      throw new AssertionError(s"$value was not a valid NumString")
    }

  import scala.language.experimental.macros
  def apply(value: String): NumString = macro NumStringMacro.apply
}

