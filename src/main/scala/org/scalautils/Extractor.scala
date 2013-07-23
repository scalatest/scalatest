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

/*
 Describe need to deal with flickers. Fix them so they pass 100% if you can, but if not, the problem
 is wastes people's time looking at them. Or if they happen too often it is the cry wolf problem, and people
 don't bother looking anymore when the CI server fails. They get jaded and red doesn't mean the same.
 Best thing to do is fix it, but sometimes that's not practical. In such situations, cancel when the 
 problem occurs.

 If just needed one place, just write it in place

try ...
catch {
  case e: DBAccessException if e.getMessage == "500:Internal Server Error" => cancel(e)
}

 But if you're going to use it in multiple places, make an extractor. This is what it would look like written out in full:

object InternalServerError {
  def unapply(e: Throwable): Option[Throwable] = {
    case e: DBAccessException if e.getMessage == "500:Internal Server Error" => Some(e)
    case _ => None
  }
}

  Extractor gives you a factory method for such one-item extractors:

val InternalServerError =
  Extractor[Throwable] { e: DBAccessException =>
    e.getMessage == "500:Internal Server Error"
  }

// Catcher is a convenience subclass for Extractor[Throwable]:

val InternalServerError =
  Catcher { e: DBAccessException =>
    e.getMessage == "500:Internal Server Error"
  }
*/
final case class Extractor[T](partial: PartialFunction[T, Boolean]) {
  if (partial == null) throw new NullPointerException("partial was null")

  def unapply(exception: T): Option[T] = {
    if (partial.isDefinedAt(exception) && partial(exception)) Some(exception) else None
  }
}

