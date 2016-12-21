/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalactic.anyvals._
import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

package object prop {

  /**
   * <strong>Checkers has been moved from org.scalatest.prop to org.scalatest.check. Please update
   * your imports, as this deprecated type alias will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated("Please use org.scalatest.check.Checkers instead.", "ScalaTest 3.1.0")
  type Checkers = org.scalatest.check.Checkers

  /**
   * <strong>Checkers has been moved from org.scalatest.prop to org.scalatest.check. Please update
   * your imports, as this deprecated type alias will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated("Please use org.scalatest.check.Checkers instead.", "ScalaTest 3.1.0")
  val Checkers: org.scalatest.check.Checkers.type = org.scalatest.check.Checkers 
}


