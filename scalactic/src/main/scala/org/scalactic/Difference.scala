/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalactic

trait Difference extends Serializable {

  def inlineDiff: Option[(Any, Any)]

  def sideBySideDiff: Option[(String, String)]

  def analysis: Option[String]

}

object Difference {

  val empty = new Difference {
    val inlineDiff = None

    val sideBySideDiff = None

    val analysis = None
  }

}