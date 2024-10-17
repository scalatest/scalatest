/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.matchers.dsl

import scala.util.matching.Regex

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class RegexWithGroups(val regex: Regex, val groups: IndexedSeq[String]) {
  /**
   * Overrides toString to return regex and groups (if available)
   */
  override def toString: String = "\"" + regex.toString + "\"" + (
    if (groups.isEmpty)
      ""
    else 
      (if (groups.size > 1) " withGroups (" else " withGroup (") + groups.map("\"" + _ + "\"").mkString(", ") + ")"
  )
}
