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
package org.scalatest.tools

private[tools] case class SuiteParam(className: String, testNames: Array[String], wildcardTestNames: Array[String], nestedSuites: Array[NestedSuiteParam])
{
  private lazy val globRegex =
    className.
      replaceAll("""\.""", """\\.""").
      replaceAll("""\?""", ".").
      replaceAll("""\*""", ".*")

  //
  // A glob is a name that contains one of the following wildcard specs:
  // - '*', which matches zero or more characters
  // - '?', which matches exactly one character
  // - square brackets, which specify a set of characters to match
  //
  def isGlob: Boolean = className.matches(""".*(\[|\*|\?).*""")

  //
  // Checks className against name to see if they match.
  //
  def matches(name: String) = {
    if (!isGlob) name == className
    else         name.matches(globRegex)
  }
}