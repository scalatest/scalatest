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
package org.scalatest.tools

import java.util.regex.Pattern

private[scalatest] case class ParsedArgs(
  runpath: List[String],
  reporters: List[String],
  suites: List[String],
  junits: List[String],
  props: List[String],
  includes: List[String],
  excludes: List[String],
  concurrent: List[String],
  membersOnly: List[String],
  wildcard: List[String],
  testNGXMLFiles: List[String],
  genSuffixesPattern: Option[Pattern], 
  chosenStyles: List[String], 
  spanScaleFactor: List[String], 
  testSortingReporterTimeout: List[String]
)
