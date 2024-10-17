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

import java.util.regex.Pattern

private[scalatest] case class ParsedArgs(
  // SKIP-SCALATESTJS,NATIVE-START
  runpath: List[String],
  // SKIP-SCALATESTJS,NATIVE-END
  reporters: List[String],
  suites: List[String],
  // SKIP-SCALATESTJS,NATIVE-START
  tryAgains: List[String],
  junits: List[String],
  props: List[String],
  // SKIP-SCALATESTJS,NATIVE-END
  includes: List[String],
  excludes: List[String],
  // SKIP-SCALATESTJS,NATIVE-START
  concurrent: List[String],
  // SKIP-SCALATESTJS,NATIVE-END
  membersOnly: List[String],
  wildcard: List[String],
  // SKIP-SCALATESTJS,NATIVE-START
  testNGXMLFiles: List[String],
  genSuffixesPattern: Option[Pattern],
  spanScaleFactor: List[String], 
  testSortingReporterTimeout: List[String],
  slowpokeParams: List[String],
  // SKIP-SCALATESTJS,NATIVE-END
  seeds: List[String]
)
