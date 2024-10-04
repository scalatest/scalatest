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
package org.scalactic.source

import org.scalatest._

class PositionSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  "source.Position's filePathname method" should "end in the simple name given in fileName" in {
    val pos = Position.here
    if (System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") != null && System.getenv("SCALACTIC_FILL_FILE_PATHNAMES") == "yes")
      pos.filePathname should endWith (pos.fileName)
    else
      pos.filePathname should equal (org.scalactic.Resources.pleaseDefineScalacticFillFilePathnameEnvVar)
  }
}
