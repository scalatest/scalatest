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

private[scalatest] object SuiteHelpers {
  /* This method is used in Suite.run to modify the thread name to make it easier to figure
     what test is hanging when looking at thread dumps. */
  def augmentedThreadName(currentName: String, suiteName: String): String = {
    val prefix = 
      if (currentName.indexOf("ScalaTest-") == -1) currentName + "-ScalaTest"   // "pool-96-thread-1" => "pool-96-thread-1-ScalaTest-running-<suiteName>"
      else {                                                   // "ScalaTest-3-running-OldSpec" => "ScalaTest-3-running-<suiteName>"
        val regex = """(.*?)-running-.*""".r                   // "ScalaTest-3" => "ScalaTest-3-running-<suiteName"
        val pMatcher = regex.pattern.matcher(currentName)
        val matches = pMatcher.matches()
        if (matches) pMatcher.group(1) else currentName
      }
    prefix + "-running-" + suiteName
  }
}

