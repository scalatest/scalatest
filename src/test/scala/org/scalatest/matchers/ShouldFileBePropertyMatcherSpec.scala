/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import scala.reflect.BeanProperty

trait CustomFileBePropertyMatchers {

  class FileBePropertyMatcher extends BePropertyMatcher[java.io.File] {
    def apply(left: java.io.File) = BePropertyMatchResult(left.isFile, "file")
  }

  class DirectoryBePropertyMatcher extends BePropertyMatcher[java.io.File] {
    def apply(left: java.io.File) = BePropertyMatchResult(left.isDirectory, "directory")
  }

  val file = new FileBePropertyMatcher
  val directory = new DirectoryBePropertyMatcher
}

class ShouldFileBePropertyMatcherSpec extends Spec with ShouldMatchers with CustomFileBePropertyMatchers {
 
  object `A temp file` {
 
    def `should be a file, not a directory` {

      val tempFile = java.io.File.createTempFile("delete", "me")
 
      try {
        tempFile should be a (file)
        tempFile should not be a (directory)
      }
      finally {
        tempFile.delete()
      }
    }
  }
}
