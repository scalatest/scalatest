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
package org.scalatest

trait FileMocks {

  class FileMock {
    def file: Boolean = true
    def directory: Boolean = false
    override def toString = "FileMock"
  }
  val fileMock = new FileMock

  class NotFileMock {
    def file: Boolean = false
    def directory: Boolean = true
    override def toString = "NotFileMock"
  }
  val notFileMock = new NotFileMock

  class IsFileMock {
    def isFile: Boolean = true
    def isDirectory: Boolean = false
    override def toString = "IsFileMock"
  }
  val isFileMock = new IsFileMock

  class IsNotFileMock {
    def isFile: Boolean = false
    def isDirectory: Boolean = true
    override def toString = "IsNotFileMock"
  }
  val isNotFileMock = new IsNotFileMock

  class NoPredicateMock {
    override def toString = "NoPredicateMock"
  }
  val noPredicateMock = new NoPredicateMock
}
