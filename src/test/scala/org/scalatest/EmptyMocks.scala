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

trait EmptyMocks {

  class EmptyMock {
    def empty: Boolean = true
    def full: Boolean = false
    override def toString = "EmptyMock"
  }
  val emptyMock = new EmptyMock

  class NotEmptyMock {
    def empty: Boolean = false
    def full: Boolean = true
    override def toString = "NotEmptyMock"
  }
  val notEmptyMock = new NotEmptyMock

  class IsEmptyMock {
    def isEmpty: Boolean = true
    def isFull: Boolean = false
    override def toString = "IsEmptyMock"
  }
  val isEmptyMock = new IsEmptyMock

  class IsNotEmptyMock {
    def isEmpty: Boolean = false
    def isFull: Boolean = true
    override def toString = "IsNotEmptyMock"
  }
  val isNotEmptyMock = new IsNotEmptyMock

  class NoPredicateMock {
    override def toString = "NoPredicateMock"
  }
  val noPredicateMock = new NoPredicateMock
}
