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

trait FruitMocks {

  class AppleMock {
    def apple: Boolean = true
    def orange: Boolean = false
    override def toString = "AppleMock"
  }
  val appleMock = new AppleMock

  class NotAppleMock {
    def apple: Boolean = false
    def orange: Boolean = true
    override def toString = "NotAppleMock"
  }
  val notAppleMock = new NotAppleMock

  class IsAppleMock {
    def isApple: Boolean = true
    def isOrange: Boolean = false
    override def toString = "IsAppleMock"
  }
  val isAppleMock = new IsAppleMock

  class IsNotAppleMock {
    def isApple: Boolean = false
    def isOrange: Boolean = true
    override def toString = "IsNotAppleMock"
  }
  val isNotAppleMock = new IsNotAppleMock

  class NoPredicateMock {
    override def toString = "NoPredicateMock"
  }
  val noPredicateMock = new NoPredicateMock
}
