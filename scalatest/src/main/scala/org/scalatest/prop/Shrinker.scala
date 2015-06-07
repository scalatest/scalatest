/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals._
import scala.annotation.tailrec
import org.scalactic.Requirements._
import Stream.empty

// Wrote this class by looking at the Javadoc of java.util.Random.
// And by testing its behavior against that of java.util.Random.
// Maybe this should be a trait, so that people can, hmm. Could 
// make subclasses with extra methods, like nextSmallInt or something,
// and in a pattern match narrow the type and call that method.
private[prop] trait Shrinker[T] {
  def apply(init: T): Stream[T]
}

private[prop] object Shrinker {
  def intShrinker: Shrinker[Int] =
    new Shrinker[Int] {
      def apply(init: Int): Stream[Int] = 0 #:: 1 #:: -1 #:: empty
    }
}

