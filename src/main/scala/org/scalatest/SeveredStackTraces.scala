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

/**
 * Trait that causes <a href="exceptions/StackDepth.html"><code>StackDepth</code></a> exceptions thrown by a running test (such as <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a>s) to have
 * the exception's stack trace severed at the stack depth. Because the stack depth indicates the exact line of code that caused
 * the exception to be thrown, the severed stack trace will show that offending line of code on top. This can make the line
 * of test code that discovered a problem to be more easily found in IDEs and tools that don't make use of
 * ScalaTest's <code>StackDepth</code> exceptions directly.
 *
 * @author Bill Venners
 */
trait SeveredStackTraces extends SuiteMixin { this: Suite =>

  /**
   * Invokes <code>super.withFixture(test)</code> and transforms a thrown <code>StackDepth</code> exception by severing
   * its stack trace at the stack depth.
   */
  abstract override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case Exceptional(e: StackDepth) => Exceptional(e.severedAtStackDepth)
      case o => o
    }
  }
}
