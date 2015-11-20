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
package org.scalatest.examples.asyncfunsuite.sharedtests

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ThreadSafeStack[T] {

  final val MAX = 10
  private final val buf = new ListBuffer[T]

  def push(o: T): Unit =
    synchronized {
      if (buf.size != MAX)
        buf.prepend(o)
      else
        throw new IllegalStateException("can't push onto a full stack")
    }

  def pop(): T =
    synchronized {
      if (buf.size != 0)
        buf.remove(0)
      else
        throw new IllegalStateException("can't pop an empty stack")
    }

  def peek: T =
    synchronized {
      if (buf.size != 0)
        buf(0)
      else
        throw new IllegalStateException("can't pop an empty stack")
    }

  def full: Boolean = synchronized { buf.size == MAX }
  def empty: Boolean = synchronized { buf.size == 0 }
  def size = synchronized { buf.size }

  override def toString =
    synchronized { buf.mkString("ThreadSafeStack(", ", ", ")") }
}

import org.scalatest.AsyncFunSuite

trait FunSuiteStackBehaviors { this: AsyncFunSuite =>

  def nonEmptyStack( createNonEmptyStack: => Future[ThreadSafeStack[Int]],
      lastItemAdded: Int, name: String): Unit = {

    test("empty is invoked on this non-empty stack: " + name) {
      val futureStack = createNonEmptyStack
      futureStack map { stack => assert(!stack.empty) }
    }

    test("peek is invoked on this non-empty stack: " + name) {
      val futureStack = createNonEmptyStack
      futureStack map { stack =>
        val size = stack.size
        assert(stack.peek === lastItemAdded)
        assert(stack.size === size)
      }
    }

    test("pop is invoked on this non-empty stack: " + name) {
      val futureStack = createNonEmptyStack
      futureStack map { stack =>
        val size = stack.size
        assert(stack.pop === lastItemAdded)
        assert(stack.size === size - 1)
      }
    }
  }

  def nonFullStack(createNonFullStack: => Future[ThreadSafeStack[Int]],
      name: String): Unit = {

    test("full is invoked on this non-full stack: " + name) {
      val futureStack = createNonFullStack
      futureStack map { stack => assert(!stack.full) }
    }

    test("push is invoked on this non-full stack: " + name) {
      val futureStack = createNonFullStack
      futureStack map { stack =>
        val size = stack.size
        stack.push(7)
        assert(stack.size === size + 1)
        assert(stack.peek === 7)
      }
    }
  }
}

class StackFunSuite extends AsyncFunSuite with FunSuiteStackBehaviors {

  implicit val executionContext = ExecutionContext.Implicits.global

  // Stack fixture creation methods
  val emptyStackName = "empty stack"
  def emptyStack = Future { new ThreadSafeStack[Int] }

  val fullStackName = "full stack"
  def fullStack =
    Future {
      val stack = new ThreadSafeStack[Int]
      for (i <- 0 until stack.MAX)
        stack.push(i)
      stack
    }

  val almostEmptyStackName = "almost empty stack"
  def almostEmptyStack =
    Future {
      val stack = new ThreadSafeStack[Int]
      stack.push(9)
      stack
    }

  val almostFullStackName = "almost full stack"
  def almostFullStack =
    Future {
      val stack = new ThreadSafeStack[Int]
      for (i <- 1 to 9)
        stack.push(i)
      stack
    }

  val lastValuePushed = 9

  test("empty is invoked on an empty stack") {
    val futureStack = emptyStack
    futureStack map { stack => assert(stack.empty) }
  }

  test("peek is invoked on an empty stack") {
    val futureStack = emptyStack
    futureStack map { stack =>
      assertThrows[IllegalStateException] {
        stack.peek
      }
    }
  }

  test("pop is invoked on an empty stack") {
    val futureStack = emptyStack
    futureStack map { stack =>
      assertThrows[IllegalStateException] {
        stack.pop
      }
    }
  }

  testsFor(nonEmptyStack(almostEmptyStack, lastValuePushed,
      almostEmptyStackName))
  testsFor(nonFullStack(almostEmptyStack, almostEmptyStackName))

  testsFor(nonEmptyStack(almostFullStack, lastValuePushed,
      almostFullStackName))
  testsFor(nonFullStack(almostFullStack, almostFullStackName))

  test("full is invoked on a full stack") {
    val futureStack = fullStack
    futureStack map { stack => assert(stack.full) }
  }

  testsFor(nonEmptyStack(fullStack, lastValuePushed, fullStackName))

  test("push is invoked on a full stack") {
    val futureStack = fullStack
    futureStack map { stack =>
      assertThrows[IllegalStateException] {
        stack.push(10)
      }
    }
  }
}
