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
package org.scalatest.examples.funsuite.sharedtests

import scala.collection.mutable.ListBuffer

class Stack[T] {

  val MAX = 10
  private val buf = new ListBuffer[T]

  def push(o: T) {
    if (!full)
      buf.prepend(o)
    else
      throw new IllegalStateException("can't push onto a full stack")
  }

  def pop(): T = {
    if (!empty)
      buf.remove(0)
    else
      throw new IllegalStateException("can't pop an empty stack")
  }

  def peek: T = {
    if (!empty)
      buf(0)
    else
      throw new IllegalStateException("can't pop an empty stack")
  }

  def full: Boolean = buf.size == MAX
  def empty: Boolean = buf.size == 0
  def size = buf.size

  override def toString = buf.mkString("Stack(", ", ", ")")
}

import org.scalatest.FunSuite

trait FunSuiteStackBehaviors { this: FunSuite =>

  def nonEmptyStack(createNonEmptyStack: => Stack[Int], lastItemAdded: Int) {

    test("empty is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
      val stack = createNonEmptyStack
      assert(!stack.empty)
    }

    test("peek is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
      val stack = createNonEmptyStack
      val size = stack.size
      assert(stack.peek === lastItemAdded)
      assert(stack.size === size)
    }

    test("pop is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
      val stack = createNonEmptyStack
      val size = stack.size
      assert(stack.pop === lastItemAdded)
      assert(stack.size === size - 1)
    }
  }

  def nonFullStack(createNonFullStack: => Stack[Int]) {

    test("full is invoked on this non-full stack: " + createNonFullStack.toString) {
      val stack = createNonFullStack
      assert(!stack.full)
    }

    test("push is invoked on this non-full stack: " + createNonFullStack.toString) {
      val stack = createNonFullStack
      val size = stack.size
      stack.push(7)
      assert(stack.size === size + 1)
      assert(stack.peek === 7)
    }
  }
}

class StackFunSuite extends FunSuite with FunSuiteStackBehaviors {

  // Stack fixture creation methods
  def emptyStack = new Stack[Int]

  def fullStack = {
    val stack = new Stack[Int]
    for (i <- 0 until stack.MAX)
      stack.push(i)
    stack
  }

  def stackWithOneItem = {
    val stack = new Stack[Int]
    stack.push(9)
    stack
  }

  def stackWithOneItemLessThanCapacity = {
    val stack = new Stack[Int]
    for (i <- 1 to 9)
      stack.push(i)
    stack
  }

  val lastValuePushed = 9

  test("empty is invoked on an empty stack") {
    val stack = emptyStack
    assert(stack.empty)
  }

  test("peek is invoked on an empty stack") {
    val stack = emptyStack
    assertThrows[IllegalStateException] {
      stack.peek
    }
  }

  test("pop is invoked on an empty stack") {
    val stack = emptyStack
    assertThrows[IllegalStateException] {
      emptyStack.pop
    }
  }

  testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
  testsFor(nonFullStack(stackWithOneItem))

  testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
  testsFor(nonFullStack(stackWithOneItemLessThanCapacity))

  test("full is invoked on a full stack") {
    val stack = fullStack
    assert(stack.full)
  }

  testsFor(nonEmptyStack(fullStack, lastValuePushed))

  test("push is invoked on a full stack") {
    val stack = fullStack
    assertThrows[IllegalStateException] {
      stack.push(10)
    }
  }
}
