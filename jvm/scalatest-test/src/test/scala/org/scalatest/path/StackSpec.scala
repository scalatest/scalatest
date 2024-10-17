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
package org.scalatest.path

import scala.collection.mutable.ListBuffer
import org.scalatest.funspec.PathAnyFunSpec

class Stack[T] {

  val MAX = 10
  private val buf = new ListBuffer[T]

  def push(o: T): Unit = {
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

trait StackBehaviors { this: PathAnyFunSpec =>

  def nonEmptyStack(newStack: => Stack[Int], lastItemAdded: Int): Unit = {

    it("should be non-empty") {
      assert(!newStack.empty)
    }

    it("should return the top item on peek") {
      assert(newStack.peek === lastItemAdded)
    }

    it("should not remove the top item on peek") {
      val stack = newStack
      val size = stack.size
      assert(stack.peek === lastItemAdded)
      assert(stack.size === size)
    }

    it("should remove the top item on pop") {
      val stack = newStack
      val size = stack.size
      assert(stack.pop() === lastItemAdded)
      assert(stack.size === size - 1)
    }
  }
  
  def nonFullStack(newStack: => Stack[Int]): Unit = {

    it("should not be full") {
      assert(!newStack.full)
    }

    it("should add to the top on push") {
      val stack = newStack
      val size = stack.size
      stack.push(7)
      assert(stack.size === size + 1)
      assert(stack.peek === 7)
    }
  }
}

class StackSpec extends PathAnyFunSpec with StackBehaviors {

  //SCALATESTJS,NATIVE-ONLY override def newInstance = new StackSpec

  val lastValuePushed = 9

  describe("A Stack") {
       
    val stack = new Stack[Int]
    
    describe("(when empty)") {

      it("should be empty") {
        info("hi there from should be empty")
        assert(stack.empty)
      }

      it("should complain on peek") {
        intercept[IllegalStateException] {
          stack.peek
        }
      }

      it("should complain on pop") {
        intercept[IllegalStateException] {
          stack.pop()
        }
      }
    }
  
    describe("(with one item)") {
      
      stack.push(9)
      
      it("should be non-empty, DUDE!") {
        assert(!stack.empty)
      }

      it("should do something else") {
      }

      it should behave like nonEmptyStack(stack, lastValuePushed)
      it should behave like nonFullStack(stack)
    }

    describe("(with one item less than capacity)") {
      
      for (i <- 1 to 9)
        stack.push(i)

      it should behave like nonEmptyStack(stack, lastValuePushed)
      it should behave like nonFullStack(stack)
    }

    describe("(full)") {

      for (i <- 0 until stack.MAX)
        stack.push(i)

      it("should be full") {
        assert(stack.full)
      }

      it should behave like nonEmptyStack(stack, lastValuePushed)

      it("should complain on a push") {
        withClue("stack was: " + stack) {
          intercept[IllegalStateException] {
            stack.push(10)
          }
        }
      }
    }
  }
}

