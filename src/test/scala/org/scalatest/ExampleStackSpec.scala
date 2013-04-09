package org.scalatest

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

trait ExampleStackBehaviors { this: FunSpec =>

  def nonEmptyStack(newStack: => Stack[Int], lastItemAdded: Int) {

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
      assert(stack.pop === lastItemAdded)
      assert(stack.size === size - 1)
    }
  }

  def nonFullStack(newStack: => Stack[Int]) {

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

class ExampleStackSpec extends FunSpec with ExampleStackBehaviors {

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

  describe("A Stack") {

    describe("(when empty)") {

      it("should be empty") {
        assert(emptyStack.empty)
      }

      it("should complain on peek") {
        intercept[IllegalStateException] {
          emptyStack.peek
        }
      }

      it("should complain on pop") {
        intercept[IllegalStateException] {
          emptyStack.pop
        }
      }
    }

    describe("(with one item)") {
      it should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
      it should behave like nonFullStack(stackWithOneItem)
    }

    describe("(with one item less than capacity)") {
      it should behave like nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed)
      it should behave like nonFullStack(stackWithOneItemLessThanCapacity)
    }

    describe("(full)") {

      it("should be full") {
        assert(fullStack.full)
      }

      it should behave like nonEmptyStack(fullStack, lastValuePushed)

      it("should complain on a push") {
        intercept[IllegalStateException] {
          fullStack.push(10)
        }
      }
    }
  }
}
