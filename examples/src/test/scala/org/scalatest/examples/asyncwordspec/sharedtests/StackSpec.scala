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
package org.scalatest.examples.asyncwordspec.sharedtests

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

// Stack operations
case class Push[T](value: T)
sealed abstract class StackOp
case object Pop extends StackOp
case object Peek extends StackOp
case object Size extends StackOp

// Stack info
case class StackInfo[T](top: Option[T], size: Int, max: Int) {
  require(size >= 0, "size was less than zero")
  require(max >= size, "max was less than size")
  val isFull: Boolean = size == max
  val isEmpty: Boolean = size == 0
}

class StackActor[T](Max: Int, name: String) {

  private final val buf = new ListBuffer[T]

  def !(push: Push[T]): Unit =
    synchronized {
      if (buf.size != Max)
        buf.prepend(push.value)
      else
        throw new IllegalStateException("can't push onto a full stack")
    }

  def ?(op: StackOp)(implicit c: ExecutionContext): Future[StackInfo[T]] =
    synchronized {
      op match {
        case Pop => 
          Future {
            if (buf.size != 0)
              StackInfo(Some(buf.remove(0)), buf.size, Max)
            else
              throw new IllegalStateException("can't pop an empty stack")
          }
        case Peek => 
          Future {
            if (buf.size != 0)
              StackInfo(Some(buf(0)), buf.size, Max)
            else
              throw new IllegalStateException("can't peek an empty stack")
          }
        case Size => 
          Future { StackInfo(None, buf.size, Max) }
      }
    }

  override def toString: String = name
}

import org.scalatest.AsyncWordSpec

trait AsyncWordSpecStackBehaviors { this: AsyncWordSpec =>

  def nonEmptyStackActor(createNonEmptyStackActor: => StackActor[Int],
        lastItemAdded: Int, name: String): Unit = {

    ("return non-empty StackInfo when Size is fired at non-empty stack actor: " + name) in {
      val stackActor = createNonEmptyStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(!stackInfo.isEmpty)
      }
    }

    ("return before and after StackInfo that has existing size and lastItemAdded as top when Peek is fired at non-empty stack actor: " + name) in {
      val stackActor = createNonEmptyStackActor
      val futurePair: Future[(StackInfo[Int], StackInfo[Int])] = 
        for {
          beforePeek <- stackActor ? Size
          afterPeek <- stackActor ? Peek
        } yield (beforePeek, afterPeek)
      futurePair map { case (beforePeek, afterPeek) =>
        assert(afterPeek.top == Some(lastItemAdded))
        assert(afterPeek.size == beforePeek.size)
      }
    }

    ("return before and after StackInfo that has existing size - 1 and lastItemAdded as top when Pop is fired at non-empty stack actor: " + name) in {
      val stackActor = createNonEmptyStackActor
      val futurePair: Future[(StackInfo[Int], StackInfo[Int])] = 
        for {
          beforePop <- stackActor ? Size
          afterPop <- stackActor ? Pop
        } yield (beforePop, afterPop)
      futurePair map { case (beforePop, afterPop) =>
        assert(afterPop.top == Some(lastItemAdded))
        assert(afterPop.size == beforePop.size - 1)
      }
    }
  }

  def nonFullStackActor(createNonFullStackActor: => StackActor[Int], name: String): Unit = {

    ("return non-full StackInfo when Size is fired at non-full stack actor: " + name) in {
      val stackActor = createNonFullStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(!stackInfo.isFull)
      }
    }

    ("return before and after StackInfo that has existing size + 1 and new item as top when Push is fired at non-full stack actor: " + name) in {
      val stackActor = createNonFullStackActor
      val futurePair: Future[(StackInfo[Int], StackInfo[Int])] = 
        for {
          beforePush <- stackActor ? Size
          afterPush <- { stackActor ! Push(7); stackActor ? Peek }
        } yield (beforePush, afterPush)
      futurePair map { case (beforePush, afterPush) =>
        assert(afterPush.top == Some(7))
        assert(afterPush.size == beforePush.size + 1)
      }
    }
  }
}

class StackSpec extends AsyncWordSpec with AsyncWordSpecStackBehaviors {

  val Max = 10
  val LastValuePushed = Max - 1

  // Stack fixture creation methods
  val emptyStackActorName = "empty stack actor"
  def emptyStackActor = new StackActor[Int](Max, emptyStackActorName )

  val fullStackActorName = "full stack actor"
  def fullStackActor = {
    val stackActor = new StackActor[Int](Max, fullStackActorName )
    for (i <- 0 until Max)
      stackActor ! Push(i)
    stackActor
  }

  val almostEmptyStackActorName = "almost empty stack actor"
  def almostEmptyStackActor = {
    val stackActor = new StackActor[Int](Max, almostEmptyStackActorName )
    stackActor ! Push(LastValuePushed)
    stackActor
  }

  val almostFullStackActorName = "almost full stack actor"
  def almostFullStackActor = {
    val stackActor = new StackActor[Int](Max, almostFullStackActorName)
    for (i <- 1 to LastValuePushed)
      stackActor ! Push(i)
    stackActor
  }

  "A Stack" when {
    "empty" should {
      "be empty" in {
        val stackActor = emptyStackActor
        val futureStackInfo = stackActor ? Size
        futureStackInfo map { stackInfo =>
          assert(stackInfo.isEmpty)
        }
      }

      "complain on peek" in {
        recoverToSucceededIf[IllegalStateException] {
          emptyStackActor ? Peek
        }
      }

      "complain on pop" in {
        recoverToSucceededIf[IllegalStateException] {
          emptyStackActor ? Pop
        }
      }
    }
    "it contains one item" should {
      behave like nonEmptyStackActor(almostEmptyStackActor, LastValuePushed, almostEmptyStackActorName)
      behave like nonFullStackActor(almostEmptyStackActor, almostEmptyStackActorName)
    }
    "it contains one item less than capacity" should {
      behave like nonEmptyStackActor(almostFullStackActor, LastValuePushed, almostFullStackActorName)
      behave like nonFullStackActor(almostFullStackActor, almostFullStackActorName)
    }
    "full" should {
      "be full" in {
        val stackActor = fullStackActor
        val futureStackInfo = stackActor ? Size
        futureStackInfo map { stackInfo =>
          assert(stackInfo.isFull)
        }
      }
      behave like nonEmptyStackActor(fullStackActor, LastValuePushed, fullStackActorName)
      "complain on a push" in {
        val stackActor = fullStackActor
        assertThrows[IllegalStateException] {
          stackActor ! Push(10)
        }
      }
    }
  }

}
