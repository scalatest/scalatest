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
package org.scalatest.examples.asyncfeaturespec.sharedtests

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

import org.scalatest.AsyncFeatureSpec

trait AsyncFeatureSpecStackBehaviors { this: AsyncFeatureSpec =>

  def nonEmptyStackActor(createNonEmptyStackActor: => StackActor[Int],
        lastItemAdded: Int, name: String): Unit = {

    scenario("Size is fired at non-empty stack actor: " + name) {
      val stackActor = createNonEmptyStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(!stackInfo.isEmpty)
      }
    }

    scenario("Peek is fired at non-empty stack actor: " + name) {
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

    scenario("Pop is fired at non-empty stack actor: " + name) {
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

    scenario("Size is fired at non-full stack actor: " + name) {
      val stackActor = createNonFullStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(!stackInfo.isFull)
      }
    }

    scenario("Push is fired at non-full stack actor: " + name) {
      val stackActor = createNonFullStackActor
      val futurePair: Future[(StackInfo[Int], StackInfo[Int])] = 
        for {
          beforePush <- stackActor ? Size
          afterPush <- { stackActor ! Push(7); stackActor ? Peek }
        } yield (beforePush, afterPush)
      futurePair map { case (beforePush, afterPush) =>
        assert(afterPush.size == beforePush.size + 1)
        assert(afterPush.top == Some(7))
      }
    }
  }
}

class StackSpec extends AsyncFeatureSpec with AsyncFeatureSpecStackBehaviors {

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

  feature("A Stack actor") {

    scenario("Size is fired at empty stack actor") {
      val stackActor = emptyStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(stackInfo.isEmpty)
      }
    }

    scenario("Peek is fired at empty stack actor") {
      recoverToSucceededIf[IllegalStateException] {
        emptyStackActor ? Peek
      }
    }

    scenario("Pop is fired at empty stack actor") {
      recoverToSucceededIf[IllegalStateException] {
        emptyStackActor ? Pop
      }
    }

    scenariosFor(nonEmptyStackActor(almostEmptyStackActor, LastValuePushed, almostEmptyStackActorName))
    scenariosFor(nonFullStackActor(almostEmptyStackActor, almostEmptyStackActorName))

    scenariosFor(nonEmptyStackActor(almostFullStackActor, LastValuePushed, almostFullStackActorName))
    scenariosFor(nonFullStackActor(almostFullStackActor, almostFullStackActorName))

    scenario("Size is fired at full stack actor") {
      val stackActor = fullStackActor
      val futureStackInfo = stackActor ? Size
      futureStackInfo map { stackInfo =>
        assert(stackInfo.isFull)
      }
    }

    scenariosFor(nonEmptyStackActor(fullStackActor, LastValuePushed, fullStackActorName))

    scenario("Push is fired at full stack actor") {
      val stackActor = fullStackActor
      assertThrows[IllegalStateException] {
        stackActor ! Push(10)
      }
    }
  }
}
