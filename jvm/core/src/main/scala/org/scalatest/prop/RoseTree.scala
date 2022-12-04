/*
 * Copyright 2001-2020 Artima, Inc.
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

import scala.annotation.tailrec
import scala.concurrent.{Future, ExecutionContext}
import org.scalactic.ColCompatHelper.LazyListOrStream

trait RoseTree[+T] { thisRoseTreeOfT =>

  val value: T

  // Compute the shrinks list on demand using this RoseTree's value.
  // This will be called only when a property fails, and just once, and it
  // won't take long, so no need to make this a lazy val.
  def shrinks: LazyListOrStream[RoseTree[T]]

  def depthFirstShrinks[E](fun: T => (Boolean, Option[E])): (LazyListOrStream[RoseTree[T]], Option[E]) = {
    @tailrec
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: LazyListOrStream[RoseTree[T]]): (LazyListOrStream[RoseTree[T]], Option[E]) = {
      pending match {
        case head #:: tail => 
          val (result, errDataOpt) = fun(head.value)
          if (!result) {
            // If the function fail, we got a new failure value, and we'll go one level deeper.
            val headChildrenRTs = head.shrinks
            shrinkLoop(head, errDataOpt, headChildrenRTs)
          }
          else {
            // The function call succeeded, let's continue to try the sibling.
            shrinkLoop(lastFailure, lastFailureData, tail)
          }

        case _ => // No more further sibling to try, return the last failure
          (LazyListOrStream(lastFailure), lastFailureData)
      }
    }
    shrinkLoop(this, None, shrinks)
  }

  def depthFirstShrinksForFuture[E](fun: T => Future[(Boolean, Option[E])])(implicit execContext: ExecutionContext): Future[(LazyListOrStream[RoseTree[T]], Option[E])] = {
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: LazyListOrStream[RoseTree[T]], processed: Set[T]): Future[(LazyListOrStream[RoseTree[T]], Option[E])] = {
      pending match {
        case head #:: tail => 
          val future = fun(head.value)
          future.flatMap { case (result, errDataOpt) =>
            if (!result) {
              // If the function fail, we got a new failure value, and we'll go one level deeper.
              val headChildrenRTs = head.shrinks
              val newProceesed = processed + head.value
              shrinkLoop(head, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed)
            }
            else {
              // The function call succeeded, let's continue to try the sibling.
              shrinkLoop(lastFailure, lastFailureData, tail, processed + head.value)
            }
          }

        case _ =>
          Future.successful((LazyListOrStream(lastFailure), lastFailureData))
      }
    }

    shrinkLoop(this, None, shrinks, Set(value))
  }

  // This makes sense to me say Char is on the inside, then T is Char, and U is (Char, Int). So
  // for each shrunken Char, we'll get the one (Char, Int).
  def map[U](f: T => U): RoseTree[U] = {

    new RoseTree[U] {
      val value: U = f(thisRoseTreeOfT.value)
      def shrinks: LazyListOrStream[RoseTree[U]] = {
        def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f)
        val roseTrees = thisRoseTreeOfT.shrinks
        roseTrees.map(roseTreeOfTToRoseTreeOfUFun)
      }
    }
  }

  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {

    val roseTreeOfU: RoseTree[U] = f(thisRoseTreeOfT.value)

    new RoseTree[U] {

      val value: U = roseTreeOfU.value

      def shrinks: LazyListOrStream[RoseTree[U]] = {

        val shrunkenRoseTreeOfUs = thisRoseTreeOfT.shrinks
        val roseTreeOfUs: LazyListOrStream[RoseTree[U]] =
          for (rt <- shrunkenRoseTreeOfUs) yield
            rt.flatMap(f)

        val sameAsBefore = roseTreeOfU.shrinks
        roseTreeOfUs ++ sameAsBefore
      } 
    } 
  } 

  override def toString: String = s"RoseTree($value)"
}

object RoseTree {
  def map2[T, U, V](tree1: RoseTree[T], tree2: RoseTree[U])(f: (T, U) => V): RoseTree[V] = {
    val tupValue = f(tree1.value, tree2.value)
    val shrinks1 = tree1.shrinks
    val candidates1: LazyListOrStream[RoseTree[V]] =
      for (candidate <- shrinks1) yield
        map2(candidate, tree2)(f)
    val roseTreeOfV =
      new RoseTree[V] {
        val value = tupValue
        def shrinks: LazyListOrStream[RoseTree[V]] = {
          candidates1 #::: tree2.shrinks.map(candidate => map2(tree1, candidate)(f))
        }
      }
    roseTreeOfV
  }
}

// Terminal node of a RoseTree is a Rose.
case class Rose[T](value: T) extends RoseTree[T] {
  def shrinks: LazyListOrStream[RoseTree[T]] = LazyListOrStream.empty
  override def toString: String = s"Rose($value)"
}


/*
import org.scalatest.prop._

def unfold[a](rt: RoseTree[a], indent: String = ""): Unit = {
  println(s"$indent ${rt.value}")
  val (roseTrees, rnd2) = rt.shrinks(Randomizer.default)
  roseTrees.foreach(t => unfold(t, s"$indent  "))
}

case class RoseBush[T](o: T, shr: (T, Randomizer) => (List[RoseTree[T]], Randomizer)) extends RoseTree[T] {
  val value: T = o
  def shrinks(rnd: Randomizer): (List[RoseTree[T]], Randomizer) = shr(o, rnd)
}

def intShr: (Int, Randomizer) => (List[RoseTree[Int]], Randomizer) = { (n: Int, rnd: Randomizer) =>
  @tailrec
  def loop(n: Int, acc: List[Int]): List[Int] = {
    val half = n / 2
    if (half == 0)
      0 :: acc
    else
      loop(half, half :: acc)
  }
  val roseTrees = if (n > 0) loop(n, Nil).reverse.map(x => RoseBush(x, intShr)) else List.empty
  (roseTrees, rnd)
}

def charShr: (Char, Randomizer) => (List[RoseTree[Char]], Randomizer) = { (c: Char, rnd: Randomizer) =>
  val roseTrees = if (c > 'A' && c <= 'Z') ('A' to (c - 1).toChar).toList.reverse.map(x => RoseBush(x, charShr)) else List.empty
  (roseTrees, rnd)
}

scala> for {
     |   c <- RoseTree('B', charShr)
     |   i <- RoseTree(6, intShr)
     | } yield (c, i)
res5: org.scalatest.prop.RoseTree[(Char, Int)] = RoseTree((B,6),org.scalatest.prop.RoseTree$$Lambda$12440/1544455474@1a80e1d9)

scala> unfold(res5)
 (B,6)
   (A,6)
     (A,3)
       (A,1)
         (A,0)
       (A,0)
     (A,1)
       (A,0)
     (A,0)
   (B,3)
     (B,1)
       (B,0)
     (B,0)
   (B,1)
     (B,0)
   (B,0)
*/


