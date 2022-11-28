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
  def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[T]], Randomizer)

  def depthFirstShrinks[E](fun: T => (Boolean, Option[E]), rnd: Randomizer): (LazyListOrStream[RoseTree[T]], Option[E], Randomizer) = {
    @tailrec
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: LazyListOrStream[RoseTree[T]], processed: Set[T] , currentRnd: Randomizer): (LazyListOrStream[RoseTree[T]], Option[E], Randomizer) = {
      pending match {
        case head #:: tail => 
          val (result, errDataOpt) = fun(head.value)
          if (!result) {
            // If the function fail, we got a new failure value, and we'll go one level deeper.
            val (headChildrenRTs, nextRnd) = head.shrinks(currentRnd)
            val newProceesed = processed + head.value
            shrinkLoop(head, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed,  nextRnd)
          }
          else {
            // The function call succeeded, let's continue to try the sibling.
            shrinkLoop(lastFailure, lastFailureData, tail, processed + head.value, currentRnd)
          }

        case _ => // No more further sibling to try, return the last failure
          (LazyListOrStream(lastFailure), lastFailureData, currentRnd)
      }
    }
    val (firstLevelShrinks, nextRnd) = shrinks(rnd)
    shrinkLoop(this, None, firstLevelShrinks, Set(value), nextRnd)
  }

  def depthFirstShrinksForFuture[E](fun: T => Future[(Boolean, Option[E])], rnd: Randomizer)(implicit execContext: ExecutionContext): Future[(LazyListOrStream[RoseTree[T]], Option[E], Randomizer)] = {
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: LazyListOrStream[RoseTree[T]], processed: Set[T] , currentRnd: Randomizer): Future[(LazyListOrStream[RoseTree[T]], Option[E], Randomizer)] = {
      pending match {
        case head #:: tail => 
          val future = fun(head.value)
          future.flatMap { case (result, errDataOpt) =>
            if (!result) {
              // If the function fail, we got a new failure value, and we'll go one level deeper.
              val (headChildrenRTs, nextRnd) = head.shrinks(currentRnd)
              val newProceesed = processed + head.value
              shrinkLoop(head, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed,  nextRnd)
            }
            else {
              // The function call succeeded, let's continue to try the sibling.
              shrinkLoop(lastFailure, lastFailureData, tail, processed + head.value, currentRnd)
            }
          }

        case _ =>
          Future.successful((LazyListOrStream(lastFailure), lastFailureData, currentRnd))
      }
    }

    val (firstLevelShrinks, nextRnd) = shrinks(rnd)
    shrinkLoop(this, None, firstLevelShrinks, Set(value), nextRnd)
  }

  def combineFirstDepthShrinks[E, U](fun: (T, U) => (Boolean, Option[E]), rnd: Randomizer, roseTreeOfU: RoseTree[U]): (LazyListOrStream[RoseTree[(T, U)]], Option[E], Randomizer) = {
    val (shrunkRtOfT, errOpt1, rnd2) = depthFirstShrinks(value => fun(value, roseTreeOfU.value), rnd)
    val bestT = shrunkRtOfT.headOption.getOrElse(this)
    val bestTValue = bestT.value
    val (shrunkRtOfU, errOpt2, rnd3) = roseTreeOfU.depthFirstShrinks(value => fun(bestTValue, value), rnd2)
    val bestU = shrunkRtOfU.headOption.getOrElse(roseTreeOfU)
    val bestUValue = bestU.value
    val errOpt = LazyListOrStream(errOpt1, errOpt2).flatten.lastOption
    (LazyListOrStream(bestT.map(t => (t, bestUValue))), errOpt, rnd3)
  }

  def combineFirstDepthShrinksForFuture[E, U](fun: (T, U) => Future[(Boolean, Option[E])], rnd: Randomizer, roseTreeOfU: RoseTree[U])(implicit execContext: ExecutionContext): Future[(LazyListOrStream[RoseTree[(T, U)]], Option[E], Randomizer)] = 
    for {
      (shrunkRtOfT, errOpt1, rnd2) <- depthFirstShrinksForFuture(value => fun(value, roseTreeOfU.value), rnd)
      bestT = shrunkRtOfT.headOption.getOrElse(this)
      bestTValue = bestT.value
      (shrunkRtOfU, errOpt2, rnd3) <- roseTreeOfU.depthFirstShrinksForFuture(value => fun(bestTValue, value), rnd2)
      bestU = shrunkRtOfU.headOption.getOrElse(roseTreeOfU)
      bestUValue = bestU.value
      errOpt = LazyListOrStream(errOpt1, errOpt2).flatten.lastOption
    } yield (LazyListOrStream(bestT.map(t => (t, bestUValue))), errOpt, rnd3)

  // This makes sense to me say Char is on the inside, then T is Char, and U is (Char, Int). So
  // for each shrunken Char, we'll get the one (Char, Int).
  def map[U](f: T => U): RoseTree[U] = {

    new RoseTree[U] {
      val value: U = f(thisRoseTreeOfT.value)
      def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[U]], Randomizer) = {
        def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f)
        val (roseTrees, rnd2) = thisRoseTreeOfT.shrinks(rnd)
        (roseTrees.map(roseTreeOfTToRoseTreeOfUFun), rnd2)
      }
    }
  }

  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {

    val roseTreeOfU: RoseTree[U] = f(thisRoseTreeOfT.value)

    new RoseTree[U] {

      val value: U = roseTreeOfU.value

      def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[U]], Randomizer) = {

        val (shrunkenRoseTreeOfUs, rnd2) = thisRoseTreeOfT.shrinks(rnd)
        val roseTreeOfUs: LazyListOrStream[RoseTree[U]] =
          for (rt <- shrunkenRoseTreeOfUs) yield
            rt.flatMap(f)

        val (sameAsBefore, rnd3) = roseTreeOfU.shrinks(rnd2)
        (roseTreeOfUs ++ sameAsBefore, rnd3)
      } 
    } 
  } 

  override def toString: String = s"RoseTree($value)"
}

object RoseTree {

  def map2[T, U, V](tree1: RoseTree[T], tree2: RoseTree[U], f: (T, U) => V, rnd: Randomizer): (RoseTree[V], Randomizer) = {
    val tupValue = f(tree1.value, tree2.value)
    val (shrinks1, rnd2) = tree1.shrinks(rnd)
    val (candidates1, rnd3) = {
      val pairs: LazyListOrStream[(RoseTree[V], Randomizer)] =
        for (candidate <- shrinks1) yield
          map2(candidate, tree2, f, rnd2)
      (pairs.map(tup => tup._1), pairs.map(tup => tup._2).lastOption.getOrElse(rnd2))
    }
      val (shrinks2, rnd4) = tree2.shrinks(rnd3)
    val (candidates2, rnd5) = {
      val pairs: LazyListOrStream[(RoseTree[V], Randomizer)] =
        for (candidate <- shrinks2) yield
          map2(tree1, candidate, f, rnd4)
      (pairs.map(tup => tup._1), pairs.map(tup => tup._2).lastOption.getOrElse(rnd4))
    }
    val roseTreeOfV =
      new RoseTree[V] {
        val value = tupValue
        def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[V]], Randomizer) = {
          (candidates1 ++ candidates2, rnd)
        }
      }
    (roseTreeOfV, rnd5)
  }

  def map2ForFuture[T, U, V](tree1: RoseTree[T], tree2: RoseTree[U], f: (T, U) => V, rnd: Randomizer)(implicit execContext: ExecutionContext): Future[(RoseTree[V], Randomizer)] = {
    val tupValue = f(tree1.value, tree2.value)
    val (shrinks1, rnd2) = tree1.shrinks(rnd)
    val (candidateFutures1, rnd3Future) = {
      val pairs: LazyListOrStream[Future[(RoseTree[V], Randomizer)]] =
        for (candidate <- shrinks1) yield
          map2ForFuture(candidate, tree2, f, rnd2)
      (pairs.map(fut => fut.map(_._1)), pairs.map(fut => fut.map(_._2)).lastOption.getOrElse(Future.successful(rnd2)))
    }
    for {
      candidates1: LazyListOrStream[RoseTree[V]] <- Future.sequence(candidateFutures1)
      rnd3 <- rnd3Future
      (shrinks2, rnd4) = tree2.shrinks(rnd3)
      (candidatesFutures2, rnd5Future) = {
        val pairs: LazyListOrStream[Future[(RoseTree[V], Randomizer)]] =
          for (candidate <- shrinks2) yield
            map2ForFuture(tree1, candidate, f, rnd4)
        (pairs.map(fut => fut.map(_._1)), pairs.map(fut => fut.map(_._2)).lastOption.getOrElse(Future.successful(rnd4)))
      }
      candidates2 <- Future.sequence(candidatesFutures2)
      rnd5 <- rnd5Future
    } yield {
      val roseTreeOfV =
        new RoseTree[V] {
          val value = tupValue
          def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[V]], Randomizer) = {
            (candidates1 #::: candidates2, rnd)
          }
        }
      (roseTreeOfV, rnd5)   
    }
  }
}

// Terminal node of a RoseTree is a Rose.
case class Rose[T](value: T) extends RoseTree[T] {
  def shrinks(rnd: Randomizer): (LazyListOrStream[RoseTree[T]], Randomizer) = (LazyListOrStream.empty, rnd)
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


