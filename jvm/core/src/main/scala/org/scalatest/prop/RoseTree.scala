/*
 * Copyright 2001-2025 Artima, Inc.
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

/**
 * A tree data structure in which each node contains a value of type `T` and
 * a lazy list (or stream) of `RoseTree[T]`. The values in each RoseTree[T] in the
 * lazy list represent shrunken or simplified values of type `T`.
 *
 * @tparam T the type of value contained in each node of the tree
 */
trait RoseTree[+T] { thisRoseTreeOfT =>

  /**
   * A value of type T to use in a property-based test.
   */
  val value: T

  /**
   * Lazily computed stream of child nodes (subtrees) of this node, the values of which represent
   * shrunken or simplified values of type `T` as compared to the member named `value`.
   * Each child node is a `RoseTree[T]`.
   */
  def shrinks: LazyListOrStream[RoseTree[T]]

  private lazy val maximumIterationCount = 1000000

  /**
   * Performs a search for a minimal (most shrunken or simplified) failing case.
   *
   * @param fun a function that takes a value of type `T` and returns an `Option[E]`,
   *            where the option is a `Some` that contains data (of type `E`) if the given `T` caused a failure, otherwise, 
   *            the given `T` did not cause a failure, and the returned `Option` will be a `None`
   * @tparam E the type of additional data returned in case of failure
   * @return an optional error data, if a shrunken or simplified case was found during the search
   */
  def shrinkSearch[E](fun: T => Option[E]): Option[(T, E)] = {
    @tailrec
    def shrinkLoop(lastFailure: Option[(RoseTree[T], E)], pending: LazyListOrStream[RoseTree[T]], count: Int): Option[(RoseTree[T], E)] = {
      if (count < maximumIterationCount)
        pending match {
          case head #:: tail => 
            fun(head.value) match {
              case Some(errData) =>
                // If the function failed, we replace the lastFailure with this new (more shrunken) failure value, and
                // we'll search one level deeper.
                val headChildrenRTs = head.shrinks
                shrinkLoop(Some((head, errData)), headChildrenRTs, count + 1)
              case None =>
                // The function call succeeded, let's continue to try the sibling.
                shrinkLoop(lastFailure, tail, count + 1)
            }
          case _ => // No more further siblings to try, return the last failure
            lastFailure
        }
      else 
        lastFailure
    }
    shrinkLoop(None, shrinks, 0).map { case (roseTree, errData) => (roseTree.value, errData) }
  }

  /**
   * Performs a search for a minimal (most shrunken or simplified) failing case for a `Future[T]`.
   *
   * @param fun a function that takes a value of type `T` and returns a `Future[Option[E]]`,
   *            where the `Option` is a `Some` that contains data (of type `E`) if the given `T` causes a failure, otherwise, 
   *            the given `T` does not cause a failure and the `Option` will be a `None`
   * @tparam E the type of additional data returned in case of failure
   * @return a future optional error data, if a shrunken or simplified case was found during the search
   */
  def shrinkSearchForFuture[E](fun: T => Future[Option[E]])(implicit execContext: ExecutionContext): Future[Option[(T, E)]] = {
    def shrinkLoop(lastFailure: Option[(RoseTree[T], E)], pending: LazyListOrStream[RoseTree[T]], count: Int): Future[Option[(RoseTree[T], E)]] = {
      if (count < maximumIterationCount) 
        pending match {
          case head #:: tail => 
            val future = fun(head.value)
            future.flatMap {
              case Some(errData) =>
                // If the function failed, we got a new failure value, and we'll go one level deeper.
                val headChildrenRTs = head.shrinks
                shrinkLoop(Some((head, errData)), headChildrenRTs, count + 1)
              case None =>
                // The function call succeeded, let's continue to try the sibling.
                shrinkLoop(lastFailure, tail, count + 1)
            }

          case _ =>
            Future.successful(lastFailure)
        }
      else 
        Future.successful(lastFailure)
      
    }
    shrinkLoop(None, shrinks, 0).map { opt => 
      opt.map { case (roseTree, errData) => (roseTree.value, errData) }
    }
  }

  /**
   * Maps the value of this tree node to a new value of type `U`, producing a new `RoseTree[U]`.
   *
   * @param f a function that transforms a value of type `T` to a value of type `U`
   * @tparam U the new type of value in the resulting `RoseTree`
   * @return a new `RoseTree` with the transformed value
   */
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

  /**
   * Flat maps the value of this tree node to a new `RoseTree[U]` using a function `f`, producing a new `RoseTree[U]`.
   *
   * @param f a function that transforms a value of type `T` to a `RoseTree[U]`
   * @tparam U the type of value in the resulting `RoseTree`
   * @return a new `RoseTree` with the transformed value
   */
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

  /**
   * Returns a string representation of the `RoseTree`, including its value.
   *
   * @return a string representation of the `RoseTree`
   */
  override def toString: String = s"RoseTree($value)"
}

/**
 * Companion object for the `RoseTree` trait.
 * Contains utility methods for working with `RoseTree`s.
 */
object RoseTree {

  /**
   * Combines two `RoseTree`s of types `T` and `U` into a new `RoseTree` of type `V` using a function `f`.
   *
   * @param tree1 the first `RoseTree` of type `T`
   * @param tree2 the second `RoseTree` of type `U`
   * @param f     a function that combines a value of type `T` and a value of type `U` to produce a value of type `V`
   * @tparam T the type of value in the first `RoseTree`
   * @tparam U the type of value in the second `RoseTree`
   * @tparam V the type of value in the resulting `RoseTree`
   * @return a new `RoseTree` of type `V` containing the combined values
   */
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
  val roseTrees = rt.shrinks
  roseTrees.foreach(t => unfold(t, s"$indent  "))
}

case class RoseBush[T](o: T, shr: T => List[RoseTree[T]]) extends RoseTree[T] {
  val value: T = o
  def shrinks: LazyList[RoseTree[T]] = LazyList.from(shr(o))
}

def intShr: Int => List[RoseTree[Int]] = { (n: Int) =>
  @tailrec
  def loop(n: Int, acc: List[Int]): List[Int] = {
    val half = n / 2
    if (half == 0)
      0 :: acc
    else
      loop(half, half :: acc)
  }
  val roseTrees = if (n > 0) loop(n, Nil).reverse.map(x => RoseBush(x, intShr)) else List.empty
  roseTrees
}

def charShr: Char => List[RoseTree[Char]] = { (c: Char) =>
  val roseTrees = if (c > 'A' && c <= 'Z') ('A' to (c - 1).toChar).toList.reverse.map(x => RoseBush(x, charShr)) else List.empty
  roseTrees
}

scala> for {
         c <- RoseBush('B', charShr)
         i <- RoseBush(6, intShr)
       } yield (c, i)
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


