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

case class RoseTree[T](value: T, private val shrinker: T => List[RoseTree[T]]) {

  // Compute the shrinks list on demand using this RoseTree's value.
  // This will be called only when a property fails, and just once, and it
  // won't take long, so no need to make this a lazy val.
  def shrinks: List[RoseTree[T]] = shrinker(value)

  // This makes sense to me say Char is on the inside, then T is Char, and U is (Char, Int). So
  // for each shrunken Char, we'll get the one (Char, Int).
  def map[U](f: T => U): RoseTree[U] = {

    val u: U = f(value) // (Char, Int) the Int is constant, essentially, captured by the T => U function. The T, the Char, is what varies.

    def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f) 

    def uToListOfRoseTreeOfUFun(u: U): List[RoseTree[U]] = shrinks.map(roseTreeOfTToRoseTreeOfUFun)

    RoseTree(u, uToListOfRoseTreeOfUFun)
  }

  // So here, we need to go through each of the Ints. U here is Char? No, U is (Char, Int) again? Yes.
  // Ah, and T is Int.
  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {

    val roseTreeOfU: RoseTree[U] = f(value) // One RoseTree[(Char, Int)]

    val u: U = roseTreeOfU.value // One (Char, Int)

    val roseTreeOfTs: List[RoseTree[T]] = RoseTree(value, (o: T) => List.empty) :: shrinks // List of RoseTree[Int]
    // Can I add to this a RoseTree(value, emptyListFun)?

    // Ah, I'm not using value, which is T, except to get the roseTreeOfU oh and the List[RoseTree[T]]
    // That's the one that's missing. I need to add one more at the top, which is value (: T)...

    def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = f(roseTreeOfT.value)

    def uToListOfRoseTreeOfUFun(u: U): List[RoseTree[U]] = roseTreeOfTs.map(roseTreeOfTToRoseTreeOfUFun)

    // So yes, I get one u (one (Char, Int)), which is the root of the tree. I now need to make the
    // tree part. It should use the same Char but go through the Ints.
    RoseTree(u, uToListOfRoseTreeOfUFun)
    // CharIntPair => roseTreeOfInts.map(roseTreeOfInt => f(roseTreeOfInt.value))
    // So I think right there it should be working. But I am throwing away 
  }
}

object RoseTree {
  def emptyFun[T]: T => List[RoseTree[T]] = o => List.empty
}


/*
def unfold[a](rt: RoseTree[a], indent: String = ""): Unit =
   println(s"$indent ${rt.value}")
   val rts = rt.shrinker(rt.value)
   rts.foreach(t => unfold(t, s"$indent  "))

def xShr: Char => List[RoseTree[Char]] =
  (c: Char) => if (c > 'X') ('X' to (c - 1).toChar).toList.reverse.map(x => RoseTree(x, xShr)) else List.empty

def boolShr: Boolean => List[RoseTree[Boolean]] =
  (b: Boolean) => if b then List(RoseTree(false, unused => List.empty)) else List.empty

def intShr: Int => List[RoseTree[Int]] =
  (n: Int) => if (n > 0) (0 to n - 1).toList.reverse.map(x => RoseTree(x, intShr)) else List.empty

def charShr: Char => List[RoseTree[Char]] =
  (c: Char) => if (c > 'a') ('a' to (c - 1).toChar).toList.reverse.map(x => RoseTree(x, charShr)) else List.empty

Wow.

https://www.well-typed.com/blog/2019/05/integrated-shrinking/

(c,2)
   (c,1) // I don't get to 0 here. Oh, I get a List, so I have the other one. Need to go all the way down all List ones? Oh and later in the list is smaller? c,0 is similar to b,1. But need to get to b,0. So farther down is smaller.
     (b,1)
       (a,1)
     (a,1)
   (c,0)
     (b,0)
       (a,0)
     (a,0)

  case class Ugh[t](value: t, shrink: t => List[t]) {
    def map[u](f: t => u): Ugh[u] = Ugh(u => shrink(value).map(f))
  }


scala> for {
     |   i <- iRt
     |   c <- cRt
     | } yield (c, i)
val res3: RoseTree[(Char, Int)] = RoseTree((c,2),rs$line$10$RoseTree$$Lambda$1728/368792881@7f814c6e)

scala> unfold(res3)
 (c,2)
   (c,1)
     (b,1)
       (a,1)
     (a,1)
   (c,0)
     (b,0)
       (a,0)
     (a,0)

I'm missing (b, 2), and (a, 2). Oh, my map needs value at the beginning.
*/


