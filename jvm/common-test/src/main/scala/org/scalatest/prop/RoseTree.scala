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

case class RoseTree[T](value: T, shrinker: T => List[RoseTree[T]]) {

  // This makes sense to me say Char is on the inside, then T is Char, and U is (Char, Int) So
  // for each shrunken Char, we'll get the one Int.
  def map[U](f: T => U): RoseTree[U] = {

    val u: U = f(value)

    def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f) 

    def uToListOfRoseTreeOfUFun(u: U): List[RoseTree[U]] = shrinker(value).map(roseTreeOfTToRoseTreeOfUFun) 

    RoseTree(u, uToListOfRoseTreeOfUFun)
  }

  // So here, we need to go through each of the Ints. U here is Char? No, U is (Char, Int) again? Yes.
  // Ah, and T is Int.
  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {
    val roseTreeOfU: RoseTree[U] = f(value) // One RoseTree[(Char, Int)]
    val u: U = roseTreeOfU.value // One (Char, Int)
    val roseTreeOfTs: List[RoseTree[T]] = shrinker(value) // List of RoseTree[Int]

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
   // if rts.isEmpty then println("EMPTY: ${rt.value}")
   rts.foreach(t => unfold(t, s"$indent  "))

def boolShr: Boolean => List[RoseTree[Boolean]] =
  (b: Boolean) => if b then List(RoseTree(false, unused => List.empty)) else List.empty

def intShr: Int => List[RoseTree[Int]] =
  (n: Int) => if (n > 0) (0 to n - 1).toList.reverse.map(x => RoseTree(x, intShr)) else List.empty

def charShr: Char => List[RoseTree[Char]] =
  (c: Char) => if (c > 'a') ('a' to (c - 1).toChar).toList.reverse.map(x => RoseTree(x, charShr)) else List.empty

Wow.

https://www.well-typed.com/blog/2019/05/integrated-shrinking/


(c,2)
   (c,1)
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

*/


