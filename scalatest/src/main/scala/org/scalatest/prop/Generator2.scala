/*
 * Copyright 2001-2015 Artima, Inc.
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

private[prop] class Generator2[A, B, C](
  abc: (A, B) => C,
  cab: C => (A, B)
)(
  genOfA: Generator[A],
  genOfB: Generator[B]
) extends Generator[C] { thisGenerator =>

  private val underlying: Generator[C] = {
    for {
      a <- genOfA
      b <- genOfB
    } yield abc(a, b)
  }

  def next(size: Int, edges: List[C], rnd: Randomizer): (C, List[C], Randomizer) = underlying.next(size, edges, rnd)
  override def initEdges(maxLength: Int, rnd: Randomizer): (List[C], Randomizer) = underlying.initEdges(maxLength, rnd)
  override def map[U](f: (C) => U): Generator[U] = underlying.map(f)
  override def flatMap[U](f: (C) => Generator[U]): Generator[U] = underlying.flatMap(f)
  override def canonicals(rnd: Randomizer): (Iterator[C], Randomizer) = underlying.canonicals(rnd) 
  override def shrink(cValue: C, rnd: Randomizer): (Iterator[C], Randomizer) = {
    val (aValue, bValue) = cab(cValue)
    val (itOfA, rnd1) = genOfA.shrink(aValue, rnd)
    val (itOfB, rnd2) = genOfB.shrink(bValue, rnd1)
    val streamOfA: Stream[A] = itOfA.toStream
    val streamOfB: Stream[B] = itOfB.toStream
    val streamOfC: Stream[C] =
      for {
        a <- streamOfA
        b <- streamOfB
      } yield abc(a, b)
    (streamOfC.iterator, rnd2)
  }
}
