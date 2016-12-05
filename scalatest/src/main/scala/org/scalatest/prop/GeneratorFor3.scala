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

private[prop] class GeneratorFor3[A, B, C, D](
  aBCToD: (A, B, C) => D,
  dToABC: D => (A, B, C)
)(
  genOfA: Generator[A],
  genOfB: Generator[B],
  genOfC: Generator[C]
) extends Generator[D] { thisGenerator =>

  private val underlying: Generator[D] = {
    for {
      a <- genOfA
      b <- genOfB
      c <- genOfC
    } yield aBCToD(a, b, c)
  }

  def next(size: Int, edges: List[D], rnd: Randomizer): (D, List[D], Randomizer) = underlying.next(size, edges, rnd)
  override def initEdges(maxLength: Int, rnd: Randomizer): (List[D], Randomizer) = underlying.initEdges(maxLength, rnd)
  override def map[U](f: D => U): Generator[U] = underlying.map(f)
  override def flatMap[U](f: D => Generator[U]): Generator[U] = underlying.flatMap(f)
  override def canonicals(rnd: Randomizer): (Iterator[D], Randomizer) = underlying.canonicals(rnd) 
  override def shrink(dValue: D, rnd: Randomizer): (Iterator[D], Randomizer) = {
    val (aValue, bValue, cValue) = dToABC(dValue)
    val (itOfA, rnd1) = genOfA.shrink(aValue, rnd)
    val (itOfB, rnd2) = genOfB.shrink(bValue, rnd1)
    val (itOfC, rnd3) = genOfC.shrink(cValue, rnd2)
    val streamOfA: Stream[A] = itOfA.toStream
    val streamOfB: Stream[B] = itOfB.toStream
    val streamOfC: Stream[C] = itOfC.toStream
    val streamOfD: Stream[D] = // TODO: check about the problem with streams and memory leaks, or do this a different way
      for {
        a <- streamOfA
        b <- streamOfB
        c <- streamOfC
      } yield aBCToD(a, b, c)
    (streamOfD.iterator, rnd3)
  }
}
