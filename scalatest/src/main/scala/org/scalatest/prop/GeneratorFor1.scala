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

private[prop] class GeneratorFor1[A, B](
  aToB: A => B,
  bToA: B => A
)(
  genOfA: Generator[A]
) extends Generator[B] { thisGenerator =>

  private val underlying: Generator[B] = {
    for {
      a <- genOfA
    } yield aToB(a)
  }

  def next(size: Int, edges: List[B], rnd: Randomizer): (B, List[B], Randomizer) = underlying.next(size, edges, rnd)
  override def initEdges(maxLength: Int, rnd: Randomizer): (List[B], Randomizer) = underlying.initEdges(maxLength, rnd)
  override def map[U](f: (B) => U): Generator[U] = underlying.map(f)
  override def flatMap[U](f: (B) => Generator[U]): Generator[U] = underlying.flatMap(f)
  override def canonicals(rnd: Randomizer): (Iterator[B], Randomizer) = underlying.canonicals(rnd) 
  override def shrink(bValue: B, rnd: Randomizer): (Iterator[B], Randomizer) = {
    val aValue = bToA(bValue)
    val (itOfA, rnd1) = genOfA.shrink(aValue, rnd)
    val streamOfA: Stream[A] = itOfA.toStream
    val streamOfB: Stream[B] = // TODO: check about the problem with streams and memory leaks, or do this a different way
      for {
        a <- streamOfA
      } yield aToB(a)
    (streamOfB.iterator, rnd1)
  }
}
