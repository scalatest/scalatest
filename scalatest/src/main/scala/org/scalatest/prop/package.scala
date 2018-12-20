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
package org.scalatest

import org.scalactic.anyvals._
import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

// TODO: Get rid of the PosZInt.ensuringValid calls by chosing a PosZInt between 1 and 20 instead of an Int
package object prop {

  // The valueOf methods are called by the function generators.
  def valueOf[B](a: Any, multiplier: Int)(implicit genOfB: Generator[B]): B = {
    val seed = a.hashCode.toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfB.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[C](a: Any, b: Any, multiplier: Int)(implicit genOfC: Generator[C]): C = {
    def combinedHashCode(a: Any, b: Any): Int = 
      37 * (
        37 + a.hashCode
      ) + b.hashCode
    val seed = combinedHashCode(a, b).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfC.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[D](a: Any, b: Any, c: Any, multiplier: Int)(implicit genOfD: Generator[D]): D = {
    def combinedHashCode(a: Any, b: Any, c: Any): Int = 
      37 * (
        37 * (
          37 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    val seed = combinedHashCode(a, b, c).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfD.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[E](a: Any, b: Any, c: Any, d: Any, multiplier: Int)(implicit genOfE: Generator[E]): E = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 + a.hashCode
          ) + b.hashCode
        ) + c.hashCode
      ) + d.hashCode
    val seed = combinedHashCode(a, b, c, d).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfE.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[F](a: Any, b: Any, c: Any, d: Any, e: Any, multiplier: Int)(implicit genOfF: Generator[F]): F = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 + a.hashCode
            ) + b.hashCode
          ) + c.hashCode
        ) + d.hashCode
      ) + e.hashCode
    val seed = combinedHashCode(a, b, c, d, e).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfF.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[G](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, multiplier: Int)(implicit genOfG: Generator[G]): G = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 + a.hashCode
              ) + b.hashCode
            ) + c.hashCode
          ) + d.hashCode
        ) + e.hashCode
      ) + f.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfG.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[H](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, multiplier: Int)(implicit genOfH: Generator[H]): H = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 + a.hashCode
                ) + b.hashCode
              ) + c.hashCode
            ) + d.hashCode
          ) + e.hashCode
        ) + f.hashCode
      ) + g.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfH.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[I](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, multiplier: Int)(implicit genOfI: Generator[I]): I = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 + a.hashCode
                  ) + b.hashCode
                ) + c.hashCode
              ) + d.hashCode
            ) + e.hashCode
          ) + f.hashCode
        ) + g.hashCode
      ) + h.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfI.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[J](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, multiplier: Int)(implicit genOfJ: Generator[J]): J = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 + a.hashCode
                    ) + b.hashCode
                  ) + c.hashCode
                ) + d.hashCode
              ) + e.hashCode
            ) + f.hashCode
          ) + g.hashCode
        ) + h.hashCode
      ) + i.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfJ.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[K](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, multiplier: Int)(implicit genOfK: Generator[K]): K = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 + a.hashCode
                      ) + b.hashCode
                    ) + c.hashCode
                  ) + d.hashCode
                ) + e.hashCode
              ) + f.hashCode
            ) + g.hashCode
          ) + h.hashCode
        ) + i.hashCode
      ) + j.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfK.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[L](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, multiplier: Int)(implicit genOfL: Generator[L]): L = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 + a.hashCode
                        ) + b.hashCode
                      ) + c.hashCode
                    ) + d.hashCode
                  ) + e.hashCode
                ) + f.hashCode
              ) + g.hashCode
            ) + h.hashCode
          ) + i.hashCode
        ) + j.hashCode
      ) + k.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfL.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[M](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, multiplier: Int)(implicit genOfM: Generator[M]): M = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 + a.hashCode
                          ) + b.hashCode
                        ) + c.hashCode
                      ) + d.hashCode
                    ) + e.hashCode
                  ) + f.hashCode
                ) + g.hashCode
              ) + h.hashCode
            ) + i.hashCode
          ) + j.hashCode
        ) + k.hashCode
      ) + l.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfM.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[N](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, multiplier: Int)(implicit genOfN: Generator[N]): N = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 + a.hashCode
                            ) + b.hashCode
                          ) + c.hashCode
                        ) + d.hashCode
                      ) + e.hashCode
                    ) + f.hashCode
                  ) + g.hashCode
                ) + h.hashCode
              ) + i.hashCode
            ) + j.hashCode
          ) + k.hashCode
        ) + l.hashCode
      ) + m.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfN.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[O](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, multiplier: Int)(implicit genOfO: Generator[O]): O = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 + a.hashCode
                              ) + b.hashCode
                            ) + c.hashCode
                          ) + d.hashCode
                        ) + e.hashCode
                      ) + f.hashCode
                    ) + g.hashCode
                  ) + h.hashCode
                ) + i.hashCode
              ) + j.hashCode
            ) + k.hashCode
          ) + l.hashCode
        ) + m.hashCode
      ) + n.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfO.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[P](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, multiplier: Int)(implicit genOfP: Generator[P]): P = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 + a.hashCode
                                ) + b.hashCode
                              ) + c.hashCode
                            ) + d.hashCode
                          ) + e.hashCode
                        ) + f.hashCode
                      ) + g.hashCode
                    ) + h.hashCode
                  ) + i.hashCode
                ) + j.hashCode
              ) + k.hashCode
            ) + l.hashCode
          ) + m.hashCode
        ) + n.hashCode
      ) + o.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfP.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[Q](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, multiplier: Int)(implicit genOfQ: Generator[Q]): Q = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 + a.hashCode
                                  ) + b.hashCode
                                ) + c.hashCode
                              ) + d.hashCode
                            ) + e.hashCode
                          ) + f.hashCode
                        ) + g.hashCode
                      ) + h.hashCode
                    ) + i.hashCode
                  ) + j.hashCode
                ) + k.hashCode
              ) + l.hashCode
            ) + m.hashCode
          ) + n.hashCode
        ) + o.hashCode
      ) + p.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfQ.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[R](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, multiplier: Int)(implicit genOfR: Generator[R]): R = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 + a.hashCode
                                    ) + b.hashCode
                                  ) + c.hashCode
                                ) + d.hashCode
                              ) + e.hashCode
                            ) + f.hashCode
                          ) + g.hashCode
                        ) + h.hashCode
                      ) + i.hashCode
                    ) + j.hashCode
                  ) + k.hashCode
                ) + l.hashCode
              ) + m.hashCode
            ) + n.hashCode
          ) + o.hashCode
        ) + p.hashCode
      ) + q.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfR.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[S](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, multiplier: Int)(implicit genOfS: Generator[S]): S = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 * (
                                        37 + a.hashCode
                                      ) + b.hashCode
                                    ) + c.hashCode
                                  ) + d.hashCode
                                ) + e.hashCode
                              ) + f.hashCode
                            ) + g.hashCode
                          ) + h.hashCode
                        ) + i.hashCode
                      ) + j.hashCode
                    ) + k.hashCode
                  ) + l.hashCode
                ) + m.hashCode
              ) + n.hashCode
            ) + o.hashCode
          ) + p.hashCode
        ) + q.hashCode
      ) + r.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfS.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[T](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, multiplier: Int)(implicit genOfT: Generator[T]): T = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 * (
                                        37 * (
                                          37 + a.hashCode
                                        ) + b.hashCode
                                      ) + c.hashCode
                                    ) + d.hashCode
                                  ) + e.hashCode
                                ) + f.hashCode
                              ) + g.hashCode
                            ) + h.hashCode
                          ) + i.hashCode
                        ) + j.hashCode
                      ) + k.hashCode
                    ) + l.hashCode
                  ) + m.hashCode
                ) + n.hashCode
              ) + o.hashCode
            ) + p.hashCode
          ) + q.hashCode
        ) + r.hashCode
      ) + s.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfT.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[U](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, multiplier: Int)(implicit genOfU: Generator[U]): U = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 * (
                                        37 * (
                                          37 * (
                                            37 + a.hashCode
                                          ) + b.hashCode
                                        ) + c.hashCode
                                      ) + d.hashCode
                                    ) + e.hashCode
                                  ) + f.hashCode
                                ) + g.hashCode
                              ) + h.hashCode
                            ) + i.hashCode
                          ) + j.hashCode
                        ) + k.hashCode
                      ) + l.hashCode
                    ) + m.hashCode
                  ) + n.hashCode
                ) + o.hashCode
              ) + p.hashCode
            ) + q.hashCode
          ) + r.hashCode
        ) + s.hashCode
      ) + t.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfU.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[V](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any, multiplier: Int)(implicit genOfV: Generator[V]): V = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 * (
                                        37 * (
                                          37 * (
                                            37 * (
                                              37 + a.hashCode
                                            ) + b.hashCode
                                          ) + c.hashCode
                                        ) + d.hashCode
                                      ) + e.hashCode
                                    ) + f.hashCode
                                  ) + g.hashCode
                                ) + h.hashCode
                              ) + i.hashCode
                            ) + j.hashCode
                          ) + k.hashCode
                        ) + l.hashCode
                      ) + m.hashCode
                    ) + n.hashCode
                  ) + o.hashCode
                ) + p.hashCode
              ) + q.hashCode
            ) + r.hashCode
          ) + s.hashCode
        ) + t.hashCode
      ) + u.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfV.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }

  def valueOf[W](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any, v: Any, multiplier: Int)(implicit genOfW: Generator[W]): W = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any, v: Any): Int = 
      37 * (
        37 * (
          37 * (
            37 * (
              37 * (
                37 * (
                  37 * (
                    37 * (
                      37 * (
                        37 * (
                          37 * (
                            37 * (
                              37 * (
                                37 * (
                                  37 * (
                                    37 * (
                                      37 * (
                                        37 * (
                                          37 * (
                                            37 * (
                                              37 * (
                                                37 + a.hashCode
                                              ) + b.hashCode
                                            ) + c.hashCode
                                          ) + d.hashCode
                                        ) + e.hashCode
                                      ) + f.hashCode
                                    ) + g.hashCode
                                  ) + h.hashCode
                                ) + i.hashCode
                              ) + j.hashCode
                            ) + k.hashCode
                          ) + l.hashCode
                        ) + m.hashCode
                      ) + n.hashCode
                    ) + o.hashCode
                  ) + p.hashCode
                ) + q.hashCode
              ) + r.hashCode
            ) + s.hashCode
          ) + t.hashCode
        ) + u.hashCode
      ) + v.hashCode
    val seed = combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v).toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (result, _, _) = genOfW.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    result
  }
}
