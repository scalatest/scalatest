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

package object prop {

  // Called by the general function1 generator.
  def valueOf[B](a: Any, fun: Int => Int)(implicit genOfB: Generator[B]): B = {
   val seed = (fun(a.hashCode)).toLong
   val rnd = Randomizer(seed)
   val (size, nextRnd) = rnd.chooseInt(1, 20)
   val (result, _, _) = genOfB.next(size, Nil, nextRnd)
   result
  }

  // Called by the general function2 generator.
  def valueOf[C](a: Any, b: Any, fun: Int => Int)(implicit genOfC: Generator[C]): C = {
    def combinedHashCode(a: Any, b: Any): Int =
     37 * (
       37 + a.hashCode
     ) + b.hashCode
    val seed = (fun(combinedHashCode(a, b))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfC.next(size, Nil, nextRnd)
    result
  }

  def valueOf[D](a: Any, b: Any, c: Any, fun: Int => Int)(implicit genOfD: Generator[D]): D = {
    def combinedHashCode(a: Any, b: Any, c: Any): Int =
      37 * (
        37 * (
          37 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    val seed = (fun(combinedHashCode(a, b, c))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfD.next(size, Nil, nextRnd)
    result
  }

  def valueOf[E](a: Any, b: Any, c: Any, d: Any, fun: Int => Int)(implicit genOfE: Generator[E]): E = {
    def combinedHashCode(a: Any, b: Any, c: Any, d: Any): Int =
      37 * (
        37 * (
          37 * (
            37 + a.hashCode
          ) + b.hashCode
        ) + c.hashCode
      ) + d.hashCode
    val seed = (fun(combinedHashCode(a, b, c, d))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfE.next(size, Nil, nextRnd)
    result
  }

  def valueOf[F](a: Any, b: Any, c: Any, d: Any, e: Any, fun: Int => Int)(implicit genOfF: Generator[F]): F = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfF.next(size, Nil, nextRnd)
    result
  }

  def valueOf[G](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, fun: Int => Int)(implicit genOfG: Generator[G]): G = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfG.next(size, Nil, nextRnd)
    result
  }

  def valueOf[H](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, fun: Int => Int)(implicit genOfH: Generator[H]): H = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfH.next(size, Nil, nextRnd)
    result
  }

  def valueOf[I](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, fun: Int => Int)(implicit genOfI: Generator[I]): I = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfI.next(size, Nil, nextRnd)
    result
  }

  def valueOf[J](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, fun: Int => Int)(implicit genOfJ: Generator[J]): J = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfJ.next(size, Nil, nextRnd)
    result
  }

  def valueOf[K](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, fun: Int => Int)(implicit genOfK: Generator[K]): K = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfK.next(size, Nil, nextRnd)
    result
  }

  def valueOf[L](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, fun: Int => Int)(implicit genOfL: Generator[L]): L = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfL.next(size, Nil, nextRnd)
    result
  }

  def valueOf[M](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, fun: Int => Int)(implicit genOfM: Generator[M]): M = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfM.next(size, Nil, nextRnd)
    result
  }

  def valueOf[N](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, fun: Int => Int)(implicit genOfN: Generator[N]): N = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfN.next(size, Nil, nextRnd)
    result
  }

  def valueOf[O](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, fun: Int => Int)(implicit genOfO: Generator[O]): O = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfO.next(size, Nil, nextRnd)
    result
  }

  def valueOf[P](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, fun: Int => Int)(implicit genOfP: Generator[P]): P = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfP.next(size, Nil, nextRnd)
    result
  }

  def valueOf[Q](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, fun: Int => Int)(implicit genOfQ: Generator[Q]): Q = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfQ.next(size, Nil, nextRnd)
    result
  }

  def valueOf[R](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, fun: Int => Int)(implicit genOfR: Generator[R]): R = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfR.next(size, Nil, nextRnd)
    result
  }

  def valueOf[S](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, fun: Int => Int)(implicit genOfS: Generator[S]): S = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfS.next(size, Nil, nextRnd)
    result
  }

  def valueOf[T](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, fun: Int => Int)(implicit genOfT: Generator[T]): T = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfT.next(size, Nil, nextRnd)
    result
  }

  def valueOf[U](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, fun: Int => Int)(implicit genOfU: Generator[U]): U = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfU.next(size, Nil, nextRnd)
    result
  }

  def valueOf[V](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any, fun: Int => Int)(implicit genOfV: Generator[V]): V = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfV.next(size, Nil, nextRnd)
    result
  }

  def valueOf[W](a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any, k: Any, l: Any, m: Any, n: Any, o: Any, p: Any, q: Any, r: Any, s: Any, t: Any, u: Any, v: Any, fun: Int => Int)(implicit genOfW: Generator[W]): W = {
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
    val seed = (fun(combinedHashCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))).toLong
    val rnd = Randomizer(seed)
    val (size, nextRnd) = rnd.chooseInt(1, 20)
    val (result, _, _) = genOfW.next(size, Nil, nextRnd)
    result
  }

}


