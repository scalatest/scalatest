/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal

trait Validations {

  def withGood[A, ERR, RESULT](
    a: A Or Every[ERR]
  )(
    fn: A => RESULT
  ): RESULT Or Every[ERR] = {
    a match {
      case Good(valid) => Good(fn(valid))
      case Bad(every) => Bad(every)
    }
  }

  def withGood[A, B, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR]
  )(
    fn: (A, B) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b)(fn.curried)

  private def withGoodCurried[A, B, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR]
  )(
    fn: A => B => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (B => RESULT) Or Every[ERR] = withGood[A, ERR, B => RESULT](a)(fn)
    b match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  def withGood[A, B, C, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR]
  )(
    fn: (A, B, C) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c)(fn.curried)

  private def withGoodCurried[A, B, C, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR]
  )(
    fn: A => B => C => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (C => RESULT) Or Every[ERR] = withGoodCurried[A, B, ERR, C => RESULT](a, b)(fn)
    c match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  def withGood[A, B, C, D, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR]
  )(
    fn: (A, B, C, D) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d)(fn.curried)

  private def withGoodCurried[A, B, C, D, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR]
  )(
    fn: A => B => C => D => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (D => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, ERR, D => RESULT](a, b, c)(fn)
    d match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  def withGood[A, B, C, D, E, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR]
  )(
    fn: (A, B, C, D, E) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR]
  )(
    fn: A => B => C => D => E => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (E => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, ERR, E => RESULT](a, b, c, d)(fn)
    e match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (F => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, ERR, F => RESULT](a, b, c, d, e)(fn)
    f match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (G => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, ERR, G => RESULT](a, b, c, d, e, f)(fn)
    g match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (H => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, ERR, H => RESULT](a, b, c, d, e, f, g)(fn)
    h match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (I => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, ERR, I => RESULT](a, b, c, d, e, f, g, h)(fn)
    i match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (J => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, ERR, J => RESULT](a, b, c, d, e, f, g, h, i)(fn)
    j match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (K => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, ERR, K => RESULT](a, b, c, d, e, f, g, h, i, j)(fn)
    k match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (L => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, ERR, L => RESULT](a, b, c, d, e, f, g, h, i, j, k)(fn)
    l match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (M => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, ERR, M => RESULT](a, b, c, d, e, f, g, h, i, j, k, l)(fn)
    m match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (N => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, N => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m)(fn)
    n match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (O => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, O => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n)(fn)
    o match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (P => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, P => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)(fn)
    p match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (Q => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, Q => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)(fn)
    q match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (R => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, R => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)(fn)
    r match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (S => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, S => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)(fn)
    s match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (T => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, T => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)(fn)
    t match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => U => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (U => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, U => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(fn)
    u match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR],
    v: V Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR],
    v: V Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => U => V => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (V => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, V => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(fn)
    v match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }
}

object Validations extends Validations

