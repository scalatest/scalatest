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

import com.nicta.rng.Rng
import japgolly.nyaya.test.PTest._
import org.scalatest.exceptions.DiscardedEvaluationException
import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import japgolly.nyaya.test.Executor._
import org.scalatest.Assertions
import scalaz.EphemeralStream
import japgolly.nyaya.test.Settings

trait NyayaGeneratorDrivenPropertyChecks extends /*Whenever with Configuration with*/ Assertions {

  implicit def settings = new Settings

  def prepareData[A](gen: Gen[A], sizeDist: Settings.SizeDist, genSize: GenSize, debug: Boolean): Data[A] =
    (sampleSize, seedo, debugPrefix) => {
      val samples: (SampleSize, GenSize) => Rng[EphemeralStream[A]] = (s, g) => {
        if (debug) println(s"${debugPrefix}Generating ${s.value} samples @ sz ${g.value}...")
        gen.data(g, s).map(_ take s.value)
      }
      val rng =
        if (sizeDist.isEmpty)
          samples(sampleSize, genSize)
        else {
          var total = sizeDist.foldLeft(0)(_ + _._1)
          var rem = sampleSize.value
          val plan = sizeDist.map { case (si, gg) =>
            val gs = gg.fold[GenSize](p => genSize.map(v => (v * p + 0.5).toInt max 0), identity)
            val ss = SampleSize((si.toDouble / total * rem + 0.5).toInt)
            total -= si
            rem -= ss.value
            (ss, gs)
          }
          plan.toStream
            .map(samples.tupled)
            .foldLeft(Rng insert EphemeralStream[A])((a, b) => b.flatMap(c => a.map(_ ++ c)))
        }

      seedo.fold(rng)(seed => Rng.setseed(seed).flatMap(_ => rng))
        .run
    }

  def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      Result(a, p(a))
    } catch {
      case e: Throwable => Error(a, e)
    }

  def testN[A](p: Prop[A], data: EphemeralStream[A], runInc: () => Int, S: Settings): RunState[A] = {
    val it = EphemeralStream.toIterable(data).iterator
    var rs = RunState.empty[A]
    while (rs.success && it.hasNext) {
      val a = it.next()
      rs = RunState(runInc(), test1(p, a))
    }
    rs
  }

  def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A] = {
    val data = g(S.sampleSize, S.seed, "").unsafePerformIO()
    var i = 0
    testN(p, data, () => {i+=1; i}, S)
  }

  def forAll[A](fun: (A) => Unit)(implicit genA: Gen[A], S: Settings) {
    def propF = { (a: A) =>
      val (unmetCondition, exception) =
        try {
          fun(a)
          (false, None)
        }
        catch {
          case e: DiscardedEvaluationException => (true, None)
          case e: Throwable => (false, Some(e))
        }
      !unmetCondition && exception.isEmpty
    }

    val prop = Prop.test[A]("test", propF)
    val runStatus = run(prop, prepareData(genA, S.sizeDist, S.genSize, S.debug), S) //PTest.test(prop, genA, S)
    runStatus match {
      case RunState(_, Satisfied) | RunState(_, Proved) => ()  // test passed
      case RunState(runs, Falsified(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Test failed after $runs runs, failing input value: " + e.input.show)

      case RunState(runs, Error(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Error occurred after $runs runs, error message: " + e.getMessage)
    }
  }

  def forAll[A, B](fun: (A, B) => Unit)(implicit genA: Gen[A], genB: Gen[B], S: Settings) {
    def propF = { (a: A, b: B) =>
      val (unmetCondition, exception) =
        try {
          fun(a, b)
          (false, None)
        }
        catch {
          case e: DiscardedEvaluationException => (true, None)
          case e: Throwable => (false, Some(e))
        }
      !unmetCondition && exception.isEmpty
    }

    val prop = Prop.test[(A, B)]("test", (tuple2: Tuple2[A, B]) => propF(tuple2._1, tuple2._2))
    val runStatus = run(prop, prepareData(genA *** genB, S.sizeDist, S.genSize, S.debug), S)
    runStatus match {
      case RunState(_, Satisfied) | RunState(_, Proved) => ()  // test passed
      case RunState(runs, Falsified(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Test failed after $runs runs, failing input value: " + e.input.show)

      case RunState(runs, Error(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Error occurred after $runs runs, error message: " + e.getMessage)
    }
  }

  def forAll[A, B, C](fun: (A, B, C) => Unit)(implicit genA: Gen[A], genB: Gen[B], genC: Gen[C], S: Settings) {
    def propF = { (a: A, b: B, c: C) =>
      val (unmetCondition, exception) =
        try {
          fun(a, b, c)
          (false, None)
        }
        catch {
          case e: DiscardedEvaluationException => (true, None)
          case e: Throwable => (false, Some(e))
        }
      !unmetCondition && exception.isEmpty
    }

    val prop = Prop.test[((A, B), C)]("test", (tuple2: Tuple2[(A, B), C]) => propF(tuple2._1._1, tuple2._1._2, tuple2._2))
    val runStatus = run(prop, prepareData(genA *** genB *** genC, S.sizeDist, S.genSize, S.debug), S)
    runStatus match {
      case RunState(_, Satisfied) | RunState(_, Proved) => ()  // test passed
      case RunState(runs, Falsified(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Test failed after $runs runs, failing input value: " + e.input.show)

      case RunState(runs, Error(a, e)) =>
        // TODO: should throw GeneratorDrivenPropertyCheckFailedException here
        fail(s"Error occurred after $runs runs, error message: " + e.getMessage)
    }
  }
}

object NyayaGeneratorDrivenPropertyChecks extends NyayaGeneratorDrivenPropertyChecks