/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalactic.anyvals._
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException
import scala.annotation.tailrec
import org.scalatest.Resources

class OrgScalaTestPropSpec extends WordSpec with Matchers {

  def samplesForGen[T](genOfT: Generator[T], desiredLength: PosInt, originalRnd: Randomizer): List[(T, Randomizer)] = {
    @tailrec
    def samplesLoop(count: Int, rnd: Randomizer, acc: List[(T, Randomizer)]): List[(T, Randomizer)] = {
      if (count == desiredLength.value) acc
      else {
        val (size, nextRnd) = rnd.chooseInt(1, 100)
        val (value, _, nextNextRnd) = genOfT.next(size, Nil, rnd)
        samplesLoop(count + 1, nextNextRnd, (value, rnd) :: acc)
      }
    }
    samplesLoop(0, originalRnd, Nil)
  }

  "The org.scalatest.prop package object" should {

    "offer a valueOf method that takes 1 input arguments" in {
      val implicitGen = implicitly[Generator[String => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val stringGen = CommonGenerators.strings

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val (s, _, nextRnd) = stringGen.next(5, List.empty, rnd)
        val result1 = fun(s)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](s, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 2 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple2Gen = CommonGenerators.tuple2s[String, Int]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((s, i), _, nextRnd) = tuple2Gen.next(5, List.empty, rnd)
        val result1 = fun(s, i)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](s, i, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 3 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple3Gen = CommonGenerators.tuple3s[String, Int, Long]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3), _, nextRnd) = tuple3Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 4 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple4Gen = CommonGenerators.tuple4s[String, Int, Long, Float]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4), _, nextRnd) = tuple4Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 5 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple5Gen = CommonGenerators.tuple5s[String, Int, Long, Float, Double]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5), _, nextRnd) = tuple5Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 6 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple6Gen = CommonGenerators.tuple6s[String, Int, Long, Float, Double, String]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6), _, nextRnd) = tuple6Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 7 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple7Gen = CommonGenerators.tuple7s[String, Int, Long, Float, Double, String, Int]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7), _, nextRnd) = tuple7Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 8 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple8Gen = CommonGenerators.tuple8s[String, Int, Long, Float, Double, String, Int, Long]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8), _, nextRnd) = tuple8Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 9 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple9Gen = CommonGenerators.tuple9s[String, Int, Long, Float, Double, String, Int, Long, Float]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9), _, nextRnd) = tuple9Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 10 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple10Gen = CommonGenerators.tuple10s[String, Int, Long, Float, Double, String, Int, Long, Float, Double]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10), _, nextRnd) = tuple10Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 11 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple11Gen = CommonGenerators.tuple11s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11), _, nextRnd) = tuple11Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, intToInt)
        result1 shouldEqual result2
      }


    }

    "offer a valueOf method that takes 12 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple12Gen = CommonGenerators.tuple12s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12), _, nextRnd) = tuple12Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 13 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple13Gen = CommonGenerators.tuple13s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13), _, nextRnd) = tuple13Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 14 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple14Gen = CommonGenerators.tuple14s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14), _, nextRnd) = tuple14Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 15 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple15Gen = CommonGenerators.tuple15s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15), _, nextRnd) = tuple15Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 16 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple16Gen = CommonGenerators.tuple16s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16), _, nextRnd) = tuple16Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 17 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple17Gen = CommonGenerators.tuple17s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17), _, nextRnd) = tuple17Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 18 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple18Gen = CommonGenerators.tuple18s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18), _, nextRnd) = tuple18Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 19 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple19Gen = CommonGenerators.tuple19s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19), _, nextRnd) = tuple19Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 20 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple20Gen = CommonGenerators.tuple20s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20), _, nextRnd) = tuple20Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 21 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple21Gen = CommonGenerators.tuple21s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val ((p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21), _, nextRnd) = tuple21Gen.next(5, List.empty, rnd)
        val result1 = fun(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, intToInt)
        result1 shouldEqual result2
      }
    }

    "offer a valueOf method that takes 22 input arguments" in {
      val implicitGen = implicitly[Generator[(String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int) => Long]]

      val rnd = Randomizer.default

      val samples = samplesForGen(implicitGen, PosInt(100), rnd)
      val tuple22Gen = CommonGenerators.tuple22s[String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int, Long, Float, Double, String, Int]

      val intToIntGen: Generator[Int => Int] = Generator.function1IntToIntGenerator

      samples.foreach { case (fun, rnd) =>
        val (t22, _, nextRnd) = tuple22Gen.next(5, List.empty, rnd)
        val result1 = fun(t22._1, t22._2, t22._3, t22._4, t22._5, t22._6, t22._7, t22._8, t22._9, t22._10, t22._11, t22._12, t22._13, t22._14, t22._15, t22._16, t22._17, t22._18, t22._19, t22._20, t22._21, t22._22)

        val (intToInt, _, _) = intToIntGen.next(10, Nil, rnd)
        val result2 = org.scalatest.prop.valueOf[Long](t22._1, t22._2, t22._3, t22._4, t22._5, t22._6, t22._7, t22._8, t22._9, t22._10, t22._11, t22._12, t22._13, t22._14, t22._15, t22._16, t22._17, t22._18, t22._19, t22._20, t22._21, t22._22, intToInt)
        result1 shouldEqual result2
      }
    }
  }

}
