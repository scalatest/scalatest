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
package org.scalactic.anyvals

import org.scalactic.ColCompatHelper.Iterable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

import org.scalactic.{Every, One, Many, StringNormalizations}
import org.scalactic.UnitSpec
import org.scalactic.NormalizingEquality

import org.scalatest.CompatParColls.Converters._

class NonEmptyStringSpec extends UnitSpec {
  "A NonEmptyString" can "be constructed with one character" in {
    val onesie = NonEmptyString("3")
    onesie.length shouldBe 1
    onesie(0) shouldBe '3'

    val onesie2 = NonEmptyString('3')
    onesie2.length shouldBe 1
    onesie2(0) shouldBe '3'
  }
  it can "be constructed with many characters" in {
    val twosie = NonEmptyString("23")
    twosie.length shouldBe 2
    twosie(0) shouldBe '2'
    twosie(1) shouldBe '3'

    val twosie2 = NonEmptyString('2', '3')
    twosie2.length shouldBe 2
    twosie2(0) shouldBe '2'
    twosie2(1) shouldBe '3'

    val threesie = NonEmptyString("123")
    threesie.length shouldBe 3
    threesie(0) shouldBe '1'
    threesie(1) shouldBe '2'
    threesie(2) shouldBe '3'

    val threesie2 = NonEmptyString('1', '2', '3')
    threesie2.length shouldBe 3
    threesie2(0) shouldBe '1'
    threesie2(1) shouldBe '2'
    threesie2(2) shouldBe '3'
  }
  it can "be constructed from a Iterable via the from method on NonEmptyString singleton" in {
    NonEmptyString.from("") shouldBe None
    NonEmptyString.from("1") shouldBe Some(NonEmptyString("1"))
    NonEmptyString.from("123") shouldBe Some(NonEmptyString("123"))
  }
  it can "be deconstructed with NonEmptyString" in {
    NonEmptyString("1") match {
      case NonEmptyString(x) => x shouldEqual "1"
      case _ => fail()
    }
    NonEmptyString("hi") match {
      case NonEmptyString(hi) => hi shouldEqual "hi"
      case _ => fail()
    }
  }
  it should "have an apply method" in {
    NonEmptyString("123")(0) shouldEqual '1'
    NonEmptyString("123")(1) shouldEqual '2'
    NonEmptyString("hi")(0) shouldEqual 'h'
    NonEmptyString("789")(2) shouldEqual '9'

    // SKIP-SCALATESTJS,NATIVE-START
    val iobe = 
    the [IndexOutOfBoundsException] thrownBy { // In ScalaJs, this throws scala.scalajs.runtime.UndefinedBehaviorError
      NonEmptyString("123")(3)                 // TODO, might be nice to check for that exception on ScalaJS instead of just skipping the check
    }
    val javaVersion = System.getProperty("java.version")
    val javaMajorVersion =  (if (javaVersion.startsWith("1.")) javaVersion.drop(2) else javaVersion).takeWhile(_ != '.').toInt
    val expectedErrorMessage = if (javaMajorVersion <= 17) "String index out of range: 3" else "Index 3 out of bounds for length 3" 
    iobe should have message expectedErrorMessage
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a length method" in {
    NonEmptyString("1").length shouldBe 1
    NonEmptyString("12").length shouldBe 2
    NonEmptyString("12345").length shouldBe 5
  }
  it should "have a ++ method that takes another NonEmptyString" in {
    NonEmptyString("123") ++ NonEmptyString("4") shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ NonEmptyString("45") shouldEqual NonEmptyString("12345")
    NonEmptyString("123") ++ NonEmptyString("456") shouldEqual NonEmptyString("123456")
  }
  it should "have a ++ method that takes an Every" in {
    NonEmptyString("123") ++ One('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Every('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Every('4', '5', '6') shouldEqual NonEmptyString("123456")
    NonEmptyString("123") ++ One('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ One('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Every('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Every('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ One('4') shouldEqual NonEmptyString("1234")
  }
  it should "have a ++ method that takes a IterableOnce" in {
    NonEmptyString("123") ++ "4" shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Vector('4', '5', '6') shouldEqual NonEmptyString("123456")
    NonEmptyString("123") ++ Iterable('4') shouldEqual NonEmptyString("1234")
    NonEmptyString("123") ++ Set('4', '5') shouldEqual NonEmptyString("12345")
    NonEmptyString("123") ++ Set('4', '5').iterator shouldEqual NonEmptyString("12345")
  }
  it should "have a +: method" in {
    '0' +: NonEmptyString("1") shouldBe NonEmptyString("01")
    '0' +: NonEmptyString("12") shouldBe NonEmptyString("012")
    '0' +: NonEmptyString("onetwo") shouldBe NonEmptyString("0onetwo")
  }
  it should "implement PartialFunction[Int, Char]" in {
    val pf1: PartialFunction[Int, Char] = NonEmptyString("1")
    pf1.isDefinedAt(0) shouldBe true
    pf1.isDefinedAt(1) shouldBe false
  }
  it should "have a :+ method" in {
    NonEmptyString("1") :+ '2' shouldBe NonEmptyString("12")
    NonEmptyString("12") :+ '3' shouldBe NonEmptyString("123")
  }
  it should "have 3 addString methods" in {
    NonEmptyString("hi").addString(new StringBuilder) shouldBe new StringBuilder("hi")
    NonEmptyString("123").addString(new StringBuilder) shouldBe new StringBuilder("123")

    NonEmptyString("hi").addString(new StringBuilder, "#") shouldBe new StringBuilder("h#i")
    NonEmptyString("123").addString(new StringBuilder, "#") shouldBe new StringBuilder("1#2#3")
    NonEmptyString("123").addString(new StringBuilder, ", ") shouldBe new StringBuilder("1, 2, 3")

    NonEmptyString("hi").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<h#i>")
    NonEmptyString("123").addString(new StringBuilder, "<", "#", ">") shouldBe new StringBuilder("<1#2#3>")
    NonEmptyString("123").addString(new StringBuilder, " ( ", ", ", " ) ") shouldBe new StringBuilder(" ( 1, 2, 3 ) ")
  }
  it should "have an andThen method (inherited from PartialFunction)" in {
    val pf1 = NonEmptyString("1") andThen (_ + 1)
    pf1(0) shouldEqual 50
    val pf2 = NonEmptyString("123") andThen (_ + 1)
    pf2(0) shouldEqual 50
    pf2(1) shouldEqual 51
    pf2(2) shouldEqual 52
  }
  it should "have an applyOrElse method (inherited from PartialFunction)" in {
    NonEmptyString("123").applyOrElse(0, (_: Int) => 'a') shouldEqual '1'
    NonEmptyString("123").applyOrElse(1, (_: Int) => 'b') shouldEqual '2'
    NonEmptyString("123").applyOrElse(2, (_: Int) => 'c') shouldEqual '3'
    NonEmptyString("123").applyOrElse(3, (_: Int) => 'd') shouldEqual 'd'
    NonEmptyString("123").applyOrElse(4, (_: Int) => 'e') shouldEqual 'e'
  }
  it should "have an canEqual method" is pending
  it should "have an charAt method" in {
    val s = NonEmptyString("123")
    s.charAt(0) shouldBe '1'
    s.charAt(1) shouldBe '2'
    s.charAt(2) shouldBe '3'
  }
  // Could have an implicit conversion from Every[Char] to CharSequence like
  // there is for Seq in Predef.
  /*
  scala> Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).collect { case i if i > 10 == 0 => i / 2 }
  res1: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an collectFirst method" in {
    NonEmptyString("12345678910") collectFirst { case i if i.toString.toInt > 10 => i / 2 } shouldBe None
    NonEmptyString("123456789101112") collectFirst { case i if i.toString.toInt > 6 => i.toString.toInt / 2 } shouldBe Some(3)
  }
  /*
  scala> Vector(1).combinations(2).toVector
  res2: Vector[scala.collection.immutable.Vector[Int]] = Vector()
  */
  /*
   companion method not relevant. Has an empty and other GenTraverable stuff.
  */
  // SKIP-DOTTY-START
  it should "have an compose method, inherited from PartialFunction" in {
    val fn: Int => Char = NonEmptyString("123").compose((idx: Int) => (idx + 1).toChar)
    fn(-1) shouldBe '1'
    fn(0) shouldBe '2'
    fn(1) shouldBe '3'
    //fn(2) shouldBe '4'
  }
  // SKIP-DOTTY-END
  it should "have a contains method" in {
    val e = NonEmptyString("123")
    e.contains('5') shouldBe false
    e.contains('0') shouldBe false
    e.contains('1') shouldBe true
    e.contains('2') shouldBe true
    e.contains('3') shouldBe true
    e.contains('4') shouldBe false
  }
  // Decided to just overload one for GenSeq and one for Every. Could have done
  // what that has a Slicing nature, but that's a bit too fancy pants.
  it should "have a containsSlice method that takes GenSeq" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.containsSlice("23") shouldBe true
    nonEmptyString.containsSlice("235") shouldBe false
    nonEmptyString.containsSlice("") shouldBe true
    nonEmptyString.containsSlice(Vector('2', '3')) shouldBe true
    nonEmptyString.containsSlice(Vector('2', '3', '5')) shouldBe false
    nonEmptyString.containsSlice(Vector.empty) shouldBe true
    nonEmptyString.containsSlice(ListBuffer('2', '3')) shouldBe true
    nonEmptyString.containsSlice(ListBuffer('2', '3', '5')) shouldBe false
    nonEmptyString.containsSlice(ListBuffer.empty) shouldBe true
  }
  it should "have a containsSlice method that takes an Every" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.containsSlice(Every('2', '3')) shouldBe true
    nonEmptyString.containsSlice(Every('2', '3', '5')) shouldBe false
    nonEmptyString.containsSlice(Every('3')) shouldBe true
  }
  it should "have a containsSlice method that takes a NonEmptyString" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.containsSlice(NonEmptyString("23")) shouldBe true
    nonEmptyString.containsSlice(NonEmptyString("235")) shouldBe false
    nonEmptyString.containsSlice(NonEmptyString("3")) shouldBe true
  }
  it should "have 3 copyToArray methods" in {

    val arr1 = Array.fill(5)('a')
    NonEmptyString("12345").copyToArray(arr1)
    arr1 shouldEqual Array('1', '2', '3', '4', '5')

    val arr2 = Array.fill(5)('a')
    NonEmptyString("12345").copyToArray(arr2, 1)
    arr2 shouldEqual Array('a', '1', '2', '3', '4')

    val arr3 = Array.fill(5)('a')
    NonEmptyString("12345").copyToArray(arr3, 1, 2)
    arr3 shouldEqual Array('a', '1', '2', 'a', 'a')
  }
  it should "have a copyToBuffer method" in {

    val buf = ListBuffer.fill(3)('a')
    NonEmptyString("12345").copyToBuffer(buf)
    buf shouldEqual Buffer('a', 'a', 'a', '1', '2', '3', '4', '5')
  }
  it should "have a corresponds method that takes a GenSeq" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.corresponds(List(2, 4, 6, 8, 10))(_.toString.toInt * 2 == _) shouldBe true
    nonEmptyString.corresponds(List(2, 4, 6, 8, 11))(_.toString.toInt * 2 == _) shouldBe false
    nonEmptyString.corresponds(List(2, 4, 6, 8))(_.toString.toInt * 2 == _) shouldBe false
    nonEmptyString.corresponds(List(2, 4, 6, 8, 10, 12))(_.toString.toInt * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes an Every" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.corresponds(Many(2, 4, 6, 8, 10))(_.toString.toInt * 2 == _) shouldBe true
    nonEmptyString.corresponds(Many(2, 4, 6, 8, 11))(_.toString.toInt * 2 == _) shouldBe false
    nonEmptyString.corresponds(Many(2, 4, 6, 8))(_.toString.toInt * 2 == _) shouldBe false
    nonEmptyString.corresponds(Many(2, 4, 6, 8, 10, 12))(_.toString.toInt * 2 == _) shouldBe false
  }
  it should "have a corresponds method that takes a NonEmptyString" in {
    val nonEmptyString = NonEmptyString("1234")
    nonEmptyString.corresponds(NonEmptyString("2468"))(_.toString.toInt * 2 == _.toString.toInt) shouldBe true
    nonEmptyString.corresponds(NonEmptyString("2469"))(_.toString.toInt * 2 == _.toString.toInt) shouldBe false
    nonEmptyString.corresponds(NonEmptyString("246"))(_.toString.toInt * 2 == _.toString.toInt) shouldBe false
    nonEmptyString.corresponds(NonEmptyString("24689"))(_.toString.toInt * 2 == _.toString.toInt) shouldBe false
  }
  it should "have a count method" in {
    val nonEmptyString = NonEmptyString("12345")
    nonEmptyString.count(_.toString.toInt > 10) shouldBe 0
    nonEmptyString.count(_.toString.toInt % 2 == 0) shouldBe 2
    nonEmptyString.count(_.toString.toInt % 2 == 1) shouldBe 3
  }
  /*
  it should not have a diff method
    scala> Vector(1, 2, 3).diff(Vector(1, 2, 3))
    res0: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a distinct method" in {
    NonEmptyString("123").distinct shouldBe NonEmptyString("123")
    NonEmptyString("1").distinct shouldBe NonEmptyString("1")
    NonEmptyString("1211").distinct shouldBe NonEmptyString("12")
    NonEmptyString("111").distinct shouldBe NonEmptyString("1")
  }

  /*
  it should not have an drop method
    scala> Vector(1, 2, 3).drop(3)
    res1: scala.collection.immutable.Vector[Int] = Vector()

  it should not have an dropRight method
    scala> Vector(1, 2, 3).dropRight(3)
    res0: scala.collection.immutable.Vector[Int] = Vector()

  it should not have an dropWhile method
    scala> Vector(1, 2, 3).dropWhile(_ < 10)
    res2: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an endsWith method that takes a GenSeq" in {
    NonEmptyString("1").endsWith("1") shouldBe true
    NonEmptyString("1").endsWith("12") shouldBe false
    NonEmptyString("12").endsWith("12") shouldBe true
    NonEmptyString("12345").endsWith("12") shouldBe false
    NonEmptyString("12345").endsWith("5") shouldBe true
    NonEmptyString("12345").endsWith("345") shouldBe true
  }
  it should "have an endsWith method that takes an Every" in {
    NonEmptyString("1").endsWith(Every('1')) shouldBe true
    NonEmptyString("1").endsWith(Every('1', '2')) shouldBe false
    NonEmptyString("12").endsWith(Every('1', '2')) shouldBe true
    NonEmptyString("12345").endsWith(Every('1', '2')) shouldBe false
    NonEmptyString("12345").endsWith(Every('5')) shouldBe true
    NonEmptyString("12345").endsWith(Every('3', '4', '5')) shouldBe true
  }
  it should "have an endsWith method that takes a NonEmptyString" in {
    NonEmptyString("1").endsWith(NonEmptyString("1")) shouldBe true
    NonEmptyString("1").endsWith(NonEmptyString("12")) shouldBe false
    NonEmptyString("12").endsWith(NonEmptyString("12")) shouldBe true
    NonEmptyString("12345").endsWith(NonEmptyString("12")) shouldBe false
    NonEmptyString("12345").endsWith(NonEmptyString("5")) shouldBe true
    NonEmptyString("12345").endsWith(NonEmptyString("345")) shouldBe true
  }
  it should "have an equals method" in {
    NonEmptyString("1") shouldEqual NonEmptyString("1")
    NonEmptyString("1") should not equal NonEmptyString("2")
    NonEmptyString("12") should not equal NonEmptyString("23")
  }
  it should "have an exists method" in {
    NonEmptyString("123").exists(_ == '2') shouldBe true
    NonEmptyString("123").exists(_ == '5') shouldBe false
  }
  /*
  it should not have a filter method
    scala> Vector(1, 2, 3).filter(_ > 10)
    res12: scala.collection.immutable.Vector[Int] = Vector()

  it should not have a filterNot method
    scala> Vector(1, 2, 3).filterNot(_ < 10)
    res13: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a find method" in {
    NonEmptyString("123").find(_ == '5') shouldBe None
    NonEmptyString("123").find(_ == '2') shouldBe Some('2')
  }
  it should "have a flatMap method" in {
    NonEmptyString("123") flatMap (i => NonEmptyString(i.toString + "1")) shouldBe NonEmptyString("112131")
    val ss = NonEmptyString("hiho")
    val is = NonEmptyString("123")
    (for (s <- ss;
          i <- is) yield
      s.toString + i.toString) shouldBe NonEmptyString("h1h2h3i1i2i3h1h2h3o1o2o3")
    NonEmptyString("5") flatMap (i => NonEmptyString(i + "3")) shouldBe NonEmptyString("53")
    NonEmptyString("8") flatMap (i => NonEmptyString(i.toString)) shouldBe NonEmptyString("8")
  }
  it can "be flattened when in a IterableOnce" in {
    Vector(NonEmptyString("123"), NonEmptyString("123")).flatten shouldBe Vector('1', '2', '3', '1', '2', '3')
    List(NonEmptyString("123"), NonEmptyString("123")).flatten shouldBe List('1', '2', '3', '1', '2', '3')
    // SKIP-SCALATESTJS,NATIVE-START
    List(NonEmptyString("123"), NonEmptyString("123")).par.flatten shouldBe List('1', '2', '3', '1', '2', '3').par
    // SKIP-SCALATESTJS,NATIVE-END
  }
  it should "have a fold method" in {
    NonEmptyString("1").fold('0')((e1, e2) => (e1.toString.toInt + e2.toString.toInt).toString.charAt(0)) shouldBe '1'
    NonEmptyString("1").fold('1')((e1, e2) => (e1.toString.toInt * e2.toString.toInt).toString.charAt(0)) shouldBe '1'
    NonEmptyString("2").fold('0')((e1, e2) => (e1.toString.toInt + e2.toString.toInt).toString.charAt(0)) shouldBe '2'
    NonEmptyString("2").fold('1')((e1, e2) => (e1.toString.toInt * e2.toString.toInt).toString.charAt(0)) shouldBe '2'
    NonEmptyString("3").fold('0')((e1, e2) => (e1.toString.toInt + e2.toString.toInt).toString.charAt(0)) shouldBe '3'
    NonEmptyString("3").fold('1')((e1, e2) => (e1.toString.toInt * e2.toString.toInt).toString.charAt(0)) shouldBe '3'
    NonEmptyString("123").fold('0') ((e1, e2) => (e1.toString.toInt + e2.toString.toInt).toString.charAt(0)) shouldBe '6'
    NonEmptyString("123").fold('1')((e1, e2) => (e1.toString.toInt * e2.toString.toInt).toString.charAt(0)) shouldBe '6'
  }
  it should "have a foldLeft method" in {
    NonEmptyString("1").foldLeft("0")(_ + _) shouldBe "01"
    NonEmptyString("1").foldLeft("1")(_ + _) shouldBe "11"
    NonEmptyString("123").foldLeft("0")(_ + _) shouldBe "0123"
    NonEmptyString("123").foldLeft(1)(_ + _.toString.toInt) shouldBe 7
  }
  it should "have a foldRight method" in {
    NonEmptyString("1").foldRight("0")(_ + _.toString) shouldBe "10"
    NonEmptyString("1").foldRight("1")(_ + _.toString) shouldBe "11"
    NonEmptyString("123").foldRight("0")(_ + _.toString) shouldBe "1230"
    NonEmptyString("123").foldRight(1)(_.toString.toInt + _) shouldBe 7
  }
  it should "have a forall method" in {
    NonEmptyString("12345").forall(_.toString.toInt > 0) shouldBe true
    NonEmptyString("12345").forall(_.toString.toInt < 0) shouldBe false
  }
  it should "have a foreach method" in {
    var num = 0
    NonEmptyString("123") foreach (num += _.toString.toInt)
    num shouldBe 6
    for (i <- NonEmptyString("123"))
      num += i.toString.toInt
    num shouldBe 12
    NonEmptyString("5") foreach (num *= _.toString.toInt)
    num shouldBe 60
  }
  it should "have a groupBy method" in {
    NonEmptyString("12345").groupBy(_.toString.toInt % 2) shouldBe Map(1 -> NonEmptyString("135"), 0 -> NonEmptyString("24"))
    NonEmptyString("12333").groupBy(_.toString.toInt % 2) shouldBe Map(1 -> NonEmptyString("1333"), 0 -> NonEmptyString("2"))
    NonEmptyString("11333").groupBy(_.toString.toInt % 2) shouldBe Map(1 -> NonEmptyString("11333"))
    NonEmptyString("12357").groupBy(_.toString.toInt % 2) shouldBe Map(1 -> NonEmptyString("1357"), 0 -> NonEmptyString("2"))
  }
  it should "have a grouped method" in {
    NonEmptyString("123").grouped(2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("3"))
    NonEmptyString("123").grouped(1).toList shouldBe List(NonEmptyString("1"), NonEmptyString("2"), NonEmptyString("3"))
    an [IllegalArgumentException] should be thrownBy { NonEmptyString("123").grouped(0) }
    NonEmptyString("123456789").grouped(2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("34"), NonEmptyString("56"), NonEmptyString("78"), NonEmptyString("9"))
    NonEmptyString("123456789").grouped(3).toList shouldBe List(NonEmptyString("123"), NonEmptyString("456"), NonEmptyString("789"))
    NonEmptyString("123456789").grouped(4).toList shouldBe List(NonEmptyString("1234"), NonEmptyString("5678"), NonEmptyString("9"))
    NonEmptyString("1").grouped(2).toList shouldBe List(NonEmptyString("1"))
    NonEmptyString("1").grouped(1).toList shouldBe List(NonEmptyString("1"))
  }
  it should "have a hasDefiniteSize method" in {
    NonEmptyString("1").hasDefiniteSize shouldBe true
    NonEmptyString("12").hasDefiniteSize shouldBe true
  }
  it should "have a hashCode method" in {
    NonEmptyString("1").hashCode shouldEqual NonEmptyString("1").hashCode
    NonEmptyString("12").hashCode shouldEqual NonEmptyString("12").hashCode
  }
  it should "have a head method" in {
    NonEmptyString("hi").head shouldBe 'h'
    NonEmptyString("123").head shouldBe '1'
  }
  it should "have a headOption method" in {
    NonEmptyString("hi").headOption shouldBe Some('h')
    NonEmptyString("123").headOption shouldBe Some('1')
  }
  it should "have 2 indexOf methods" in {
    NonEmptyString("12345").indexOf('3') shouldBe 2
    NonEmptyString("12345").indexOf('1') shouldBe 0
    NonEmptyString("12345").indexOf('1', 2) shouldBe -1
    NonEmptyString("12345").indexOf('6') shouldBe -1
    NonEmptyString("12345").indexOf('5', 3) shouldBe 4

    val es = NonEmptyString("abc")
    es.indexOf('a') shouldBe 0
    es.indexOf('a', 1) shouldBe -1
    es.indexOf('A') shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOf('a') shouldBe 0
    es.indexOf('A') shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take a GenSeq" in {
    NonEmptyString("12345").indexOfSlice(List('2', '3')) shouldBe "12345".indexOfSlice(List('2', '3'))
    NonEmptyString("12345").indexOfSlice(List('2', '3'), 3) shouldBe "12345".indexOfSlice(List('2', '3'), 3)
    NonEmptyString("12345").indexOfSlice(List('2', '3', '5'), 3) shouldBe "12345".indexOfSlice(List('2', '3', '5'), 3)
    NonEmptyString("12345").indexOfSlice(List('2', '3', '5')) shouldBe "12345".indexOfSlice(List('2', '3', '5'))
    NonEmptyString("12345").indexOfSlice(List('5')) shouldBe "12345".indexOfSlice(List('5'))
    NonEmptyString("12345").indexOfSlice(List('1', '2', '3', '4', '5')) shouldBe "12345".indexOfSlice(List('1', '2', '3', '4', '5'))
    NonEmptyString("12345").indexOfSlice(List('1', '2', '3', '4', '5'), 0) shouldBe "12345".indexOfSlice(List('1', '2', '3', '4', '5'), 0)
    NonEmptyString("12345").indexOfSlice(List('1', '2', '3', '4', '5'), 1) shouldBe "12345".indexOfSlice(List('1', '2', '3', '4', '5'), 1)
    NonEmptyString("12345").indexOfSlice(List('1', '2', '3', '4', '5'), -1) shouldBe "12345".indexOfSlice(List('1', '2', '3', '4', '5'), -1)
    NonEmptyString("12345").indexOfSlice(List.empty) shouldBe "12345".indexOfSlice(List.empty)
    NonEmptyString("12345").indexOfSlice(List.empty, 6) shouldBe "12345".indexOfSlice(List.empty, 6)
    NonEmptyString("12345").indexOfSlice(List.empty, 4) shouldBe "12345".indexOfSlice(List.empty, 4)

    val es = NonEmptyString("abcde")
    val s = "abcde"
    es.indexOfSlice(List('a', 'b')) shouldBe s.indexOfSlice(List('a', 'b'))
    es.indexOfSlice(List('a', 'b'), 1) shouldBe s.indexOfSlice(List('a', 'b'), 1)
    es.indexOfSlice(List('A', 'B')) shouldBe s.indexOfSlice(List('A', 'B'))
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(List('a', 'b')) shouldBe 0
    es.indexOfSlice(List('A', 'B')) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take an Every" in {
    NonEmptyString("12345").indexOfSlice(Every('2', '3')) shouldBe 1
    NonEmptyString("12345").indexOfSlice(Every('2', '3'), 3) shouldBe -1
    NonEmptyString("12345").indexOfSlice(Every('2', '3', '5'), 3) shouldBe -1
    NonEmptyString("12345").indexOfSlice(Every('2', '3', '5')) shouldBe -1
    NonEmptyString("12345").indexOfSlice(Every('5')) shouldBe 4
    NonEmptyString("12345").indexOfSlice(Every('1', '2', '3', '4', '5')) shouldBe 0
    NonEmptyString("12345").indexOfSlice(Every('1', '2', '3', '4', '5'), 0) shouldBe 0
    NonEmptyString("12345").indexOfSlice(Every('1', '2', '3', '4', '5'), 1) shouldBe -1
    NonEmptyString("12345").indexOfSlice(Every('1', '2', '3', '4', '5'), -1) shouldBe 0

    val es = NonEmptyString("abcde")
    es.indexOfSlice(Every('a', 'b')) shouldBe 0
    es.indexOfSlice(Every('a', 'b'), 1) shouldBe -1
    es.indexOfSlice(Every('A', 'B')) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(Every('a', 'b')) shouldBe 0
    es.indexOfSlice(Every('A', 'B')) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexOfSlice methods that take a NonEmptyString" in {
    NonEmptyString("12345").indexOfSlice(NonEmptyString("23")) shouldBe 1
    NonEmptyString("12345").indexOfSlice(NonEmptyString("23"), 3) shouldBe -1
    NonEmptyString("12345").indexOfSlice(NonEmptyString("235"), 3) shouldBe -1
    NonEmptyString("12345").indexOfSlice(NonEmptyString("235")) shouldBe -1
    NonEmptyString("12345").indexOfSlice(NonEmptyString("5")) shouldBe 4
    NonEmptyString("12345").indexOfSlice(NonEmptyString("12345")) shouldBe 0
    NonEmptyString("12345").indexOfSlice(NonEmptyString("12345"), 0) shouldBe 0
    NonEmptyString("12345").indexOfSlice(NonEmptyString("12345"), 1) shouldBe -1
    NonEmptyString("12345").indexOfSlice(NonEmptyString("12345"), -1) shouldBe 0

    val es = NonEmptyString("abcde")
    es.indexOfSlice(NonEmptyString("ab")) shouldBe 0
    es.indexOfSlice(NonEmptyString("ab"), 1) shouldBe -1
    es.indexOfSlice(NonEmptyString("AB")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.indexOfSlice(NonEmptyString("ab")) shouldBe 0
    es.indexOfSlice(NonEmptyString("AB")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 indexWhere methods" in {
    NonEmptyString("12345").indexWhere(_ == '3') shouldBe 2
    NonEmptyString("12345").indexWhere(_ == '1') shouldBe 0
    NonEmptyString("12345").indexWhere(_ == '6') shouldBe -1
    NonEmptyString("12345").indexWhere(_ == '5') shouldBe 4
  }
  it should "have an indices method" in {
    NonEmptyString("1").indices shouldBe List(1).indices
    NonEmptyString("123").indices shouldBe (0 to 2)
    NonEmptyString("12345").indices shouldBe (0 to 4)
  }
  /*
  it should not have an init method
    scala> Vector(1).init
    res30: scala.collection.immutable.Vector[Int] = Vector()

  it should "have an inits method" is pending
    scala> Vector(1).inits.toString
    res32: String[scala.collection.immutable.Vector[Int]] = String(Vector(1), Vector())

  it should "have an intersect method" is pending
    scala> Vector(1, 2, 3) intersect Vector(4, 5)
    res33: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have an isDefinedAt method, inherited from PartialFunction" in {
    NonEmptyString("1").isDefinedAt(0) shouldBe true
    NonEmptyString("1").isDefinedAt(1) shouldBe false
    NonEmptyString("123").isDefinedAt(1) shouldBe true
    NonEmptyString("123").isDefinedAt(2) shouldBe true
    NonEmptyString("123").isDefinedAt(3) shouldBe false
    NonEmptyString("123").isDefinedAt(0) shouldBe true
    NonEmptyString("123").isDefinedAt(-1) shouldBe false
  }
  it should "have an isEmpty method" in {
    NonEmptyString("hi").isEmpty shouldBe false
    NonEmptyString("123").isEmpty shouldBe false
  }
  it should "have an isTraversableAgain method" in {
    NonEmptyString("hi").isTraversableAgain shouldBe true
    NonEmptyString("123").isTraversableAgain shouldBe true
  }
  it should "have an iterator method" in {
    NonEmptyString("hi").iterator.toList shouldBe List('h', 'i')
    NonEmptyString("123").iterator.toList shouldBe List('1', '2', '3')
  }
  it should "have a last method" in {
    NonEmptyString("hi").last shouldBe 'i'
    NonEmptyString("123").last shouldBe '3'
  }
  it should "have 2 lastIndexOf methods" in {
    NonEmptyString("12345").lastIndexOf('2') shouldBe 1
    NonEmptyString("123451").lastIndexOf('1') shouldBe 5
    NonEmptyString("12345").lastIndexOf('0') shouldBe -1
    NonEmptyString("12345").lastIndexOf('5') shouldBe 4
    NonEmptyString("12345").lastIndexOf('3') shouldBe 2
    NonEmptyString("1").lastIndexOf('1') shouldBe 0
    NonEmptyString("12345").lastIndexOf('2', 3) shouldBe 1
    NonEmptyString("12345").lastIndexOf('2', 0) shouldBe -1
    NonEmptyString("12345").lastIndexOf('2', 1) shouldBe 1

    val es = NonEmptyString("abc")
    es.lastIndexOf('a') shouldBe 0
    es.lastIndexOf('b') shouldBe 1
    es.lastIndexOf('c') shouldBe 2
    es.lastIndexOf('c', 1) shouldBe -1
    es.lastIndexOf('A') shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOf('a') shouldBe 0
    es.lastIndexOf('A') shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take a GenSeq" in {
    NonEmptyString("12345").lastIndexOfSlice(List('2', '3')) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(List('2', '3'), 3) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(List('2', '3', '5'), 3) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(List('2', '3', '5')) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(List('5')) shouldBe 4
    NonEmptyString("12345").lastIndexOfSlice(List('1', '2', '3', '4', '5')) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(List('1', '2', '3', '4', '5'), 0) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(List('1', '2', '3', '4', '5'), 1) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(List('1', '2', '3', '4', '5'), -1) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(List.empty) shouldBe 5
    NonEmptyString("12345").lastIndexOfSlice(List.empty, 6) shouldBe 5
    NonEmptyString("12345").lastIndexOfSlice(List.empty, 4) shouldBe 4

    val es = NonEmptyString("abcde")
    es.lastIndexOfSlice(List('a', 'b')) shouldBe 0
    es.lastIndexOfSlice(List('b', 'c'), 0) shouldBe -1
    es.lastIndexOfSlice(List('A', 'B')) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(List('a', 'b')) shouldBe 0
    es.lastIndexOfSlice(List('A', 'B')) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take an Every" in {
    NonEmptyString("12345").lastIndexOfSlice(Every('2', '3')) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(Every('2', '3'), 3) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(Every('2', '3', '5'), 3) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(Every('2', '3', '5')) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(Every('5')) shouldBe 4
    NonEmptyString("12345").lastIndexOfSlice(Every('1', '2', '3', '4', '5')) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(Every('1', '2', '3', '4', '5'), 0) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(Every('1', '2', '3', '4', '5'), 1) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(Every('1', '2', '3', '4', '5'), -1) shouldBe -1

    val es = NonEmptyString("abcde")
    es.lastIndexOfSlice(Every('a', 'b')) shouldBe 0
    es.lastIndexOfSlice(Every('b', 'c'), 0) shouldBe -1
    es.lastIndexOfSlice(Every('A', 'B')) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(Every('a', 'b')) shouldBe 0
    es.lastIndexOfSlice(Every('A', 'B')) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexOfSlice methods that take a NonEmptyString" in {
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("23")) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("23"), 3) shouldBe 1
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("235"), 3) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("235")) shouldBe -1
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("5")) shouldBe 4
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("12345")) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("12345"), 0) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("12345"), 1) shouldBe 0
    NonEmptyString("12345").lastIndexOfSlice(NonEmptyString("12345"), -1) shouldBe -1

    val es = NonEmptyString("abcde")
    es.lastIndexOfSlice(NonEmptyString("ab")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyString("bc"), 0) shouldBe -1
    es.lastIndexOfSlice(NonEmptyString("AB")) shouldBe -1
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6114
    implicit val strEq = StringNormalizations.lowerCased.toEquality
    //DOTTY-ONLY implicit val strEq: NormalizingEquality[String] = StringNormalizations.lowerCased.toEquality
    es.lastIndexOfSlice(NonEmptyString("ab")) shouldBe 0
    es.lastIndexOfSlice(NonEmptyString("AB")) shouldBe -1
    // SKIP-DOTTY-END
  }
  it should "have 2 lastIndexWhere methods" in {
    NonEmptyString("12345").lastIndexWhere(_ == '2') shouldBe 1
    NonEmptyString("12345").lastIndexWhere(_ == '0') shouldBe -1
    NonEmptyString("12345").lastIndexWhere(_ == '5') shouldBe 4
    NonEmptyString("12345").lastIndexWhere(_ == '3') shouldBe 2
    NonEmptyString("1").lastIndexWhere(_ == '1') shouldBe 0
    NonEmptyString("12345").lastIndexWhere(_ == '2', 3) shouldBe 1
    NonEmptyString("12345").lastIndexWhere(_ == '2', 0) shouldBe -1
    NonEmptyString("12345").lastIndexWhere(_ == '2', 1) shouldBe 1
  }
  it should "have an lastOption method" in {
    NonEmptyString("hi").lastOption shouldBe Some('i')
    NonEmptyString("123").lastOption shouldBe Some('3')
  }
  it should "have an lengthCompare method" in {
    NonEmptyString("hi").lengthCompare(0) should be > 0
    NonEmptyString("hi").lengthCompare(1) should be > 0
    NonEmptyString("hi").lengthCompare(2) shouldEqual 0
    NonEmptyString("hi").lengthCompare(3) should be < 0
    NonEmptyString("123").lengthCompare(0) should be > 0
    NonEmptyString("123").lengthCompare(1) should be > 0
    NonEmptyString("123").lengthCompare(2) should be > 0
    NonEmptyString("123").lengthCompare(3) shouldEqual 0
    NonEmptyString("123").lengthCompare(4) should be < 0
  }
  it should "have an inherited lift method" in {
    val liftedHi = NonEmptyString("hi").lift
    liftedHi(0) shouldBe Some('h')
    liftedHi(1) shouldBe Some('i')
    liftedHi(2) shouldBe None
    liftedHi(-1) shouldBe None
    val liftedMany = NonEmptyString("123").lift
    liftedMany(0) shouldBe Some('1')
    liftedMany(1) shouldBe Some('2')
    liftedMany(2) shouldBe Some('3')
    liftedMany(3) shouldBe None
    liftedMany(-1) shouldBe None
  }
  it should "have a map method" in {
    NonEmptyString("123") map ((e: Char) => (e + 1).toChar) shouldBe NonEmptyString("234")
    (for (ele <- NonEmptyString("123")) yield (ele.toString.toInt * 2).toString.charAt(0)) shouldBe NonEmptyString("246")
    NonEmptyString("5") map ((e: Char) => (e + 3).toChar) shouldBe NonEmptyString("8")
  }
  it should "have a max method" in {
    NonEmptyString("12345").max shouldBe '5'
    NonEmptyString("1").max shouldBe '1'
    NonEmptyString("aaacccbbb").max shouldBe 'c'
  }
  it should "have a maxBy method" in {
    NonEmptyString("12345").maxBy(e => if (e == '3') 1 else 0) shouldBe '3'
  }
  it should "have a min method" in {
    NonEmptyString("12345").min shouldBe '1'
    NonEmptyString("1").min shouldBe '1'
    NonEmptyString("aaacccbbb").min shouldBe 'a'
  }
  it should "have a minBy method" in {
    NonEmptyString("12345").minBy(e => if (e == '3') 0 else 1) shouldBe '3'
  }
  it should "have a mkString method" in {
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    NonEmptyString("hi").mkString shouldBe "hi"
    NonEmptyString("123").mkString shouldBe "123"
    // SKIP-DOTTY-END

    NonEmptyString("hi").mkString("#") shouldBe "h#i"
    NonEmptyString("123").mkString("#") shouldBe "1#2#3"
    NonEmptyString("123").mkString(", ") shouldBe "1, 2, 3"

    NonEmptyString("hi").mkString("<", "#", ">") shouldBe "<h#i>"
    NonEmptyString("123").mkString("<", "#", ">") shouldBe "<1#2#3>"
    NonEmptyString("123").mkString(" ( ", ", ", " ) ") shouldBe " ( 1, 2, 3 ) "
  }
  it should "have an nonEmpty method" in {
    NonEmptyString("hi").nonEmpty shouldBe true
  }
  /*it should "have an orElse method, inherited from PartialFunction" in {
    val pf: PartialFunction[Int, Int] = { case i => -i }
    val f = NonEmptyString(1, 2, 3) orElse pf
    f(0) shouldBe 1
    f(1) shouldBe 2
    f(2) shouldBe 3
    f(3) shouldBe -3
    f(-1) shouldBe 1
  }*/
  it should "have a padTo method" in {
    NonEmptyString("1").padTo(0, 'x') shouldBe NonEmptyString("1")
    NonEmptyString("1").padTo(1, 'x') shouldBe NonEmptyString("1")
    NonEmptyString("1").padTo(2, 'x') shouldBe NonEmptyString("1x")
    NonEmptyString("1").padTo(3, 'x') shouldBe NonEmptyString("1xx")
    NonEmptyString("123").padTo(3, 'x') shouldBe NonEmptyString("123")
    NonEmptyString("123").padTo(4, 'x') shouldBe NonEmptyString("123x")
    NonEmptyString("123").padTo(5, 'x') shouldBe NonEmptyString("123xx")
  }
  // it should not have a par method, because I don't want to support that. If the user
  // needs a parallel collection, they can use a parallel collection: nonEmptyString.toVector.par...
  /*
  it should not have an partition method
    scala> Vector(1, 2, 3, 4, 5).partition(_ > 10)
    res10: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have a patch method" in {
    NonEmptyString("12345").patch(2, NonEmptyString("ab"), 2) shouldBe NonEmptyString("12ab5")
    NonEmptyString("12345").patch(2, NonEmptyString("ab"), 5) shouldBe NonEmptyString("12ab")
    NonEmptyString("12345").patch(2, NonEmptyString("ab"), 1) shouldBe NonEmptyString("12ab45")
    NonEmptyString("12345").patch(4, NonEmptyString("ab"), 2) shouldBe NonEmptyString("1234ab")
    NonEmptyString("12345").patch(5, NonEmptyString("ab"), 2) shouldBe NonEmptyString("12345ab")
    NonEmptyString("12345").patch(6, NonEmptyString("ab"), 2) shouldBe NonEmptyString("12345ab")
    NonEmptyString("12345").patch(0, NonEmptyString("ab"), 2) shouldBe NonEmptyString("ab345")
    NonEmptyString("12345").patch(0, NonEmptyString("ab"), 3) shouldBe NonEmptyString("ab45")
  }
  it should "have a permutations method" in {
    NonEmptyString("123").permutations.toStream shouldBe Stream(NonEmptyString("123"), NonEmptyString("132"), NonEmptyString("213"), NonEmptyString("231"), NonEmptyString("312"), NonEmptyString("321"))
    NonEmptyString("1").permutations.toStream shouldBe Stream(NonEmptyString("1"))
    NonEmptyString("12").permutations.toStream shouldBe Stream(NonEmptyString("12"), NonEmptyString("21"))
  }
  it should "have a prefixLength method" in {
    NonEmptyString("12345").prefixLength(_ == '1') shouldBe 1
    NonEmptyString("12345").prefixLength(_ == '2') shouldBe 0
    NonEmptyString("12345").prefixLength(_.toString.toInt <= 2) shouldBe 2
    NonEmptyString("12345").prefixLength(_.toString.toInt <= 10) shouldBe 5
    NonEmptyString("12345").prefixLength(_.toString.toInt <= 4) shouldBe 4
  }
  it should "have a product method" in {
    NonEmptyString("123").product.toInt shouldBe 59414
    NonEmptyString("3").product.toInt shouldBe 51
    NonEmptyString("345").product.toInt shouldBe 9484
    NonEmptyString("354").product.toInt shouldBe 9484
  }
  it should "have a reduce method" in {
    NonEmptyString("12345").reduce((c1, c2) => (c1 + c2).toChar).toInt shouldBe 255
    NonEmptyString("12345").reduce((c1, c2) => (c1 * c2).toChar).toInt shouldBe 36056
    NonEmptyString("5").reduce((c1, c2) => (c1 + c2).toChar).toInt shouldBe 53
    NonEmptyString("5").reduce((c1, c2) => (c1 * c2).toChar).toInt shouldBe 53
  }
  it should "have a reduceLeft method" in {
    NonEmptyString("1").reduceLeft((c1, c2) => (c1 + c2).toChar).toInt shouldBe 49
    NonEmptyString("1").reduceLeft((c1, c2) => (c1 * c2).toChar).toInt shouldBe 49
    NonEmptyString("123").reduceLeft((c1, c2) => (c1 + c2).toChar).toInt shouldBe 150
    NonEmptyString("123").reduceLeft((c1, c2) => (c1 * c2).toChar).toInt shouldBe 59414
    NonEmptyString("12345").reduceLeft((c1, c2) => (c1 * c2).toChar).toInt shouldBe 36056
  }
  it should "have a reduceLeftOption method" in {
    NonEmptyString("1").reduceLeftOption((c1, c2) => (c1 + c2).toChar).map(_.toInt) shouldBe Some(49)
    NonEmptyString("123").reduceLeftOption((c1, c2) => (c1 + c2).toChar).map(_.toInt) shouldBe Some(150)
    NonEmptyString("123").reduceLeftOption((c1, c2) => (c1 * c2).toChar).map(_.toInt) shouldBe Some(59414)
    NonEmptyString("12345").reduceLeftOption((c1, c2) => (c1 * c2).toChar).map(_.toInt) shouldBe Some(36056)
  }
  it should "have a reduceOption method" in {
    NonEmptyString("12345").reduceOption((c1, c2) => (c1 + c2).toChar).map(_.toInt) shouldBe Some(255)
    NonEmptyString("12345").reduceOption((c1, c2) => (c1 * c2).toChar).map(_.toInt) shouldBe Some(36056)
    NonEmptyString("5").reduceOption((c1, c2) => (c1 + c2).toChar).map(_.toInt) shouldBe Some(53)
    NonEmptyString("5").reduceOption((c1, c2) => (c1 * c2).toChar).map(_.toInt) shouldBe Some(53)
  }
  it should "have a reduceRight method" in {
    NonEmptyString("1").reduceRight((c1, c2) => (c1 * c2).toChar) shouldBe 49
    NonEmptyString("123").reduceRight((c1, c2) => (c1 + c2).toChar) shouldBe 150
    NonEmptyString("123").reduceRight((c1, c2) => (c1 * c2).toChar) shouldBe 59414
    NonEmptyString("12345").reduceRight((c1, c2) => (c1 * c2).toChar) shouldBe 36056
  }
  it should "have a reduceRightOption method" in {
    NonEmptyString("1").reduceRightOption((c1, c2) => (c1 + c2).toChar) shouldBe Some(49)
    NonEmptyString("1").reduceRightOption((c1, c2) => (c1 * c2).toChar) shouldBe Some(49)
    NonEmptyString("123").reduceRightOption((c1, c2) => (c1 + c2).toChar) shouldBe Some(150)
    NonEmptyString("123").reduceRightOption((c1, c2) => (c1 * c2).toChar) shouldBe Some(59414)
    NonEmptyString("12345").reduceRightOption((c1, c2) => (c1 * c2).toChar) shouldBe Some(36056)
  }
  it should "have a reverse method" in {
    NonEmptyString("33").reverse shouldBe NonEmptyString("33")
    NonEmptyString("333435").reverse shouldBe NonEmptyString("534333")
  }
  it should "have a reverseIterator method" in {
    NonEmptyString("3").reverseIterator.toStream shouldBe Stream('3')
    NonEmptyString("123").reverseIterator.toStream shouldBe Stream('3', '2', '1')
  }
  it should "have a reverseMap method" in {
    NonEmptyString("3").reverseMap(_.toString.toInt + 1).toList shouldBe List(4)
    NonEmptyString("123").reverseMap(_.toString.toInt + 1).toList shouldBe List(4, 3, 2)
  }
  /*it should "have a runWith method, inherited from PartialFunction" in {
    // TODO: What is this? Seems to be testing Vector or String instead of Every or NonEmptyString.
    var x = 0
    val f = String(1, 2, 3).runWith(x += _)

    f(0) shouldBe true
    x shouldBe 1

    f(1) shouldBe true
    x shouldBe 3

    f(2) shouldBe true
    x shouldBe 6

    f(3) shouldBe false

    var y = 0
    val g = String(3).runWith(y += _)

    g(0) shouldBe true
    y shouldBe 3

    g(0) shouldBe true
    y shouldBe 6
  }*/
  it should "have a sameElements method that takes a GenIterable" in {
    NonEmptyString("12345").sameElements(List('1', '2', '3', '4', '5')) shouldBe true
    NonEmptyString("12345").sameElements(List('1', '2', '3', '4')) shouldBe false
    NonEmptyString("12345").sameElements(List('1', '2', '3', '4', '5', '6')) shouldBe false
    NonEmptyString("12345").sameElements(List('1', '2', '3', '4', '4')) shouldBe false
    NonEmptyString("3").sameElements(List('1', '2', '3', '4', '5')) shouldBe false
    NonEmptyString("3").sameElements(List('1')) shouldBe false
    NonEmptyString("3").sameElements(List('3')) shouldBe true
  }
  it should "have a sameElements method that takes an Every" in {
    NonEmptyString("12345").sameElements(Every('1', '2', '3', '4', '5')) shouldBe true
    NonEmptyString("12345").sameElements(Every('1', '2', '3', '4')) shouldBe false
    NonEmptyString("12345").sameElements(Every('1', '2', '3', '4', '5', '6')) shouldBe false
    NonEmptyString("12345").sameElements(Every('1', '2', '3', '4', '4')) shouldBe false
    NonEmptyString("3").sameElements(Every('1', '2', '3', '4', '5')) shouldBe false
    NonEmptyString("3").sameElements(Every('1')) shouldBe false
    NonEmptyString("3").sameElements(Every('3')) shouldBe true
  }
  it should "have a sameElements method that takes a NonEmptyString" in {
    NonEmptyString("12345").sameElements(NonEmptyString("12345")) shouldBe true
    NonEmptyString("12345").sameElements(NonEmptyString("1234")) shouldBe false
    NonEmptyString("12345").sameElements(NonEmptyString("123456")) shouldBe false
    NonEmptyString("12345").sameElements(NonEmptyString("12344")) shouldBe false
    NonEmptyString("3").sameElements(NonEmptyString("12345")) shouldBe false
    NonEmptyString("3").sameElements(NonEmptyString("1")) shouldBe false
    NonEmptyString("3").sameElements(NonEmptyString("3")) shouldBe true
  }
  it should "have a sameElements method that takes a String" in {
    NonEmptyString("12345").sameElements("12345") shouldBe true
    NonEmptyString("12345").sameElements("1234") shouldBe false
    NonEmptyString("12345").sameElements("123456") shouldBe false
    NonEmptyString("12345").sameElements("12344") shouldBe false
    NonEmptyString("3").sameElements("12345") shouldBe false
    NonEmptyString("3").sameElements("1") shouldBe false
    NonEmptyString("3").sameElements("3") shouldBe true
  }
  it should "have a scan method" in {
    NonEmptyString("1").scan('0')((e1, e2) => (e1 + e2).toChar) shouldBe NonEmptyString("0a")
    NonEmptyString("123").scan('0')((e1, e2) => e2) shouldBe NonEmptyString("0123")
    NonEmptyString("123").scan('z')((e1, e2) => (e2 + 1).toChar) shouldBe NonEmptyString("z234")
    NonEmptyString("0").scan('a')((e1, e2) => (e2 + 2).toChar) shouldBe NonEmptyString("a2")
  }
  it should "have a scanLeft method" in {
    NonEmptyString("1").scanLeft("0")(_ + _) shouldBe Vector("0", "01")
    NonEmptyString("123").scanLeft("0")(_ + _) shouldBe Vector("0", "01", "012", "0123")
    NonEmptyString("123").scanLeft(0)(_ + _.toString.toInt) shouldBe Vector(0, 1, 3, 6)
    NonEmptyString("123").scanLeft("z")(_ + _) shouldBe Vector("z", "z1", "z12", "z123")
    NonEmptyString("0").scanLeft("z")(_ + _) shouldBe Vector("z", "z0")
  }
  it should "have a scanRight method" in {
    NonEmptyString("1").scanRight("0")(_.toString + _) shouldBe Vector("10", "0")
    NonEmptyString("123").scanRight("0")(_.toString + _) shouldBe Vector("1230", "230", "30", "0")
    NonEmptyString("123").scanRight(0)(_.toString.toInt + _) shouldBe Vector(6, 5, 3, 0)
    NonEmptyString("123").scanRight("z")(_.toString + _) shouldBe Vector("123z", "23z", "3z", "z")
    NonEmptyString("0").scanRight("z")(_.toString + _) shouldBe Vector("0z", "z")
  }
  it should "have a segmentLength method" in {
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 7, 0) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt == 7, 0) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 0, 0) shouldBe 10
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 1, 0) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 0, 10) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 0, 8) shouldBe 2
    NonEmptyString("1234566789").segmentLength(_.toString.toInt < 3, 0) shouldBe 2
    NonEmptyString("1234566789").segmentLength(_.toString.toInt < 5, 0) shouldBe 4
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 5, 0) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 5, 5) shouldBe 5
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 5, 4) shouldBe 0
    NonEmptyString("1234566789").segmentLength(_.toString.toInt > 5, 6) shouldBe 4
  }
  // it should "have a seq method" is pending
  it should "have a size method" in {
    NonEmptyString("5").size shouldBe 1
    NonEmptyString("123").size shouldBe 3
  }
  /*
  it should not have a slice method
    scala> Vector(3).slice(0, 0)
    res83: scala.collection.immutable.Vector[Int] = Vector()
    scala> Vector(1, 2, 3, 4, 5).slice(2, 1)
    res84: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have 2 sliding methods" in {

    NonEmptyString("1").sliding(1).toList shouldBe List(NonEmptyString("1"))
    NonEmptyString("1").sliding(2).toList shouldBe List(NonEmptyString("1"))
    NonEmptyString("123").sliding(2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("23"))
    NonEmptyString("123").sliding(1).toList shouldBe List(NonEmptyString("1"), NonEmptyString("2"), NonEmptyString("3"))
    NonEmptyString("123").sliding(3).toList shouldBe List(NonEmptyString("123"))
    NonEmptyString("12345").sliding(3).toList shouldBe List(NonEmptyString("123"), NonEmptyString("234"), NonEmptyString("345"))
    NonEmptyString("12345").sliding(2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("23"), NonEmptyString("34"), NonEmptyString("45"))
    NonEmptyString("12345").sliding(1).toList shouldBe List(NonEmptyString("1"), NonEmptyString("2"), NonEmptyString("3"), NonEmptyString("4"), NonEmptyString("5"))
    NonEmptyString("12345").sliding(4).toList shouldBe List(NonEmptyString("1234"), NonEmptyString("2345"))
    NonEmptyString("12345").sliding(5).toList shouldBe List(NonEmptyString("12345"))

    NonEmptyString("1").sliding(1, 1).toList shouldBe List(NonEmptyString("1"))
    NonEmptyString("1").sliding(1, 2).toList shouldBe List(NonEmptyString("1"))
    NonEmptyString("123").sliding(1, 1).toList shouldBe List(NonEmptyString("1"), NonEmptyString("2"), NonEmptyString("3"))
    NonEmptyString("123").sliding(2, 1).toList shouldBe List(NonEmptyString("12"), NonEmptyString("23"))
    NonEmptyString("123").sliding(2, 2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("3"))
    NonEmptyString("123").sliding(3, 2).toList shouldBe List(NonEmptyString("123"))
    NonEmptyString("123").sliding(3, 1).toList shouldBe List(NonEmptyString("123"))
    NonEmptyString("12345").sliding(3, 1).toList shouldBe List(NonEmptyString("123"), NonEmptyString("234"), NonEmptyString("345"))
    NonEmptyString("12345").sliding(2, 2).toList shouldBe List(NonEmptyString("12"), NonEmptyString("34"), NonEmptyString("5"))
    NonEmptyString("12345").sliding(2, 3).toList shouldBe List(NonEmptyString("12"), NonEmptyString("45"))
    NonEmptyString("12345").sliding(2, 4).toList shouldBe List(NonEmptyString("12"), NonEmptyString("5"))
    NonEmptyString("12345").sliding(3, 1).toList shouldBe List(NonEmptyString("123"), NonEmptyString("234"), NonEmptyString("345"))
    NonEmptyString("12345").sliding(3, 2).toList shouldBe List(NonEmptyString("123"), NonEmptyString("345"))
    NonEmptyString("12345").sliding(3, 3).toList shouldBe List(NonEmptyString("123"), NonEmptyString("45"))
    NonEmptyString("12345").sliding(3, 4).toList shouldBe List(NonEmptyString("123"), NonEmptyString("5"))
  }
  it should "have a sortBy method" in {
    NonEmptyString("12345").sortBy(_ % 2) shouldBe NonEmptyString("24135")
    NonEmptyString("12345").sortBy(_ % 3) shouldBe NonEmptyString("31425")
  }
  it should "have a sortWith method" in {
    NonEmptyString("12345").sortWith(_ > _) shouldBe NonEmptyString("54321")
    NonEmptyString("21453").sortWith(_ > _) shouldBe NonEmptyString("54321")
  }
  it should "have a sorted method" in {
    NonEmptyString("12345").sorted shouldBe NonEmptyString("12345")
    NonEmptyString("54321").sorted shouldBe NonEmptyString("12345")
    NonEmptyString("21453").sorted shouldBe NonEmptyString("12345")
  }
  /*
  it should not have a span method
    scala> Vector(1, 2, 3, 4, 5).span(_ > 10)
    res105: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  it should not have a splitAt method
    scala> Vector(1, 2, 3, 4, 5).splitAt(0)
    res106: (scala.collection.immutable.Vector[Int], scala.collection.immutable.Vector[Int]) = (Vector(),Vector(1, 2, 3, 4, 5))
  */
  it should "have 2 startsWith methods that take a GenSeq" in {
    NonEmptyString("123").startsWith(List('1')) shouldBe true
    NonEmptyString("123").startsWith(List('1', '2')) shouldBe true
    NonEmptyString("123").startsWith(List('1', '2', '3')) shouldBe true
    NonEmptyString("123").startsWith(List('1', '2', '3', '4')) shouldBe false
    NonEmptyString("1").startsWith(List('1', '2', '3', '4')) shouldBe false
    NonEmptyString("1").startsWith(List('1')) shouldBe true
    NonEmptyString("1").startsWith(List('2')) shouldBe false

    NonEmptyString("1").startsWith(List('1'), 0) shouldBe true
    NonEmptyString("1").startsWith(List('1'), 1) shouldBe false
    NonEmptyString("123").startsWith(List('1'), 1) shouldBe false
    NonEmptyString("123").startsWith(List('1'), 2) shouldBe false
    NonEmptyString("123").startsWith(List('2'), 2) shouldBe false
    NonEmptyString("123").startsWith(List('2'), 1) shouldBe true
    NonEmptyString("123").startsWith(List('2', '3'), 1) shouldBe true
    NonEmptyString("123").startsWith(List('1', '2', '3'), 1) shouldBe false
    NonEmptyString("123").startsWith(List('1', '2', '3'), 0) shouldBe true
    NonEmptyString("12345").startsWith(List('3', '4'), 2) shouldBe true
    NonEmptyString("12345").startsWith(List('3', '4', '5'), 2) shouldBe true
    NonEmptyString("12345").startsWith(List('3', '4', '5', '6'), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take an Every" in {
    NonEmptyString("123").startsWith(Every('1')) shouldBe true
    NonEmptyString("123").startsWith(Every('1', '2')) shouldBe true
    NonEmptyString("123").startsWith(Every('1', '2', '3')) shouldBe true
    NonEmptyString("123").startsWith(Every('1', '2', '3', '4')) shouldBe false
    NonEmptyString("1").startsWith(Every('1', '2', '3', '4')) shouldBe false
    NonEmptyString("1").startsWith(Every('1')) shouldBe true
    NonEmptyString("1").startsWith(Every('2')) shouldBe false

    NonEmptyString("1").startsWith(Every('1'), 0) shouldBe true
    NonEmptyString("1").startsWith(Every('1'), 1) shouldBe false
    NonEmptyString("123").startsWith(Every('1'), 1) shouldBe false
    NonEmptyString("123").startsWith(Every('1'), 2) shouldBe false
    NonEmptyString("123").startsWith(Every('2'), 2) shouldBe false
    NonEmptyString("123").startsWith(Every('2'), 1) shouldBe true
    NonEmptyString("123").startsWith(Every('2', '3'), 1) shouldBe true
    NonEmptyString("123").startsWith(Every('1', '2', '3'), 1) shouldBe false
    NonEmptyString("123").startsWith(Every('1', '2', '3'), 0) shouldBe true
    NonEmptyString("12345").startsWith(Every('3', '4'), 2) shouldBe true
    NonEmptyString("12345").startsWith(Every('3', '4', '5'), 2) shouldBe true
    NonEmptyString("12345").startsWith(Every('3', '4', '5', '6'), 2) shouldBe false
  }
  it should "have 2 startsWith methods that take a NonEmptyString" in {
    NonEmptyString("123").startsWith(NonEmptyString("1")) shouldBe true
    NonEmptyString("123").startsWith(NonEmptyString("12")) shouldBe true
    NonEmptyString("123").startsWith(NonEmptyString("123")) shouldBe true
    NonEmptyString("123").startsWith(NonEmptyString("1234")) shouldBe false
    NonEmptyString("1").startsWith(NonEmptyString("1234")) shouldBe false
    NonEmptyString("1").startsWith(NonEmptyString("1")) shouldBe true
    NonEmptyString("1").startsWith(NonEmptyString("2")) shouldBe false

    NonEmptyString("1").startsWith(NonEmptyString("1"), 0) shouldBe true
    NonEmptyString("1").startsWith(NonEmptyString("1"), 1) shouldBe false
    NonEmptyString("123").startsWith(NonEmptyString("1"), 1) shouldBe false
    NonEmptyString("123").startsWith(NonEmptyString("1"), 2) shouldBe false
    NonEmptyString("123").startsWith(NonEmptyString("2"), 2) shouldBe false
    NonEmptyString("123").startsWith(NonEmptyString("2"), 1) shouldBe true
    NonEmptyString("123").startsWith(NonEmptyString("23"), 1) shouldBe true
    NonEmptyString("123").startsWith(NonEmptyString("123"), 1) shouldBe false
    NonEmptyString("123").startsWith(NonEmptyString("123"), 0) shouldBe true
    NonEmptyString("12345").startsWith(NonEmptyString("34"), 2) shouldBe true
    NonEmptyString("12345").startsWith(NonEmptyString("345"), 2) shouldBe true
    NonEmptyString("12345").startsWith(NonEmptyString("3456"), 2) shouldBe false
  }
  it should "have a stringPrefix method" in {
    NonEmptyString("1").stringPrefix shouldBe "NonEmptyString"
    NonEmptyString("123").stringPrefix shouldBe "NonEmptyString"
  }
  it should "have a sum method" in {
    NonEmptyString("1").sum shouldBe 49
    NonEmptyString("5").sum shouldBe 53
    NonEmptyString("123").sum shouldBe 150
    NonEmptyString("12345").sum shouldBe 255
  }
  /*
    it should not have a tail method
      scala> Vector(1).tail
      res7: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a tails method
      scala> Vector(1).tails.toString
      res8: String[scala.collection.immutable.Vector[Int]] = String(Vector(1), Vector())

    it should not have a take method
      scala> Vector(1).take(0)
      res10: scala.collection.immutable.Vector[Int] = Vector()
      scala> Vector(1, 2, 3).take(0)
      res11: scala.collection.immutable.Vector[Int] = Vector()
      scala> Vector(1, 2, 3).take(-1)
      res12: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a takeRight method
      scala> Vector(1).takeRight(1)
      res13: scala.collection.immutable.Vector[Int] = Vector(1)
      scala> Vector(1).takeRight(0)
      res14: scala.collection.immutable.Vector[Int] = Vector()
      scala> Vector(1, 2, 3).takeRight(0)
      res15: scala.collection.immutable.Vector[Int] = Vector()

    it should not have a takeWhile method
      scala> Vector(1, 2, 3).takeWhile(_ > 10)
      res17: scala.collection.immutable.Vector[Int] = Vector()
      scala> Vector(1).takeWhile(_ > 10)
      res18: scala.collection.immutable.Vector[Int] = Vector()
  */
  it should "have a to method" in {
    import org.scalactic.ColCompatHelper.Factory._
    NonEmptyString("1").to(List) shouldBe List('1')
    NonEmptyString("123").to(List) shouldBe List('1', '2', '3')
    NonEmptyString("123").to(scala.collection.mutable.ListBuffer) shouldBe ListBuffer('1', '2', '3')
    NonEmptyString("123").to(Vector) shouldBe Vector('1', '2', '3')
  }
  it should "have a toArray method" in {
    NonEmptyString("123").toArray should === (Array('1', '2', '3'))
    NonEmptyString("ab").toArray should === (Array('a', 'b'))
    NonEmptyString("1").toArray should === (Array('1'))
  }
  it should "have a toBuffer method" in {
    NonEmptyString("123").toBuffer should === (Buffer('1', '2', '3'))
    NonEmptyString("ab").toBuffer should === (Buffer('a', 'b'))
    NonEmptyString("1").toBuffer should === (Buffer('1'))
  }
  it should "have a toIndexedSeq method" in {
    NonEmptyString("123").toIndexedSeq should === (IndexedSeq('1', '2', '3'))
    NonEmptyString("ab").toIndexedSeq should === (IndexedSeq('a', 'b'))
    NonEmptyString("1").toIndexedSeq should === (IndexedSeq('1'))
  }
  it should "have a toIterable method" in {
    NonEmptyString("123").toIterable should === (Iterable('1', '2', '3'))
    NonEmptyString("ab").toIterable should === (Iterable('a', 'b'))
    NonEmptyString("1").toIterable should === (Iterable('1'))
  }
  it should "have a toIterator method" in {
    NonEmptyString("123").toIterator.toString should === (Iterator('1', '2', '3').toString)
    NonEmptyString("ab").toIterator.toString should === (Iterator('a', 'b').toString)
    NonEmptyString("1").toIterator.toString should === (Iterator('1').toString)
    NonEmptyString("123").toIterator shouldBe an [Iterator[_]]
    NonEmptyString("ab").toIterator shouldBe an [Iterator[_]]
    NonEmptyString("1").toIterator shouldBe an [Iterator[_]]
  }
  it should "have a toString method" in {
    NonEmptyString("123").toString should === ("NonEmptyString(123)")
    NonEmptyString("ab").toString should === ("NonEmptyString(ab)")
    NonEmptyString("1").toString should === ("NonEmptyString(1)")
  }
  it should "have a toMap method" in {
    NonEmptyString("123").toMap should === (Map(0 -> '1', 1 -> '2', 2 -> '3'))
    NonEmptyString("ab").toMap should === (Map(0 -> 'a', 1 -> 'b'))
    NonEmptyString("1").toMap should === (Map(0 -> '1'))
  }
  it should "have a toSeq method" in {
    NonEmptyString("123").toSeq should === (Seq('1', '2', '3'))
    NonEmptyString("ab").toSeq should === (Seq('a', 'b'))
    NonEmptyString("1").toSeq should === (Seq('1'))
  }
  it should "have a toSet method" in {
    NonEmptyString("123").toSet should === (Set('1', '2', '3'))
    NonEmptyString("ab").toSet should === (Set('a', 'b'))
    NonEmptyString("1").toSet should === (Set('1'))
  }
  it should "have a toStream method" in {
    NonEmptyString("123").toStream should === (Stream('1', '2', '3'))
    NonEmptyString("ab").toStream should === (Stream('a', 'b'))
    NonEmptyString("1").toStream should === (Stream('1'))
  }
  it should "have a toVector method" in {
    NonEmptyString("123").toVector should === (Vector('1', '2', '3'))
    NonEmptyString("ab").toVector should === (Vector('a', 'b'))
    NonEmptyString("1").toVector should === (Vector('1'))
  }
  it should "have a union method that takes a GenSeq" in {
    NonEmptyString("1") union List('1') shouldBe NonEmptyString("11")
    NonEmptyString("1") union List('1', '2') shouldBe NonEmptyString("112")
    NonEmptyString("12") union List('1', '2') shouldBe NonEmptyString("1212")
    NonEmptyString("12") union List('1') shouldBe NonEmptyString("121")
    NonEmptyString("12") union List('3', '4', '5') shouldBe NonEmptyString("12345")
    NonEmptyString("123") union List('3', '4', '5') shouldBe NonEmptyString("123345")
  }
  it should "have a union method that takes an Every" in {
    NonEmptyString("1") union Every('1') shouldBe NonEmptyString("11")
    NonEmptyString("1") union Every('1', '2') shouldBe NonEmptyString("112")
    NonEmptyString("12") union Every('1', '2') shouldBe NonEmptyString("1212")
    NonEmptyString("12") union Every('1') shouldBe NonEmptyString("121")
    NonEmptyString("12") union Every('3', '4', '5') shouldBe NonEmptyString("12345")
    NonEmptyString("123") union Every('3', '4', '5') shouldBe NonEmptyString("123345")
  }
  it should "have a union method that takes a NonEmptyString" in {
    NonEmptyString("1") union NonEmptyString("1") shouldBe NonEmptyString("11")
    NonEmptyString("1") union NonEmptyString("12") shouldBe NonEmptyString("112")
    NonEmptyString("12") union NonEmptyString("12") shouldBe NonEmptyString("1212")
    NonEmptyString("12") union NonEmptyString("1") shouldBe NonEmptyString("121")
    NonEmptyString("12") union NonEmptyString("345") shouldBe NonEmptyString("12345")
    NonEmptyString("123") union NonEmptyString("345") shouldBe NonEmptyString("123345")
  }
  it should "have a union method that takes a String" in {
    NonEmptyString("1") union "1" shouldBe NonEmptyString("11")
    NonEmptyString("1") union "12" shouldBe NonEmptyString("112")
    NonEmptyString("12") union "12" shouldBe NonEmptyString("1212")
    NonEmptyString("12") union "1" shouldBe NonEmptyString("121")
    NonEmptyString("12") union "345" shouldBe NonEmptyString("12345")
    NonEmptyString("123") union "345" shouldBe NonEmptyString("123345")
  }
  it should "have an unzip method" in {
    implicit def test(c: Char): (Int, Char) = (c.toInt, c)
    NonEmptyString("12").unzip shouldBe (Vector(49, 50), Vector('1', '2'))
    NonEmptyString("1234").unzip shouldBe (Vector(49, 50, 51, 52), Vector('1', '2', '3', '4'))
    NonEmptyString("123456").unzip shouldBe (Vector(49, 50, 51, 52, 53, 54), Vector('1', '2', '3', '4', '5', '6'))
  }
  it should "have an unzip3 method" in {
    implicit def test(c: Char): (Int, String, Char) = (c.toInt, c.toString + c.toInt, c)
    NonEmptyString("123").unzip3 shouldBe (Vector(49, 50, 51), Vector("149", "250", "351"), Vector('1', '2', '3'))
    NonEmptyString("123456").unzip3 shouldBe (Vector(49, 50, 51, 52, 53, 54), Vector("149", "250", "351", "452", "553", "654"), Vector('1', '2', '3', '4', '5', '6'))
    NonEmptyString("123456789").unzip3 shouldBe (Vector(49, 50, 51, 52, 53, 54, 55, 56, 57), Vector("149", "250", "351", "452", "553", "654", "755", "856", "957"), Vector('1', '2', '3', '4', '5', '6', '7', '8', '9'))
  }
  it should "have an updated method" in {
    NonEmptyString("1").updated(0, '2') shouldBe NonEmptyString("2")
    an [IndexOutOfBoundsException] should be thrownBy { NonEmptyString("1").updated(1, '2') }
    NonEmptyString("111").updated(1, '2') shouldBe NonEmptyString("121")
    NonEmptyString("111").updated(2, '2') shouldBe NonEmptyString("112")
    NonEmptyString("111").updated(0, '2') shouldBe NonEmptyString("211")
  }
  /*
  it should not have 2 view methods, because I don't want to support views in Every
  */
  /*
    it should not have a zip method
      scala> String(1) zip Nil
      res0: String[(Int, Nothing)] = String()
  */
  it should "have a zipAll method that takes an Iterable" in {

    // Empty on right
    NonEmptyString("1").zipAll(Nil, 'a', -2) shouldBe Iterable(('1', -2))
    NonEmptyString("12").zipAll(Nil, 'a', -2) shouldBe Iterable(('1', -2), ('2', -2))

    // Same length
    NonEmptyString("1").zipAll(List(1), 'a', -2) shouldBe Iterable(('1', 1))
    NonEmptyString("12").zipAll(List(1, 2), 'a', -2) shouldBe Iterable(('1', 1), ('2', 2))

    // Non-empty, longer on right
    NonEmptyString("1").zipAll(List(10, 20), 'a', -2) shouldBe Iterable(('1', 10), ('a', 20))
    NonEmptyString("12").zipAll(List(10, 20, 30), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('a', 30))

    // Non-empty, shorter on right
    NonEmptyString("123").zipAll(List(10, 20), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('3', -2))
    NonEmptyString("1234").zipAll(List(10, 20, 30), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('3', 30), ('4', -2))
  }
  it should "have a zipAll method that takes an Every" in {

    // Same length
    NonEmptyString("1").zipAll(Every(1), 'a', -2) shouldBe Iterable(('1', 1))
    NonEmptyString("12").zipAll(Every(1, 2), 'a', -2) shouldBe Iterable(('1', 1), ('2', 2))

    // Non-empty, longer on right
    NonEmptyString("1").zipAll(Every(10, 20), 'a', -2) shouldBe Iterable(('1', 10), ('a', 20))
    NonEmptyString("12").zipAll(Every(10, 20, 30), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('a', 30))

    // Non-empty, shorter on right
    NonEmptyString("123").zipAll(Every(10, 20), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('3', -2))
    NonEmptyString("1234").zipAll(Every(10, 20, 30), 'a', -2) shouldBe Iterable(('1', 10), ('2', 20), ('3', 30), ('4', -2))
  }
  it should "have a zipAll method that takes a NonEmptyString" in {

    // Same length
    NonEmptyString("1").zipAll(NonEmptyString("1"), 'a', 'b') shouldBe Iterable(('1', '1'))
    NonEmptyString("12").zipAll(NonEmptyString("12"), 'a', 'b') shouldBe Iterable(('1', '1'), ('2', '2'))

    // Non-empty, longer on right
    NonEmptyString("1").zipAll(NonEmptyString("12"), 'a', 'b') shouldBe Iterable(('1', '1'), ('a', '2'))
    NonEmptyString("12").zipAll(NonEmptyString("123"), 'a', 'b') shouldBe Iterable(('1', '1'), ('2', '2'), ('a', '3'))

    // Non-empty, shorter on right
    NonEmptyString("123").zipAll(NonEmptyString("12"), 'a', 'b') shouldBe Iterable(('1', '1'), ('2', '2'), ('3', 'b'))
    NonEmptyString("1234").zipAll(NonEmptyString("123"), 'a', 'b') shouldBe Iterable(('1', '1'), ('2', '2'), ('3', '3'), ('4', 'b'))
  }
  it should "have a zipWithIndex method" in {
    NonEmptyString("99").zipWithIndex shouldBe Iterable(('9', 0), ('9', 1))
    NonEmptyString("12345").zipWithIndex shouldBe Iterable(('1', 0), ('2', 1), ('3', 2), ('4', 3), ('5', 4))
  }
}

