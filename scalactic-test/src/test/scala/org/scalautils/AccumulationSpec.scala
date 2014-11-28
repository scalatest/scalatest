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

import java.text._
import org.scalatest._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import prop.TableDrivenPropertyChecks._

class AccumulationSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  import java.util.Date
  case class Person(name: String, birthday: Date, address: List[String])

  def parseDate(in: String): Date Or One[ErrorMessage] = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd")
    sdf.parse(in, new ParsePosition(0)) match {
      case null => Bad(One(s"Can't parse [$in] as a date"))
      case date => Good(date)
    }
  }

  "Ny parseDate helper method" can "parse dates with the greatest of ease" in {
    parseDate("1992-02-10") shouldBe 'good
    parseDate("1992-xx-10") shouldBe 'bad
  }

  def parseAddress(in: String): List[String] Or One[ErrorMessage] = {
    val address = in.split(",\\s*")
    if (address.length > 1) Good(address.toList)
    else Bad(One(s"An address needs to have a street and a city, separated by a comma; found [$in]"))
  }

  "Ny parseAddress helper method" can "parse addresses with the greatest of ease" in {
    parseAddress("Some Street 123, Some Town") shouldBe 'good
    parseAddress("Rotterdam") shouldBe 'bad
  }

  def parseOptionalName(in: Option[String]): String Or One[ErrorMessage] =
    in match {
      case Some(nm) => Good(nm)
      case None => Bad(One("No name found"))
    }
  def parseOptionalDate(in: Option[String]): Date Or One[ErrorMessage] = 
    in match {
      case Some(dt) => parseDate(dt)
      case None => Bad(One("No date found"))
    }
  def parseOptionalAddress(in: Option[String]): List[String] Or One[ErrorMessage] = 
    in match {
     case Some(addr) => parseAddress(addr)
     case None => Bad(One("No address found"))
    }

  "The Accumulation trait" can "insert one Or into a function" in {
    def parseAddressForPerson(name: String, date: Date, in: String): Person Or Every[ErrorMessage] = {
      val address: List[String] Or One[ErrorMessage] = parseOptionalAddress(if (in.isEmpty) None else Some(in))
      withGood(address) {
        Person(name, date, _)
      } 
    }
    parseAddressForPerson("Name Only", parseDate("1974-02-11").get, "") shouldBe Bad(One("No address found"))
    parseAddressForPerson("Joe Collegue", parseDate("1974-02-11").get, "Rotterdam") shouldBe Bad(One("An address needs to have a street and a city, separated by a comma; found [Rotterdam]"))
    parseAddressForPerson("Bart Schuller", parseDate("2012-02-29").get, "Some Street 123, Some Town") shouldBe Good(Person("Bart Schuller", parseDate("2012-02-29").get, parseAddress("Some Street 123, Some Town").get))
  }

  it can "combine two Ors into a function" in {
    def parseDateAndAddress(name: String, in: String): Person Or Every[ErrorMessage] = {
      val components = in.split(';').lift
      val date: Date Or One[ErrorMessage] = parseOptionalDate(components(0))
      val address: List[String] Or One[ErrorMessage] = parseOptionalAddress(components(1))
      withGood(date, address) {
        Person(name, _, _)
      } 
    }
    parseDateAndAddress("Name Only", "") shouldBe Bad(Many("Can't parse [] as a date", "No address found"))
    parseDateAndAddress("Joe Collegue", "1974-??-??;Rotterdam") shouldBe Bad(Many("Can't parse [1974-??-??] as a date", "An address needs to have a street and a city, separated by a comma; found [Rotterdam]"))
    parseDateAndAddress("Bart Schuller", "2012-02-29;Some Street 123, Some Town") shouldBe Good(Person("Bart Schuller", parseDate("2012-02-29").get, parseAddress("Some Street 123, Some Town").get))
  }

  it can "combine three Ors into a function" in {
    def parsePerson(in: String): Person Or Every[ErrorMessage] = {
      val components = in.split(';').lift
      val name: String Or One[ErrorMessage] = parseOptionalName(components(0))
      val date: Date Or One[ErrorMessage] = parseOptionalDate(components(1))
      val address: List[String] Or One[ErrorMessage] = parseOptionalAddress(components(2))
      // This would require an implicit conversion on 22 arities of function:
      // { Person(_, _, _) } withGood(name, date, address)
      // Whereas this one can be in Goodations._ and I can have one per arity
      // i.e., one overloaded form per arity.
      withGood(name, date, address) {
        Person(_, _, _)
      } 
    }

    val bad = "Name Only"
    val partial = "Joe Colleague;1974-??-??;Rotterdam"
    val good = "Bart Schuller;2012-02-29;Some Street 123, Some Town"
    parsePerson(bad) shouldBe Bad(Many("No date found", "No address found"))
    parsePerson(partial) shouldBe Bad(Many("Can't parse [1974-??-??] as a date", "An address needs to have a street and a city, separated by a comma; found [Rotterdam]"))
    parsePerson(good) shouldBe Good(Person("Bart Schuller", parseDate("2012-02-29").get, parseAddress("Some Street 123, Some Town").get))
  }

  case class X22(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int, xr: Int, xs: Int, xt: Int, xu: Int, xv: Int)
  case class X21(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int, xr: Int, xs: Int, xt: Int, xu: Int)
  case class X20(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int, xr: Int, xs: Int, xt: Int)
  case class X19(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int, xr: Int, xs: Int)
  case class X18(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int, xr: Int)
  case class X17(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int, xq: Int)
  case class X16(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int, xp: Int)
  case class X15(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int, xo: Int)
  case class X14(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int, xn: Int)
  case class X13(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int, xm: Int)
  case class X12(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int, xl: Int)
  case class X11(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int, xk: Int)
  case class X10(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int, xj: Int)
  case class X9(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int, xi: Int)
  case class X8(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int, xh: Int)
  case class X7(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int, xg: Int)
  case class X6(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int, xf: Int)
  case class X5(xa: Int, xb: Int, xc: Int, xd: Int, xe: Int)
  case class X4(xa: Int, xb: Int, xc: Int, xd: Int)
  case class X3(xa: Int, xb: Int, xc: Int)
  case class X2(xa: Int, xb: Int)
  case class X1(xa: Int)

  def parse(s: String, c: Char): Int Or One[Char] = {
    val count = s.count(_ == c)
    if (count > 0) Good(count) else Bad(One(c))
  }

  def gimme1(s: String, ca: Char): X1 Or Every[Char] = {
    val xa = parse(s, ca)
    withGood(xa) { X1(_) }
  }

  def gimme2(s: String, ca: Char, cb: Char): X2 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    withGood(xa, xb) { X2(_, _) }
  }

  def gimme3(s: String, ca: Char, cb: Char, cc: Char): X3 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    withGood(xa, xb, xc) { X3(_, _, _) }
  }

  def gimme4(s: String, ca: Char, cb: Char, cc: Char, cd: Char): X4 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    withGood(xa, xb, xc, xd) { X4(_, _, _, _) }
  }

  def gimme5(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char): X5 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    withGood(xa, xb, xc, xd, xe) { X5(_, _, _, _, _) }
  }


  def gimme6(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char): X6 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    withGood(xa, xb, xc, xd, xe, xf) { X6(_, _, _, _, _, _) }
  }


  def gimme7(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char): X7 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    withGood(xa, xb, xc, xd, xe, xf, xg) { X7(_, _, _, _, _, _, _) }
  }


  def gimme8(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char): X8 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh) { X8(_, _, _, _, _, _, _, _) }
  }


  def gimme9(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char): X9 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi) { X9(_, _, _, _, _, _, _, _, _) }
  }


  def gimme10(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char): X10 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj) { X10(_, _, _, _, _, _, _, _, _, _) }
  }


  def gimme11(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char): X11 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk) { X11(_, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme12(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char): X12 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl) { X12(_, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme13(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char): X13 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm) { X13(_, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme14(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char): X14 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn) { X14(_, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme15(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char): X15 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo) { X15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme16(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char): X16 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp) { X16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme17(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char): X17 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq) { X17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme18(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char, cr: Char): X18 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    val xr = parse(s, cr)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq, xr) { X18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme19(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char, cr: Char, cs: Char): X19 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    val xr = parse(s, cr)
    val xs = parse(s, cs)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq, xr, xs) { X19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme20(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char, cr: Char, cs: Char, ct: Char): X20 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    val xr = parse(s, cr)
    val xs = parse(s, cs)
    val xt = parse(s, ct)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq, xr, xs, xt) { X20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme21(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char, cr: Char, cs: Char, ct: Char, cu: Char): X21 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    val xr = parse(s, cr)
    val xs = parse(s, cs)
    val xt = parse(s, ct)
    val xu = parse(s, cu)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq, xr, xs, xt, xu) { X21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }


  def gimme22(s: String, ca: Char, cb: Char, cc: Char, cd: Char, ce: Char, cf: Char, cg: Char, ch: Char, ci: Char, cj: Char, ck: Char, cl: Char, cm: Char, cn: Char, co: Char, cp: Char, cq: Char, cr: Char, cs: Char, ct: Char, cu: Char, cv: Char): X22 Or Every[Char] = {
    val xa = parse(s, ca)
    val xb = parse(s, cb)
    val xc = parse(s, cc)
    val xd = parse(s, cd)
    val xe = parse(s, ce)
    val xf = parse(s, cf)
    val xg = parse(s, cg)
    val xh = parse(s, ch)
    val xi = parse(s, ci)
    val xj = parse(s, cj)
    val xk = parse(s, ck)
    val xl = parse(s, cl)
    val xm = parse(s, cm)
    val xn = parse(s, cn)
    val xo = parse(s, co)
    val xp = parse(s, cp)
    val xq = parse(s, cq)
    val xr = parse(s, cr)
    val xs = parse(s, cs)
    val xt = parse(s, ct)
    val xu = parse(s, cu)
    val xv = parse(s, cv)
    withGood(xa, xb, xc, xd, xe, xf, xg, xh, xi, xj, xk, xl, xm, xn, xo, xp, xq, xr, xs, xt, xu, xv) { X22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) }
  }

  it should "work with arity 1" in {
    val examples =
      Table(
        (    "o",        "s", "ca" ),
        ( Good(X1(1)),   "za", 'a' ),
        ( Good(X1(2)),   "aa", 'a' ),
        ( Bad(One('b')), "aa", 'b' )
      )
    forAll (examples) { (o, s, ca) => gimme1(s, ca) shouldBe o }
  }

  it should "work with arity 2" in {
    val examples =
      Table(
        (    "o",        "s", "ca", "cb" ),
        ( Good(X2(1, 1)),   "zab", 'a', 'b' ),
        ( Good(X2(2, 1)),   "aab", 'a', 'b' ),
        ( Bad(One('b')),   "aaa", 'a', 'b' ),
        ( Bad(Every('b', 'c')), "aa", 'b', 'c')
      )
    forAll (examples) { (o, s, ca, cb) => gimme2(s, ca, cb) shouldBe o }
  }

  it should "work with arity 3" in {
    val examples =
      Table(
        (    "o",        "s", "ca", "cb", "cc" ),
        ( Good(X3(1, 1, 1)),   "zabc", 'a', 'b', 'c' ),
        ( Good(X3(2, 1, 1)),   "aabc", 'a', 'b', 'c' ),
        ( Bad(Every('b', 'c', 'd')), "aa", 'b', 'c', 'd')
      )
    forAll (examples) { (o, s, ca, cb, cc) => gimme3(s, ca, cb, cc) shouldBe o }
  }

  it should "work with arity 4" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "o"),
        ( "abcd",  'a',  'b',  'c',  'd', Good(X4(1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd', Bad(Every('a', 'b', 'c', 'd')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, o) =>
      gimme4(s, ca, cb, cc, cd) shouldBe o
    }
  }

  it should "work with arity 5" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "o"),
        ( "abcde",  'a',  'b',  'c',  'd',  'e', Good(X5(1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e', Bad(Every('a', 'b', 'c', 'd', 'e')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, o) =>
      gimme5(s, ca, cb, cc, cd, ce) shouldBe o
    }
  }


  it should "work with arity 6" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "o"),
        ( "abcdef",  'a',  'b',  'c',  'd',  'e',  'f', Good(X6(1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f', Bad(Every('a', 'b', 'c', 'd', 'e', 'f')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, o) =>
      gimme6(s, ca, cb, cc, cd, ce, cf) shouldBe o
    }
  }


  it should "work with arity 7" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "o"),
        ( "abcdefg",  'a',  'b',  'c',  'd',  'e',  'f',  'g', Good(X7(1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, o) =>
      gimme7(s, ca, cb, cc, cd, ce, cf, cg) shouldBe o
    }
  }


  it should "work with arity 8" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "o"),
        ( "abcdefgh",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h', Good(X8(1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, o) =>
      gimme8(s, ca, cb, cc, cd, ce, cf, cg, ch) shouldBe o
    }
  }


  it should "work with arity 9" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "o"),
        ( "abcdefghi",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i', Good(X9(1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, o) =>
      gimme9(s, ca, cb, cc, cd, ce, cf, cg, ch, ci) shouldBe o
    }
  }


  it should "work with arity 10" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "o"),
        ( "abcdefghij",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j', Good(X10(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, o) =>
      gimme10(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj) shouldBe o
    }
  }


  it should "work with arity 11" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "o"),
        ( "abcdefghijk",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k', Good(X11(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, o) =>
      gimme11(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck) shouldBe o
    }
  }


  it should "work with arity 12" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "o"),
        ( "abcdefghijkl",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l', Good(X12(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, o) =>
      gimme12(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl) shouldBe o
    }
  }


  it should "work with arity 13" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "o"),
        ( "abcdefghijklm",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm', Good(X13(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, o) =>
      gimme13(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm) shouldBe o
    }
  }


  it should "work with arity 14" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "o"),
        ( "abcdefghijklmn",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n', Good(X14(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, o) =>
      gimme14(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn) shouldBe o
    }
  }


  it should "work with arity 15" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "o"),
        ( "abcdefghijklmno",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o', Good(X15(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, o) =>
      gimme15(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co) shouldBe o
    }
  }


  it should "work with arity 16" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "cp", "o"),
        ( "abcdefghijklmnop",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p', Good(X16(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, o) =>
      gimme16(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp) shouldBe o
    }
  }


  it should "work with arity 17" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "cp", "cq", "o"),
        ( "abcdefghijklmnopq",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q', Good(X17(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, o) =>
      gimme17(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq) shouldBe o
    }
  }


  it should "work with arity 18" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "cp", "cq", "cr", "o"),
        ( "abcdefghijklmnopqr",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r', Good(X18(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, o) =>
      gimme18(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr) shouldBe o
    }
  }


  it should "work with arity 19" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "cp", "cq", "cr", "cs", "o"),
        ( "abcdefghijklmnopqrs",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r',  's', Good(X19(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r',  's', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, cs, o) =>
      gimme19(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, cs) shouldBe o
    }
  }


  it should "work with arity 20" in {
    val examples =
      Table(
        (    "s", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj", "ck", "cl", "cm", "cn", "co", "cp", "cq", "cr", "cs", "ct", "o"),
        ( "abcdefghijklmnopqrst",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r',  's',  't', Good(X20(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))),
        (    "z",  'a',  'b',  'c',  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',  'p',  'q',  'r',  's',  't', Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't')))
      )
    forAll (examples) { (s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, cs, ct, o) =>
      gimme20(s, ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, cs, ct) shouldBe o
    }
  }

  it should "work with arity 21" in {
    gimme21("abcdefghijklmnopqrstu", 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u') shouldBe Good(X21(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    gimme21("z", 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u') shouldBe Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u'))
  }


  it should "work with arity 22" in {
    gimme22("abcdefghijklmnopqrstuv", 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v') shouldBe Good(X22(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    gimme22("z", 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v') shouldBe Bad(Every('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v'))
  }
}
