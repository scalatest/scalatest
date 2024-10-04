/*
 * Copyright 2001-2024 Artima, Inc.
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

import scala.util.parsing.combinator._

object JSON extends JavaTokenParsers {

  def toJsVal(v: Any): JsVal =
    v match {
      case j: JsVal => j
      case m: Map[String, JsVal] => JsObj(m)
      case l: List[JsVal] => JsArr(l)
      case s: String => JsStr(s.take(s.length - 1).drop(1))
      case n: Double => JsNum(n)
      case false => JsFalse
      case true => JsTrue
      case null => JsNULL
    }

  sealed trait JsVal extends Any {
    def value: Any
    def \(key: String): Option[JsVal] = this.asInstanceOf[JsObj].value.get(key)
  }
  case class JsStr(value: String) extends AnyVal with JsVal
  case class JsObj(value: Map[String, JsVal]) extends AnyVal with JsVal
  case class JsArr(value: List[JsVal]) extends AnyVal with JsVal
  case class JsNum(value: Double) extends AnyVal with JsVal
  case object JsFalse extends JsVal {
    def value = false
  }
  case object JsTrue extends JsVal {
    def value = true
  }
  case object JsNULL extends JsVal {
    def value = null
  }

  def obj: Parser[JsObj] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (v => JsObj(Map.empty[String, JsVal] ++ v))
  def arr: Parser[List[JsVal]] =
    "[" ~> repsep(value, ",") <~ "]" ^^ (v => v.map(toJsVal))
  def member: Parser[(String, JsVal)] =
    stringLiteral ~ ":" ~ value ^^
      { case name ~ ":" ~ value => (name.take(name.length - 1).drop(1), toJsVal(value)) }
  def value: Parser[Any] = (
    obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )

  def parseJson(jsStr: String) = parseAll(value, jsStr).get.asInstanceOf[JsVal]
}