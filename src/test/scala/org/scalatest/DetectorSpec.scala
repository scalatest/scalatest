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
package org.scalatest

class DetectorSpec extends UnitSpec {
  object `A Detector` {
    class DBAccessException(message: String) extends RuntimeException(message)
    class OtherException extends RuntimeException
    val msg500 = "500: Internal Server Error"
    val msg404 = "404: Page Not Found"
    def `should throw NPE if the given partial function is null` {
      a [NullPointerException] should be thrownBy {
        Detector { null }
      }
      a [NullPointerException] should be thrownBy {
        new Detector(null)
      }
    }
    def `should be usable as an extractor for catching exceptions` {
      val InternalServerError = Detector { case dbae: DBAccessException => dbae.getMessage == "500: Internal Server Error" }
      try throw new DBAccessException(msg500) 
      catch {
        case InternalServerError(e) =>
        case _: Throwable => fail()
      }
      try throw new DBAccessException(msg404) 
      catch {
        case InternalServerError(e) => fail()
        case _: Throwable =>
      }
      try throw new OtherException
      catch {
        case InternalServerError(e) => fail()
        case _: Throwable =>
      }
    }
    def `should be usable as an extractor for detecting exceptions in Failed outcomes in withFixtures` {
      val InternalServerError = Detector { case dbae: DBAccessException => dbae.getMessage == "500: Internal Server Error" }
      val outcome1: Outcome = Failed(new DBAccessException(msg500))
      outcome1 match {
        case Failed(InternalServerError(e)) =>
        case _ => fail()
      }
      val outcome2: Outcome = Failed(new DBAccessException(msg404))
      outcome2 match {
        case Failed(InternalServerError(e)) => fail()
        case _ =>
      }
      val outcome3: Outcome = Failed(new OtherException)
      outcome3 match {
        case Failed(InternalServerError(e)) => fail()
        case _ =>
      }
      val outcome4: Outcome = Succeeded
      outcome4 match {
        case Failed(InternalServerError(e)) => fail()
        case _ =>
      }
    }
  }
}

