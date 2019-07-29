/*
 * Copyright 2001-2019 Artima, Inc.
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

import org.scalatest.featurespec._
import org.scalatest.flatspec._
import org.scalatest.freespec._
import org.scalatest.funspec._
import org.scalatest.funsuite._
import org.scalatest.propspec._
import org.scalatest.refspec._
import org.scalatest.wordspec._

trait AsyncStringFixture { this: FixtureAsyncTestSuite =>
  type FixtureParam = String
  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    test("hi")
  }
}

class StyleNameSpec extends AnyFunSuite {

  test("AnyFeatureSpec and AnyFeatureSpecLike should return org.scalatest.featurespec.AnyFeatureSpec from styleName") {
    assert((new AnyFeatureSpec).styleName == "org.scalatest.featurespec.AnyFeatureSpec")
    assert((new AnyFeatureSpecLike {}).styleName == "org.scalatest.featurespec.AnyFeatureSpec")
  }

  test("FixtureAnyFeatureSpec and FixtureAnyFeatureSpecLike should return org.scalatest.featurespec.FixtureAnyFeatureSpec from styleName") {
    assert((new FixtureAnyFeatureSpec with StringFixture).styleName == "org.scalatest.featurespec.FixtureAnyFeatureSpec")
    assert((new FixtureAnyFeatureSpecLike with StringFixture {} ).styleName == "org.scalatest.featurespec.FixtureAnyFeatureSpec")
  }

  test("AsyncFeatureSpec and AsyncFeatureSpecLike should return org.scalatest.featurespec.AsyncFeatureSpec from styleName") {
    assert((new AsyncFeatureSpec {}).styleName == "org.scalatest.featurespec.AsyncFeatureSpec")
    assert((new AsyncFeatureSpecLike {}).styleName == "org.scalatest.featurespec.AsyncFeatureSpec")
  }

  test("FixtureAsyncFeatureSpec and FixtureAsyncFeatureSpecLike should return org.scalatest.featurespec.FixtureAsyncFeatureSpec from styleName") {
    assert((new FixtureAsyncFeatureSpec with AsyncStringFixture).styleName == "org.scalatest.featurespec.FixtureAsyncFeatureSpec")
    assert((new FixtureAsyncFeatureSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.featurespec.FixtureAsyncFeatureSpec")
  }

  test("AnyFlatSpec and AnyFlatSpecLike should return org.scalatest.flatspec.AnyFlatSpec from styleName") {
    assert((new AnyFlatSpec).styleName == "org.scalatest.flatspec.AnyFlatSpec")
    assert((new AnyFlatSpecLike {}).styleName == "org.scalatest.flatspec.AnyFlatSpec")
  }

  test("FixtureAnyFlatSpec and FixtureAnyFlatSpecLike should return org.scalatest.flatspec.FixtureAnyFlatSpec from styleName") {
    assert((new FixtureAnyFlatSpec with StringFixture).styleName == "org.scalatest.flatspec.FixtureAnyFlatSpec")
    assert((new FixtureAnyFlatSpecLike with StringFixture {} ).styleName == "org.scalatest.flatspec.FixtureAnyFlatSpec")
  }

  test("AsyncFlatSpec and AsyncFlatSpecLike should return org.scalatest.flatspec.AsyncFlatSpec from styleName") {
    assert((new AsyncFlatSpec {}).styleName == "org.scalatest.flatspec.AsyncFlatSpec")
    assert((new AsyncFlatSpecLike {}).styleName == "org.scalatest.flatspec.AsyncFlatSpec")
  }

  test("FixtureAsyncFlatSpec and FixtureAsyncFlatSpecLike should return org.scalatest.flatspec.FixtureAsyncFlatSpec from styleName") {
    assert((new FixtureAsyncFlatSpec with AsyncStringFixture).styleName == "org.scalatest.flatspec.FixtureAsyncFlatSpec")
    assert((new FixtureAsyncFlatSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.flatspec.FixtureAsyncFlatSpec")
  }

  test("AnyFreeSpec and AnyFreeSpecLike should return org.scalatest.freespec.AnyFreeSpec from styleName") {
    assert((new AnyFreeSpec).styleName == "org.scalatest.freespec.AnyFreeSpec")
    assert((new AnyFreeSpecLike {}).styleName == "org.scalatest.freespec.AnyFreeSpec")
  }

  test("FixtureAnyFreeSpec and FixtureAnyFreeSpecLike should return org.scalatest.freespec.FixtureAnyFreeSpec from styleName") {
    assert((new FixtureAnyFreeSpec with StringFixture).styleName == "org.scalatest.freespec.FixtureAnyFreeSpec")
    assert((new FixtureAnyFreeSpecLike with StringFixture {} ).styleName == "org.scalatest.freespec.FixtureAnyFreeSpec")
  }

  test("AsyncFreeSpec and AsyncFreeSpecLike should return org.scalatest.freespec.AsyncFreeSpec from styleName") {
    assert((new AsyncFreeSpec {}).styleName == "org.scalatest.freespec.AsyncFreeSpec")
    assert((new AsyncFreeSpecLike {}).styleName == "org.scalatest.freespec.AsyncFreeSpec")
  }

  test("FixtureAsyncFreeSpec and FixtureAsyncFreeSpecLike should return org.scalatest.freespec.FixtureAsyncFreeSpec from styleName") {
    assert((new FixtureAsyncFreeSpec with AsyncStringFixture).styleName == "org.scalatest.freespec.FixtureAsyncFreeSpec")
    assert((new FixtureAsyncFreeSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.freespec.FixtureAsyncFreeSpec")
  }

  test("PathAnyFreeSpec and PathAnyFreeSpecLike should return org.scalatest.freespec.PathAnyFreeSpec from styleName") {
    assert((new PathAnyFreeSpec).styleName == "org.scalatest.freespec.PathAnyFreeSpec")
    assert((new PathAnyFreeSpecLike {}).styleName == "org.scalatest.freespec.PathAnyFreeSpec")
  }

  test("AnyFunSpec and AnyFunSpecLike should return org.scalatest.funspec.AnyFunSpec from styleName") {
    assert((new AnyFunSpec).styleName == "org.scalatest.funspec.AnyFunSpec")
    assert((new AnyFunSpecLike {}).styleName == "org.scalatest.funspec.AnyFunSpec")
  }

  test("FixtureAnyFunSpec and FixtureAnyFunSpecLike should return org.scalatest.funspec.FixtureAnyFunSpec from styleName") {
    assert((new FixtureAnyFunSpec with StringFixture).styleName == "org.scalatest.funspec.FixtureAnyFunSpec")
    assert((new FixtureAnyFunSpecLike with StringFixture {} ).styleName == "org.scalatest.funspec.FixtureAnyFunSpec")
  }

  test("AsyncFunSpec and AsyncFunSpecLike should return org.scalatest.funspec.AsyncFunSpec from styleName") {
    assert((new AsyncFunSpec {}).styleName == "org.scalatest.funspec.AsyncFunSpec")
    assert((new AsyncFunSpecLike {}).styleName == "org.scalatest.funspec.AsyncFunSpec")
  }

  test("PathAnyFunSpec and PathAnyFunSpecLike should return org.scalatest.funspec.PathAnyFunSpec from styleName") {
    assert((new PathAnyFunSpec).styleName == "org.scalatest.funspec.PathAnyFunSpec")
    assert((new PathAnyFunSpecLike {}).styleName == "org.scalatest.funspec.PathAnyFunSpec")
  }

  test("FixtureAsyncFunSpec and FixtureAsyncFunSpecLike should return org.scalatest.funspec.FixtureAsyncFunSpec from styleName") {
    assert((new FixtureAsyncFunSpec with AsyncStringFixture).styleName == "org.scalatest.funspec.FixtureAsyncFunSpec")
    assert((new FixtureAsyncFunSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.funspec.FixtureAsyncFunSpec")
  }

  test("AnyFunSuite and AnyFunSuiteLike should return org.scalatest.funsuite.AnyFunSuite from styleName") {
    assert((new AnyFunSuite).styleName == "org.scalatest.funsuite.AnyFunSuite")
    assert((new AnyFunSuiteLike {}).styleName == "org.scalatest.funsuite.AnyFunSuite")
  }

  test("FixtureAnyFunSuite and FixtureAnyFunSuiteLike should return org.scalatest.funsuite.FixtureAnyFunSuite from styleName") {
    assert((new FixtureAnyFunSuite with StringFixture).styleName == "org.scalatest.funsuite.FixtureAnyFunSuite")
    assert((new FixtureAnyFunSuiteLike with StringFixture {} ).styleName == "org.scalatest.funsuite.FixtureAnyFunSuite")
  }

  test("AsyncFunSuite and AsyncFunSuiteLike should return org.scalatest.funsuite.AsyncFunSuite from styleName") {
    assert((new AsyncFunSuite {}).styleName == "org.scalatest.funsuite.AsyncFunSuite")
    assert((new AsyncFunSuiteLike {}).styleName == "org.scalatest.funsuite.AsyncFunSuite")
  }

  test("FixtureAsyncFunSuite and FixtureAsyncFunSuiteLike should return org.scalatest.funsuite.FixtureAsyncFunSuite from styleName") {
    assert((new FixtureAsyncFunSuite with AsyncStringFixture).styleName == "org.scalatest.funsuite.FixtureAsyncFunSuite")
    assert((new FixtureAsyncFunSuiteLike with AsyncStringFixture {}).styleName == "org.scalatest.funsuite.FixtureAsyncFunSuite")
  }

  test("AnyPropSpec and AnyPropSpecLike should return org.scalatest.propspec.AnyPropSpec from styleName") {
    assert((new AnyPropSpec).styleName == "org.scalatest.propspec.AnyPropSpec")
    assert((new AnyPropSpecLike {}).styleName == "org.scalatest.propspec.AnyPropSpec")
  }

  test("RefSpec and RefSpecLike should return org.scalatest.refspec.RefSpec from styleName") {
    assert((new RefSpec).styleName == "org.scalatest.refspec.RefSpec")
    assert((new RefSpecLike {}).styleName == "org.scalatest.refspec.RefSpec")
  }

  test("FixtureAnyPropSpec and FixtureAnyPropSpecLike should return org.scalatest.propspec.FixtureAnyPropSpec from styleName") {
    assert((new FixtureAnyPropSpec with StringFixture).styleName == "org.scalatest.propspec.FixtureAnyPropSpec")
    assert((new FixtureAnyPropSpecLike with StringFixture {} ).styleName == "org.scalatest.propspec.FixtureAnyPropSpec")
  }

/* No AsyncPropSpec's until we release our Generator. But at that time, should add these in.
  test("AsyncPropSpec and AsyncPropSpecLike should return org.scalatest.propspec.AsyncPropSpec from styleName") {
    assert((new AsyncPropSpec {}).styleName == "org.scalatest.propspec.AsyncPropSpec")
    assert((new AsyncPropSpecLike {}).styleName == "org.scalatest.propspec.AsyncPropSpec")
  }

  test("FixtureAsyncPropSpec and FixtureAsyncPropSpecLike should return org.scalatest.propspec.FixtureAsyncPropSpec from styleName") {
    assert((new FixtureAsyncPropSpec with AsyncStringFixture).styleName == "org.scalatest.propspec.FixtureAsyncPropSpec")
    assert((new FixtureAsyncPropSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.propspec.FixtureAsyncPropSpec")
  }
*/

  test("AnyWordSpec and AnyWordSpecLike should return org.scalatest.wordspec.AnyWordSpec from styleName") {
    assert((new AnyWordSpec).styleName == "org.scalatest.wordspec.AnyWordSpec")
    assert((new AnyWordSpecLike {}).styleName == "org.scalatest.wordspec.AnyWordSpec")
  }

  test("FixtureAnyWordSpec and FixtureAnyWordSpecLike should return org.scalatest.wordspec.FixtureAnyWordSpec from styleName") {
    assert((new FixtureAnyWordSpec with StringFixture).styleName == "org.scalatest.wordspec.FixtureAnyWordSpec")
    assert((new FixtureAnyWordSpecLike with StringFixture {} ).styleName == "org.scalatest.wordspec.FixtureAnyWordSpec")
  }

  test("AsyncWordSpec and AsyncWordSpecLike should return org.scalatest.wordspec.AsyncWordSpec from styleName") {
    assert((new AsyncWordSpec {}).styleName == "org.scalatest.wordspec.AsyncWordSpec")
    assert((new AsyncWordSpecLike {}).styleName == "org.scalatest.wordspec.AsyncWordSpec")
  }

  test("FixtureAsyncWordSpec and FixtureAsyncWordSpecLike should return org.scalatest.wordspec.FixtureAsyncWordSpec from styleName") {
    assert((new FixtureAsyncWordSpec with AsyncStringFixture).styleName == "org.scalatest.wordspec.FixtureAsyncWordSpec")
    assert((new FixtureAsyncWordSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.wordspec.FixtureAsyncWordSpec")
  }
}

