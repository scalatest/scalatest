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
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatest.refspec._
// SKIP-SCALATESTJS,NATIVE-END
import org.scalatest.wordspec._

// SKIP-SCALATESTJS,NATIVE-START
trait AsyncStringFixture { this: FixtureAsyncTestSuite =>
  type FixtureParam = String
  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    test("hi")
  }
}
// SKIP-SCALATESTJS,NATIVE-END

class StyleNameSpec extends AnyFunSuite {

  test("AnyFeatureSpec and AnyFeatureSpecLike should return org.scalatest.FeatureSpec from styleName") {
    assert((new AnyFeatureSpec).styleName == "org.scalatest.FeatureSpec")
    assert((new AnyFeatureSpecLike {}).styleName == "org.scalatest.FeatureSpec")
  }

  test("FixtureAnyFeatureSpec and FixtureAnyFeatureSpecLike should return org.scalatest.fixture.FeatureSpec from styleName") {
    assert((new FixtureAnyFeatureSpec with StringFixture).styleName == "org.scalatest.fixture.FeatureSpec")
    assert((new FixtureAnyFeatureSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.FeatureSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncFeatureSpec and AsyncFeatureSpecLike should return org.scalatest.FeatureSpec from styleName") {
    assert((new AsyncFeatureSpec {}).styleName == "org.scalatest.FeatureSpec")
    assert((new AsyncFeatureSpecLike {}).styleName == "org.scalatest.FeatureSpec")
  }

  test("FixtureAsyncFeatureSpec and FixtureAsyncFeatureSpecLike should return org.scalatest.fixture.FeatureSpec from styleName") {
    assert((new FixtureAsyncFeatureSpec with AsyncStringFixture).styleName == "org.scalatest.fixture.FeatureSpec")
    assert((new FixtureAsyncFeatureSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.FeatureSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("AnyFlatSpec and AnyFlatSpecLike should return org.scalatest.FlatSpec from styleName") {
    assert((new AnyFlatSpec).styleName == "org.scalatest.FlatSpec")
    assert((new AnyFlatSpecLike {}).styleName == "org.scalatest.FlatSpec")
  }

  test("FixtureAnyFlatSpec and FixtureAnyFlatSpecLike should return org.scalatest.fixture.FlatSpec from styleName") {
    assert((new FixtureAnyFlatSpec with StringFixture).styleName == "org.scalatest.fixture.FlatSpec")
    assert((new FixtureAnyFlatSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.FlatSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncFlatSpec and AsyncFlatSpecLike should return org.scalatest.FlatSpec from styleName") {
    assert((new AsyncFlatSpec {}).styleName == "org.scalatest.FlatSpec")
    assert((new AsyncFlatSpecLike {}).styleName == "org.scalatest.FlatSpec")
  }

  test("FixtureAsyncFlatSpec and FixtureAsyncFlatSpecLike should return org.scalatest.fixture.FlatSpec from styleName") {
    assert((new FixtureAsyncFlatSpec with AsyncStringFixture).styleName == "org.scalatest.fixture.FlatSpec")
    assert((new FixtureAsyncFlatSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.FlatSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("AnyFreeSpec and AnyFreeSpecLike should return org.scalatest.FreeSpec from styleName") {
    assert((new AnyFreeSpec).styleName == "org.scalatest.FreeSpec")
    assert((new AnyFreeSpecLike {}).styleName == "org.scalatest.FreeSpec")
  }

  test("FixtureAnyFreeSpec and FixtureAnyFreeSpecLike should return org.scalatest.fixture.FreeSpec from styleName") {
    assert((new FixtureAnyFreeSpec with StringFixture).styleName == "org.scalatest.fixture.FreeSpec")
    assert((new FixtureAnyFreeSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.FreeSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncFreeSpec and AsyncFreeSpecLike should return org.scalatest.FreeSpec from styleName") {
    assert((new AsyncFreeSpec {}).styleName == "org.scalatest.FreeSpec")
    assert((new AsyncFreeSpecLike {}).styleName == "org.scalatest.FreeSpec")
  }

  test("FixtureAsyncFreeSpec and FixtureAsyncFreeSpecLike should return org.scalatest.fixture.FreeSpec from styleName") {
    assert((new FixtureAsyncFreeSpec with AsyncStringFixture).styleName == "org.scalatest.fixture.FreeSpec")
    assert((new FixtureAsyncFreeSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.FreeSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("PathAnyFreeSpec and PathAnyFreeSpecLike should return org.scalatest.path.FreeSpec from styleName") {
    class MySpec extends PathAnyFreeSpec {
      //SCALATESTJS,NATIVE-ONLY override def newInstance: org.scalatest.freespec.PathAnyFreeSpec = new MySpec
    }
    assert((new MySpec).styleName == "org.scalatest.path.FreeSpec")
    class MySpecLike extends PathAnyFreeSpecLike {
      //SCALATESTJS,NATIVE-ONLY override def newInstance: org.scalatest.freespec.PathAnyFreeSpecLike = new MySpecLike
    }
    assert((new MySpecLike).styleName == "org.scalatest.path.FreeSpec")
  }

  test("AnyFunSpec and AnyFunSpecLike should return org.scalatest.FunSpec from styleName") {
    assert((new AnyFunSpec).styleName == "org.scalatest.FunSpec")
    assert((new AnyFunSpecLike {}).styleName == "org.scalatest.FunSpec")
  }

  test("FixtureAnyFunSpec and FixtureAnyFunSpecLike should return org.scalatest.fixture.FunSpec from styleName") {
    assert((new FixtureAnyFunSpec with StringFixture).styleName == "org.scalatest.fixture.FunSpec")
    assert((new FixtureAnyFunSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.FunSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncFunSpec and AsyncFunSpecLike should return org.scalatest.FunSpec from styleName") {
    assert((new AsyncFunSpec {}).styleName == "org.scalatest.FunSpec")
    assert((new AsyncFunSpecLike {}).styleName == "org.scalatest.FunSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("PathAnyFunSpec and PathAnyFunSpecLike should return org.scalatest.path.FunSpec from styleName") {
    class MySpec extends PathAnyFunSpec {
      //SCALATESTJS,NATIVE-ONLY override def newInstance: org.scalatest.funspec.PathAnyFunSpec = new MySpec
    }
    assert((new MySpec).styleName == "org.scalatest.path.FunSpec")
    class MySpecLike extends PathAnyFunSpecLike {
      //SCALATESTJS,NATIVE-ONLY override def newInstance: org.scalatest.funspec.PathAnyFunSpecLike = new MySpecLike
    }
    assert((new MySpecLike).styleName == "org.scalatest.path.FunSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("FixtureAsyncFunSpec and FixtureAsyncFunSpecLike should return org.scalatest.fixture.FunSpec from styleName") {
    assert((new FixtureAsyncFunSpec with AsyncStringFixture).styleName == "org.scalatest.fixture.FunSpec")
    assert((new FixtureAsyncFunSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.FunSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("AnyFunSuite and AnyFunSuiteLike should return org.scalatest.FunSuite from styleName") {
    assert((new AnyFunSuite).styleName == "org.scalatest.FunSuite")
    assert((new AnyFunSuiteLike {}).styleName == "org.scalatest.FunSuite")
  }

  test("FixtureAnyFunSuite and FixtureAnyFunSuiteLike should return org.scalatest.fixture.FunSuite from styleName") {
    assert((new FixtureAnyFunSuite with StringFixture).styleName == "org.scalatest.fixture.FunSuite")
    assert((new FixtureAnyFunSuiteLike with StringFixture {} ).styleName == "org.scalatest.fixture.FunSuite")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncFunSuite and AsyncFunSuiteLike should return org.scalatest.FunSuite from styleName") {
    assert((new AsyncFunSuite {}).styleName == "org.scalatest.FunSuite")
    assert((new AsyncFunSuiteLike {}).styleName == "org.scalatest.FunSuite")
  }

  test("FixtureAsyncFunSuite and FixtureAsyncFunSuiteLike should return org.scalatest.fixture.FunSuite from styleName") {
    assert((new FixtureAsyncFunSuite with AsyncStringFixture).styleName == "org.scalatest.fixture.FunSuite")
    assert((new FixtureAsyncFunSuiteLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.FunSuite")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("AnyPropSpec and AnyPropSpecLike should return org.scalatest.PropSpec from styleName") {
    assert((new AnyPropSpec).styleName == "org.scalatest.PropSpec")
    assert((new AnyPropSpecLike {}).styleName == "org.scalatest.PropSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("RefSpec and RefSpecLike should return org.scalatest.refspec.RefSpec from styleName") {
    assert((new RefSpec).styleName == "org.scalatest.refspec.RefSpec")
    assert((new RefSpecLike {}).styleName == "org.scalatest.refspec.RefSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END

  test("FixtureAnyPropSpec and FixtureAnyPropSpecLike should return org.scalatest.fixture.PropSpec from styleName") {
    assert((new FixtureAnyPropSpec with StringFixture).styleName == "org.scalatest.fixture.PropSpec")
    assert((new FixtureAnyPropSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.PropSpec")
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

  test("AnyWordSpec and AnyWordSpecLike should return org.scalatest.WordSpec from styleName") {
    assert((new AnyWordSpec).styleName == "org.scalatest.WordSpec")
    assert((new AnyWordSpecLike {}).styleName == "org.scalatest.WordSpec")
  }

  test("FixtureAnyWordSpec and FixtureAnyWordSpecLike should return org.scalatest.fixture.WordSpec from styleName") {
    assert((new FixtureAnyWordSpec with StringFixture).styleName == "org.scalatest.fixture.WordSpec")
    assert((new FixtureAnyWordSpecLike with StringFixture {} ).styleName == "org.scalatest.fixture.WordSpec")
  }

  // SKIP-SCALATESTJS,NATIVE-START
  test("AsyncWordSpec and AsyncWordSpecLike should return org.scalatest.WordSpec from styleName") {
    assert((new AsyncWordSpec {}).styleName == "org.scalatest.WordSpec")
    assert((new AsyncWordSpecLike {}).styleName == "org.scalatest.WordSpec")
  }

  test("FixtureAsyncWordSpec and FixtureAsyncWordSpecLike should return org.scalatest.fixture.WordSpec from styleName") {
    assert((new FixtureAsyncWordSpec with AsyncStringFixture).styleName == "org.scalatest.fixture.WordSpec")
    assert((new FixtureAsyncWordSpecLike with AsyncStringFixture {}).styleName == "org.scalatest.fixture.WordSpec")
  }
  // SKIP-SCALATESTJS,NATIVE-END
}

