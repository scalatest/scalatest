/*
 * Copyright 2001-2011 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import DoOver.tryTryAgain

class DoOverSuite extends FunSuite with ShouldMatchers {

  test("passing a maxTries less than 3 generates an IllegalArgumentException") {
    intercept[IllegalArgumentException] {
      tryTryAgain(0) {}
    }
    intercept[IllegalArgumentException] {
      tryTryAgain(-1) {}
    }
    intercept[IllegalArgumentException] {
      tryTryAgain(-2) {}
    }
    intercept[IllegalArgumentException] {
      tryTryAgain(-333) {}
    }
  }

  class CouldNotGetThereException extends Exception

  class UnreliableFun(private var successList: List[Boolean]) extends Function0[Unit] {
    var invocationCount = 0
    def apply() {
      invocationCount += 1
      val shouldSucceed = successList.head
      successList = successList.tail
      if (!shouldSucceed) throw new CouldNotGetThereException
    }
  }

  test("Succeeds if majority succeeds") {

    tryTryAgain(1) {} // Need one true
    tryTryAgain(2) {} // Need one true

    // Passing in true true, because it should stop trying after two true's
    var unFun = new UnreliableFun(List(true, true)) // Need two trues
    tryTryAgain(3) { unFun() }

    unFun = new UnreliableFun(List(false, true, true)) // Need two trues
    tryTryAgain(3) { unFun() }

    unFun = new UnreliableFun(List(true, true)) // Need two trues
    tryTryAgain(4) { unFun() }

    unFun = new UnreliableFun(List(false, true, true)) // Need two trues
    tryTryAgain(4) { unFun() }

    // 111
    unFun = new UnreliableFun(List(true, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 1101
    unFun = new UnreliableFun(List(true, true, false, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 1011
    unFun = new UnreliableFun(List(true, false, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 0111
    unFun = new UnreliableFun(List(false, true, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 01011
    unFun = new UnreliableFun(List(false, true, false, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 01101
    unFun = new UnreliableFun(List(false, true, true, false, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 00111
    unFun = new UnreliableFun(List(false, false, true, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 11001
    unFun = new UnreliableFun(List(true, true, false, false, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 10101
    unFun = new UnreliableFun(List(true, false, true, false, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 10011
    unFun = new UnreliableFun(List(true, false, false, true, true)) // Need three trues
    tryTryAgain(5) { unFun() }

    // 111
    unFun = new UnreliableFun(List(true, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 1101
    unFun = new UnreliableFun(List(true, true, false, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 1011
    unFun = new UnreliableFun(List(true, false, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 0111
    unFun = new UnreliableFun(List(false, true, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 01011
    unFun = new UnreliableFun(List(false, true, false, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 01101
    unFun = new UnreliableFun(List(false, true, true, false, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 00111
    unFun = new UnreliableFun(List(false, false, true, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 11001
    unFun = new UnreliableFun(List(true, true, false, false, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 10101
    unFun = new UnreliableFun(List(true, false, true, false, true)) // Need three trues
    tryTryAgain(6) { unFun() }

    // 10011
    unFun = new UnreliableFun(List(true, false, false, true, true)) // Need three trues
    tryTryAgain(6) { unFun() }
  }

  test("Fails if majority fails") {

    var unFun = new UnreliableFun(List(false)) // Need one false
    intercept[CouldNotGetThereException] {
      tryTryAgain(1) { unFun() }
    }

    unFun = new UnreliableFun(List(false)) // Need one false
    intercept[CouldNotGetThereException] {
      tryTryAgain(2) { unFun() }
    }

    unFun = new UnreliableFun(List(false, false)) // Need two falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(3) { unFun() }
    }

    unFun = new UnreliableFun(List(true, false, false)) // Need two falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(3) { unFun() }
    }

    unFun = new UnreliableFun(List(false, false)) // Need two falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(4) { unFun() }
    }

    unFun = new UnreliableFun(List(true, false, false)) // Need two falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(4) { unFun() }
    }

    // 000
    unFun = new UnreliableFun(List(false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 0010
    unFun = new UnreliableFun(List(false, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 00110
    unFun = new UnreliableFun(List(false, false, true, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 0100
    unFun = new UnreliableFun(List(false, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 01010
    unFun = new UnreliableFun(List(false, true, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 01100
    unFun = new UnreliableFun(List(false, true, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 1000
    unFun = new UnreliableFun(List(true, false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 10010
    unFun = new UnreliableFun(List(true, false, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 10100
    unFun = new UnreliableFun(List(true, false, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 11000
    unFun = new UnreliableFun(List(true, true, false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(5) { unFun() }
    }

    // 000
    unFun = new UnreliableFun(List(false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 0010
    unFun = new UnreliableFun(List(false, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 00110
    unFun = new UnreliableFun(List(false, false, true, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 0100
    unFun = new UnreliableFun(List(false, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 01010
    unFun = new UnreliableFun(List(false, true, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 01100
    unFun = new UnreliableFun(List(false, true, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 1000
    unFun = new UnreliableFun(List(true, false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 10010
    unFun = new UnreliableFun(List(true, false, false, true, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 10100
    unFun = new UnreliableFun(List(true, false, true, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }

    // 11000
    unFun = new UnreliableFun(List(true, true, false, false, false)) // Need three falses
    intercept[CouldNotGetThereException] {
      tryTryAgain(6) { unFun() }
    }
  }
}
