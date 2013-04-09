/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestRegistrationClosedException

class ShouldBehaveLikeSpec extends FunSpec {

  def myFirstBehavior(i: Int) {
    it("This one is should blow up") {}
  }

  describe("The 'should behave like' syntax should throw an exception inside an it clause") {
    it("the code in here should fail with an exception") {
      intercept[TestRegistrationClosedException] {
        it should behave like myFirstBehavior(1) 
      }
    }
  }

  // Checking for a specific size
/*
  describe("The 'should behave like' syntax should work in a describe") {

    it should behave like nonEmptyStack(lastValuePushed)(stackWithOneItem) 

    describe(", and in a nested describe") {

      it should behave like nonEmptyStack(lastValuePushed)(stackWithOneItem) 
    }
  }
*/

  def myBehavior(i: Int) {
    it("This one is solo") {}
  }
  it should behave like myBehavior(1) 

  // TODO: Make these into real tests. I looked at it and heck they work. So I can indeed put describe clauses in
  // the shared behaviors. Cool.
  def myNestedBehavior(i: Int) {
    describe("and this is the shared describe") {
      it("This one is nested") {}
    }
  }

  it should behave like myNestedBehavior(1) 
  describe("And outer describe...") {
    it should behave like myNestedBehavior(1) 
  }

/* Correct, none of these compiled
  describe("'should not behave like' should not compile") {
    
    stackWithOneItem should not behave like (nonEmptyStack(lastValuePushed))
  }
  describe("The 'should behave like' syntax in an and or or clause, with or without not, should not compile") {
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) or behave like (nonFullStack))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) or (behave like (nonFullStack)))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) or not behave like (nonFullStack))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) or (not behave like (nonFullStack)))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) and behave like (nonFullStack))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) and (behave like (nonFullStack)))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) and not behave like (nonFullStack))
    stackWithOneItem should (behave like (nonEmptyStack(lastValuePushed)) and (not behave like (nonFullStack)))
  }
*/
}
