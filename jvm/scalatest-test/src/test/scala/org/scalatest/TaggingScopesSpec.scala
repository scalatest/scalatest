/*
 * Copyright 2001-2025 Artima, Inc.
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

import tagobjects._
import SharedHelpers._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

/*
This just tests a trait, TagGroups, that I created to answer a user query about
tagging scopes. I probably won't put this into ScalaTest proper to keep tagging
simple, but I'll leave this in the repo as an example for folks who want to do
something like this. - bv
*/
class TaggingScopesSpec extends AnyFunSpec {

  class ExampleSpec extends AnyFreeSpec {

    override lazy val tags: Map[String, Set[String]] =
      super.tags.transform {
        case (testName, tagsSet) if testName startsWith "scope1" => tagsSet + "Scope1"
        case (testName, tagsSet) if testName startsWith "scope2" => tagsSet + "Scope2"
        case (_, existingSet) => existingSet
      }

    "scope1" - {
      "slow" taggedAs Slow in {
        1 + 1 shouldEqual 2
      }
      "disk" taggedAs Disk in {
        1 + 1 shouldEqual 2
      }
    }
    "scope2" - {
      "network" taggedAs Network in {
        1 + 1 shouldEqual 2
      }
      "cpu" taggedAs CPU in {
        1 + 1 shouldEqual 2
      }
    }
  }

  describe("A Suite with TagGroups") {

    it("should run all tests if default Filter is used") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope1 slow", "scope1 disk", "scope2 network", "scope2 cpu")
    }

    it("should run non-excluded tests if Filter containing tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("org.scalatest.tags.Disk"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope1 slow", "scope2 network", "scope2 cpu")
    }

    it("should run included tests if Filter containing tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("org.scalatest.tags.Disk")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope1 disk")
    }

    it("should run non-excluded tests if Filter containing Scope1 tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("Scope1"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope2 network", "scope2 cpu")
    }

    it("should run included tests if Filter containing Scope1 tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("Scope1")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope1 slow", "scope1 disk")
    }

    it("should run non-excluded tests if Filter containing Scope2 tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("Scope2"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope1 slow", "scope1 disk")
    }

    it("should run included tests if Filter containing Scope2 tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("Scope2")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("scope2 network", "scope2 cpu")
    }
  }
}


