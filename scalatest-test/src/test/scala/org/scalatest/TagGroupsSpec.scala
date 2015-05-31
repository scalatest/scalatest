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

import tagobjects._
import SharedHelpers._
import Matchers._

/*
This just tests a trait, TagGroups, that I created to answer a user query about
grouping tags. I probably won't put this into ScalaTest proper to keep tagging
simple, but I'll leave this in the repo as an example for folks who want to do
something like this. - bv
*/
class TagGroupsSpec extends FunSpec {

  trait TagGroups extends SuiteMixin { this: Suite =>

    private val groups: Map[String, Set[String]] =
      Map(
        "NonUnit" -> Set("org.scalatest.tags.Disk", "org.scalatest.tags.Network"),
        "Sluggish" -> Set("org.scalatest.tags.Slow", "org.scalatest.tags.Network")
      )

    abstract override def run(testName: Option[String], args: Args): Status = {

      def expandGroups(set: Set[String]): Set[String] = 
        set flatMap { tag => groups.getOrElse(tag, Set(tag)) }

      val newTagsToInclude: Option[Set[String]] =
        for (set <- args.filter.tagsToInclude) yield expandGroups(set)

      val newTagsToExclude: Set[String] = expandGroups(args.filter.tagsToExclude)

      val newFilter: Filter =
        Filter(
          newTagsToInclude,
          newTagsToExclude,
          args.filter.excludeNestedSuites,
          args.filter.dynaTags
        )

      super.run(testName, args.copy(filter = newFilter))
    }
  }

  class ExampleSpec extends FunSuite with TagGroups {
    test("slow", Slow) {
      1 + 1 shouldEqual 2
    }
    test("disk", Disk) {
      1 + 1 shouldEqual 2
    }
    test("network", Network) {
      1 + 1 shouldEqual 2
    }
    test("cpu", CPU) {
      1 + 1 shouldEqual 2
    }
  }

  describe("A Suite with TagGroups") {

    it("should run all tests if default Filter is used") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("slow", "disk", "network", "cpu")
    }

    it("should run non-excluded tests if Filter containing tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("org.scalatest.tags.Disk"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("slow", "network", "cpu")
    }

    it("should run included tests if Filter containing tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("org.scalatest.tags.Disk")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("disk")
    }

    it("should run non-excluded tests if Filter containing NonUnit grouped tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("NonUnit"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("slow", "cpu")
    }

    it("should run included tests if Filter containing NonUnit grouped tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("NonUnit")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("disk", "network")
    }

    it("should run non-excluded tests if Filter containing Sluggish grouped tags-to-exclude is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToExclude = Set("Sluggish"))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("disk", "cpu")
    }

    it("should run included tests if Filter containing Sluggish grouped tags-to-include is passed in") {
      val es = new ExampleSpec
      val rep = new EventRecordingReporter
      es.run(None, Args(rep, filter = Filter(tagsToInclude = Some(Set("Sluggish")))))
      rep.testSucceededEventsReceived.map(_.testName) shouldEqual Vector("slow", "network")
    }
  }
}

