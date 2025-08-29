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

import java.io.{Closeable, IOException}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import SharedHelpers.EventRecordingReporter

// Mock closeable resource for testing
class MockResource(val name: String, tracker: MockResourceTracker) extends Closeable {
  var closed = false
  var accessed = false
  
  tracker.add(this)
  
  def access(): String = {
    accessed = true
    s"Accessed $name"
  }
  
  override def close(): Unit = {
    closed = true
  }
}

// Companion object to track resource creation
class MockResourceTracker {
  val resources = collection.mutable.ArrayBuffer.empty[MockResource]
  def add(resource: MockResource): Unit = resources += resource
  def instancesCreated: Int = resources.length
  def find(predicate: MockResource => Boolean): Option[MockResource] = resources.find(predicate)
}

class ResourceManagerSpec extends AnyFunSpec with Matchers {
  
  class TestSpec extends AnyFunSpec with ResourceManager {
    val resourceTracker = new MockResourceTracker
    lazy val sharedClient = suiteScoped(new MockResource("shared-client", resourceTracker))
    
    it("should allow suite-scoped resources") {
      sharedClient.access() should be("Accessed shared-client")
      sharedClient.closed should be(false) // Should not be closed yet within the test

      val suiteResource = suiteScoped(new MockResource("suite-resource", resourceTracker))
      suiteResource.access() should be("Accessed suite-resource")
      suiteResource.closed should be(false) // Should not be closed yet within the test 
    }
    
    it("should not close resources after first test completion") {
      sharedClient.closed should be (false)
      // Verify the test-specific resource from previous test was closed
      resourceTracker.find(_.name == "suite-resource").map(_.closed) should be (Some(false))
    }
  }

  class FailedTestSpec extends AnyFunSpec with ResourceManager {
    // This should throw an exception when accessed outside a test
    val shouldFail = suiteScoped
  }

  // Create a class with lazy resources to test
  class LazyResourceTest extends AnyFunSpec with ResourceManager with Matchers {
    val resourceTracker = new MockResourceTracker
    // Lazy val that depends on suiteScoped but won't be initialized until accessed
    lazy val lazyResource = suiteScoped(new MockResource("lazy-resource", resourceTracker))
    
    // Eager val that will be initialized immediately
    val eagerResource = new MockResource("eager-resource", resourceTracker)
    
    it("should not initialize lazy resources unless accessed") {
      // At this point, instancesCreated should be 1 (just the eager resource)
      resourceTracker.instancesCreated should be (1)
      
      // Now access the lazy resource, which should trigger initialization
      lazyResource.access() should be("Accessed lazy-resource")
      
      // Now instances created should be 2
      resourceTracker.instancesCreated should be(2)
    }
    
    it("should initialize lazy resources only once") {
      // Accessing the lazy resource multiple times should not create new instances
      lazyResource.access()
      lazyResource.access()
      
      // Still only 2 instances total
      resourceTracker.instancesCreated should be(2)
    }
  }

  describe("ResourceManager") {
    
    it("should provide a suite-scoped Using.Manager") {
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      suite.sharedClient.closed should be (true)
      suite.resourceTracker.find(_.name == "suite-resource").map(_.closed) should be (Some(true))
      suite.resourceTracker.instancesCreated should be (2)
      reporter.testSucceededEventsReceived.length should be (2)
    }
    
    it("should throw IllegalStateException if called outside of test execution") {
      val e = 
        intercept[IllegalStateException] {
          val failed = new FailedTestSpec
        }
      e.getMessage should include("`suiteScoped`` cannot be called from outside a test")
    }

    it("should not initialize lazy resources until accessed") {
      // Create an instance of the nested test suite
      val lazyResourceTest = new LazyResourceTest
      val reporter = new EventRecordingReporter
      lazyResourceTest.run(None, Args(reporter))
      reporter.testSucceededEventsReceived.size should be (2)
    }
  }

}