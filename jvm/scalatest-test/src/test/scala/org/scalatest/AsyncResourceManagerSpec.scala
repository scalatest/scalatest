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
import scala.concurrent.Future
import org.scalatest.funspec.{AnyFunSpec, AsyncFunSpec}
import org.scalatest.matchers.should.Matchers
import SharedHelpers.EventRecordingReporter

class AsyncResourceManagerSpec extends AnyFunSpec with Matchers {
  
  class TestSpec extends AsyncFunSpec with AsyncResourceManager {
    val resourceTracker = new MockResourceTracker
    lazy val sharedClient = suiteScoped(new MockResource("shared-client", resourceTracker))
    
    it("should allow suite-scoped resources (code in future block)") {
      Future {
        sharedClient.access() should be("Accessed shared-client")
        sharedClient.closed should be(false) // Should not be closed yet within the test

        val suiteResource = suiteScoped(new MockResource("suite-resource", resourceTracker))
        suiteResource.access() should be("Accessed suite-resource")
        suiteResource.closed should be(false) // Should not be closed yet within the test 
        succeed
      }
    }
    it("should allow suite-scoped resources (code before future block)") {
      sharedClient.access() should be("Accessed shared-client")
      sharedClient.closed should be(false) // Should not be closed yet within the test

      val suiteResource = suiteScoped(new MockResource("suite-resource-2", resourceTracker))
      suiteResource.access() should be("Accessed suite-resource-2")
      suiteResource.closed should be(false) // Should not be closed yet within the test 
      Future.successful(succeed)
    }
    
    it("should not close resources after first test completion (code in future block)") {
      Future {
        sharedClient.closed should be (false)
        // Verify the test-specific resource from previous test was closed
        resourceTracker.find(_.name == "suite-resource").map(_.closed) should be (Some(false))
        succeed
      }
    }

    it("should not close resources after first test completion (code before future block)") {
      sharedClient.closed should be (false)
      // Verify the test-specific resource from previous test was closed
      resourceTracker.find(_.name == "suite-resource-2").map(_.closed) should be (Some(false))
      Future.successful(succeed)
    }
  }

  class FailedTestSpec extends AsyncFunSpec with AsyncResourceManager {
    // This should throw an exception when accessed outside a test
    val shouldFail = suiteScoped
  }

  // Create a class with lazy resources to test
  class LazyResourceTest extends AsyncFunSpec with AsyncResourceManager with Matchers {
    val resourceTracker = new MockResourceTracker
    // Lazy val that depends on suiteScoped but won't be initialized until accessed
    lazy val lazyResource = suiteScoped(new MockResource("lazy-resource", resourceTracker))
    lazy val lazyResource2 = suiteScoped(new MockResource("lazy-resource-2", resourceTracker))
    
    // Eager val that will be initialized immediately
    val eagerResource = new MockResource("eager-resource", resourceTracker)
    
    it("should not initialize lazy resources unless accessed (code in future block)") {
      Future {
        // At this point, instancesCreated should be 1 (just the eager resource)
        resourceTracker.instancesCreated should be (1)
        // Now access the lazy resource, which should trigger initialization
        lazyResource.access() should be("Accessed lazy-resource")
        // Now instances created should be 2
        resourceTracker.instancesCreated should be(2)
        succeed
      }
    }

    it("should not initialize lazy resources unless accessed (code before future block)") {
      // At this point, instancesCreated should be 2 (from previous test)
      resourceTracker.instancesCreated should be (2)
      // Now access the lazy resource, which should trigger initialization
      lazyResource2.access() should be("Accessed lazy-resource-2")
      // Now instances created should be 2
      resourceTracker.instancesCreated should be(3)
      Future.successful(succeed)
    }
    
    it("should initialize lazy resources only once (code in future block)") {
      Future {
        // Accessing the lazy resource multiple times should not create new instances
        lazyResource.access()
        lazyResource.access()
        
        // Still only 2 instances total
        resourceTracker.instancesCreated should be(3)
        succeed
      }
    }

    it("should initialize lazy resources only once (code before future block)") {
      // Accessing the lazy resource multiple times should not create new instances
      lazyResource2.access()
      lazyResource2.access()
      
      // Still only 2 instances total
      resourceTracker.instancesCreated should be(3)
      Future.successful(succeed)
    }
  }

  describe("ResourceManager") {
    
    it("should provide a suite-scoped Using.Manager") {
      val suite = new TestSpec
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      suite.sharedClient.closed should be (true)
      suite.resourceTracker.find(_.name == "suite-resource").map(_.closed) should be (Some(true))
      suite.resourceTracker.instancesCreated should be (3)
      reporter.testSucceededEventsReceived.length should be (4)
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
      reporter.testSucceededEventsReceived.size should be (4)
    }
  }

}