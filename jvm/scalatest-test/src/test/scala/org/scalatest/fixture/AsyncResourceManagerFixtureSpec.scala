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
package org.scalatest.fixture

import org.scalactic.Using
import scala.util.{Success, Failure}
import scala.concurrent.Future
import java.io.{Closeable, IOException}
import org.scalatest._
import org.scalatest.funspec.{AnyFunSpec, FixtureAsyncFunSpec}
import org.scalatest.matchers.should.Matchers
import SharedHelpers.EventRecordingReporter

// Test implementation of AsyncResourceManagerFixture
class AsyncResourceManagerFixtureSpec extends AnyFunSpec with Matchers {
  
  // Test that Using.Manager is properly provided to tests
  describe("ResourceManagerFixture") {

    // A variable to track resource creation/closing across tests
    val resourceTracker = collection.mutable.ArrayBuffer.empty[MockResource]

    class TestSpec extends FixtureAsyncFunSpec with AsyncResourceManagerFixture with Matchers {

      val resourceTracker = new MockResourceTracker

      it("should provide a Using.Manager to each test, and resource is created in future block") { use =>
        Future {
          // Verify manager is provided and functional
          use should not be null
          
          // Test resource acquisition with the manager
          val resource = use(new MockResource("test-resource", resourceTracker))
          resource.access() should be("Accessed test-resource")
          resource.closed should be(false) // Should not be closed yet within the test
          
          succeed
        }
      }

      it("should provide a Using.Manager to each test, and resource is created before future block") { use =>
        // Verify manager is provided and functional
        use should not be null
        
        // Test resource acquisition with the manager
        val resource = use(new MockResource("test-resource", resourceTracker))
        resource.access() should be("Accessed test-resource")
        resource.closed should be(false) // Should not be closed yet within the test
        
        Future.successful(succeed)
      }
      
      it("should automatically close resources created in future after test completion") { use =>
        Future {
          // This test verifies the previous resource was closed
          resourceTracker.find(_.name == "test-resource").map(_.closed) should be(Some(true))
          
          // Create another resource
          val resource2 = use(new MockResource("test-resource-2", resourceTracker))
          resource2.access() should be("Accessed test-resource-2")
          
          succeed
        }
      }

      it("should automatically close resources created before future after test completion") { use =>
        // This test verifies the previous resource was closed
        resourceTracker.find(_.name == "test-resource-2").map(_.closed) should be(Some(true))
        
        // Create another resource
        val resource3 = use(new MockResource("test-resource-3", resourceTracker))
        resource3.access() should be("Accessed test-resource-3")
        
        Future.successful(succeed)
      }
      
      it("should handle exception in future gracefully") { use =>
        Future {
          // Test that exceptions are properly propagated while still closing resources
          val resource = use(new MockResource("exception-resource", resourceTracker))
          
          // This will throw, but resource should still be closed after test
          throw new RuntimeException("Test exception")
        }
      }

      it("should handle exception thrown not in future gracefully") { use =>
        // Test that exceptions are properly propagated while still closing resources
        val resource = use(new MockResource("exception-resource", resourceTracker))
          
        // This will throw, but resource should still be closed after test
        throw new RuntimeException("Test exception")
      }
    }

    it("should create a new instance for the test class and for each test, and close them automatically after finishes.") {
      // Create an instance of the test suite
      val testSpec = new TestSpec
      
      // Run the tests in the suite
      val reporter = new EventRecordingReporter
      val status = testSpec.run(None, Args(reporter))

      // SKIP-SCALATESTJS-START
      status.waitUntilCompleted()
      // SKIP-SCALATESTJS-END
      
      // Verify that 3 tests passed
      reporter.testSucceededEventsReceived.size should be (4)
      // Verify that 1 test failed (the one with the exception)
      reporter.testFailedEventsReceived.size should be (2)
      
      // Verify that resources were created and closed properly
      testSpec.resourceTracker.instancesCreated should be (6)
      testSpec.resourceTracker.resources.foreach { resource =>
        resource.closed should be (true)
      }
    }
  }
}