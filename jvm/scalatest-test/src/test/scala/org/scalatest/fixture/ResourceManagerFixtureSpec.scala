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
import org.scalatest._
import org.scalatest.funspec.{AnyFunSpec, FixtureAnyFunSpec}
import org.scalatest.matchers.should.Matchers
import SharedHelpers.EventRecordingReporter

// Test implementation of ResourceManagerFixture
class ResourceManagerFixtureSpec extends AnyFunSpec with Matchers {
  
  // Test that Using.Manager is properly provided to tests
  describe("ResourceManagerFixture") {

    // To track resource creation/closing across tests
    class TestSpec extends FixtureAnyFunSpec with ResourceManagerFixture with Matchers {

      val resourceTracker = new MockResourceTracker()

      it("should provide a Using.Manager to each test") { use =>
        // Verify manager is provided and functional
        use should not be null
        
        // Test resource acquisition with the manager
        val resource = use(new MockResource("test-resource", resourceTracker))
        resource.access() should be("Accessed test-resource")
        resource.closed should be(false) // Should not be closed yet within the test
      }
      
      it("should automatically close resources after test completion") { use =>
        // This test verifies the previous resource was closed
        resourceTracker.find(_.name == "test-resource").map(_.closed) should be(Some(true))
        
        // Create another resource
        val resource2 = use(new MockResource("test-resource-2", resourceTracker))
        resource2.access() should be("Accessed test-resource-2")
      }
      
      it("should handle exceptions gracefully") { use =>
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
      testSpec.run(None, Args(reporter))
      
      // Verify that 3 tests passed
      reporter.testSucceededEventsReceived.size should be (2)
      // Verify that 1 test failed (the one with the exception)
      reporter.testFailedEventsReceived.size should be (1)
      
      // Verify that resources were created and closed properly
      testSpec.resourceTracker.instancesCreated should be (3)
      testSpec.resourceTracker.resources.foreach { resource =>
        resource.closed should be(true)
      }
    }
  }
}
