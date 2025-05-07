package org.scalatest.fixture

import org.scalactic.Using
import scala.util.{Success, Failure}
import java.io.{Closeable, IOException}
import org.scalatest._
import org.scalatest.funspec.{AnyFunSpec, FixtureAnyFunSpec}
import org.scalatest.matchers.should.Matchers
import SharedHelpers.EventRecordingReporter

// Mock closeable resource for testing
class MockResource(val name: String) extends Closeable {
  var closed = false
  var accessed = false
  
  // Track instantiation for lazy val testing
  MockResource.instancesCreated += 1
  
  def access(): String = {
    accessed = true
    s"Accessed $name"
  }
  
  override def close(): Unit = {
    closed = true
  }
}

// Companion object to track resource creation
object MockResource {
  var instancesCreated: Int = 0
  def resetCounter(): Unit = instancesCreated = 0
}

// Test implementation of ResourceManagerFixture
class ResourceManagerFixtureSpec extends AnyFunSpec with Matchers {
  
  MockResource.resetCounter()
  
  // Test that Using.Manager is properly provided to tests
  describe("ResourceManagerFixture") {

    // A variable to track resource creation/closing across tests
    val resourceTracker = collection.mutable.ArrayBuffer.empty[MockResource]

    class TestSpec extends FixtureAnyFunSpec with ResourceManagerFixture with Matchers {

      it("should provide a Using.Manager to each test") { use =>
        // Verify manager is provided and functional
        use should not be null
        
        // Test resource acquisition with the manager
        val resource = use(new MockResource("test-resource"))
        resource.access() should be("Accessed test-resource")
        resource.closed should be(false) // Should not be closed yet within the test
        
        // Track this resource
        resourceTracker += resource
      }
      
      it("should automatically close resources after test completion") { use =>
        // This test verifies the previous resource was closed
        resourceTracker.find(_.name == "test-resource").map(_.closed) should be(Some(true))
        
        // Create another resource
        val resource2 = use(new MockResource("test-resource-2"))
        resource2.access() should be("Accessed test-resource-2")
        
        // Track this new resource
        resourceTracker += resource2
      }
      
      it("should allow suite-scoped resources") { manager =>
        // Create a suite-scoped resource using the suiteScoped manager
        val suiteResource = suiteScoped(new MockResource("suite-resource"))
        suiteResource.access() should be("Accessed suite-resource")
        
        // Verify the manager from the test parameter and suiteScoped are the same instance
        suiteScoped should not be theSameInstanceAs (manager)
        
        // Verify the test-specific resource from previous test was closed
        resourceTracker.find(_.name == "test-resource-2").map(_.closed) should be(Some(true))
        resourceTracker += suiteResource
      }
      
      it("should handle exceptions gracefully") { use =>
        // Test that exceptions are properly propagated while still closing resources
        val resource = use(new MockResource("exception-resource"))

        resourceTracker += resource
        
        // This will throw, but resource should still be closed after test
        throw new RuntimeException("Test exception")
      }
    }

    it("should create a new instance for the test class and for each test, and close them automatically after finishes.") { (use: Using.Manager) =>
      // Create an instance of the test suite
      val testSpec = new TestSpec
      
      // Run the tests in the suite
      val reporter = new EventRecordingReporter
      testSpec.run(None, Args(reporter))
      
      // Verify that 3 tests passed
      reporter.testSucceededEventsReceived.size should be (3)
      // Verify that 1 test failed (the one with the exception)
      reporter.testFailedEventsReceived.size should be (1)
      
      // Verify that resources were created and closed properly
      MockResource.instancesCreated should be (4)
      resourceTracker.length should be (4)
      resourceTracker.foreach { resource =>
        resource.closed should be(true)
      }
    }
  }
  
  // Test that accessing suiteScoped outside tests throws the expected exception
  describe("suiteScoped access") {
    
    class TestSpec extends FixtureAnyFunSpec with ResourceManagerFixture {
      // This should throw an exception when accessed outside a test
      val shouldFail = suiteScoped
    }

    it("should throw an exception when accessed outside a test") {
      // Create an instance of the test suite
      val e = 
        intercept[IllegalStateException] {
          new TestSpec
        }
      e.getMessage should include("`suiteScoped`` cannot be called from outside a test")
    }
  }
  
  // Test lazy val behavior with resources
  describe("Lazy resource initialization") {
    // Create a class with lazy resources to test
    class LazyResourceTest extends AnyFunSpec with ResourceManagerFixture with Matchers {
      // Lazy val that depends on suiteScoped but won't be initialized until accessed
      lazy val lazyResource = suiteScoped(new MockResource("lazy-resource"))
      
      // Eager val that will be initialized immediately
      val eagerResource = new MockResource("eager-resource")
      
      it("should not initialize lazy resources unless accessed") { (manager: Using.Manager) =>
        // At this point, MockResource.instancesCreated should be 1 (just the eager resource)
        MockResource.instancesCreated should be(1)
        
        // Now access the lazy resource, which should trigger initialization
        lazyResource.access() should be("Accessed lazy-resource")
        
        // Now instances created should be 2
        MockResource.instancesCreated should be(2)
      }
      
      it("should initialize lazy resources only once") { (manager: Using.Manager) =>
        // Accessing the lazy resource multiple times should not create new instances
        lazyResource.access()
        lazyResource.access()
        
        // Still only 2 instances total
        MockResource.instancesCreated should be(2)
      }
    }

    it("should not initialize lazy resources until accessed") { (manager: Using.Manager) =>
      // Create an instance of the nested test suite
      val lazyResourceTest = new LazyResourceTest
      val reporter = new EventRecordingReporter
      lazyResourceTest.run(None, Args(reporter))
      reporter.testSucceededEventsReceived.size should be (2)
    }
  }
}