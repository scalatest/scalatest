package org.scalatest.selenium

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.firefox.FirefoxProfile
import org.openqa.selenium.safari.SafariDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.ie.InternetExplorerDriver
import org.scalatest.tagobjects.Slow

class DriverSpec extends FunSpec {

  describe("Tests grouped using Driver trait") {

    trait GoogleSearchSpec extends FunSpecLike with concurrent.Eventually { this: WebBrowser with Driver =>

      describe("google.com") {

        it("should change its title based on the term searched") {
          // Cancel test when cannot access google.com
          try goTo("http://www.google.com") catch { case e: Throwable => cancel(e) }
          clickOn("q")
          textField("q").value = "Cheese!"
          submit()
          // Google's search is rendered dynamically with JavaScript.
          eventually(assert(pageTitle === "Cheese! - Google Search"))
          close()
        }

      }

    }

    class GoogleSearchSpecWithChrome extends GoogleSearchSpec with Chrome
    class GoogleSearchSpecWithSafari extends GoogleSearchSpec with Safari
    class GoogleSearchSpecWithInternetExplorer extends GoogleSearchSpec with InternetExplorer
    class GoogleSearchSpecWithFirefox extends GoogleSearchSpec with Firefox

    it("should work with Chrome", Slow) {
      // Cancel when Chrome is not available
      try new ChromeDriver catch { case e: Throwable => cancel(e) }
      val s = new GoogleSearchSpecWithChrome
      val rep = new EventRecordingReporter
      s.run(None, Args(reporter = rep))
      val scopeOpenedList = rep.scopeOpenedEventsReceived
      assert(scopeOpenedList.size == 1)
      assert(scopeOpenedList(0).message == "google.com")
      val scopeClosedList = rep.scopeClosedEventsReceived
      assert(scopeClosedList.size == 1)
      assert(scopeClosedList(0).message == "google.com")
      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size == 1)
      assert(testStartingList(0).testName == "google.com should change its title based on the term searched")
      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).testName == "google.com should change its title based on the term searched")
    }

    it("should work with Firefox", Slow) {
      // Cancel when Firefox is not available
      try new FirefoxDriver(new FirefoxProfile) catch { case e: Throwable => cancel(e) }
      val s = new GoogleSearchSpecWithFirefox
      val rep = new EventRecordingReporter
      s.run(None, Args(reporter = rep))
      val scopeOpenedList = rep.scopeOpenedEventsReceived
      assert(scopeOpenedList.size == 1)
      assert(scopeOpenedList(0).message == "google.com")
      val scopeClosedList = rep.scopeClosedEventsReceived
      assert(scopeClosedList.size == 1)
      assert(scopeClosedList(0).message == "google.com")
      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size == 1)
      assert(testStartingList(0).testName == "google.com should change its title based on the term searched")
      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).testName == "google.com should change its title based on the term searched")
    }

    it("should work with Internet Explorer", Slow) {
      // Cancel when Internet Explorer is not available
      try new InternetExplorerDriver catch { case e: Throwable => cancel(e) }
      val s = new GoogleSearchSpecWithFirefox
      val rep = new EventRecordingReporter
      s.run(None, Args(reporter = rep))
      val scopeOpenedList = rep.scopeOpenedEventsReceived
      assert(scopeOpenedList.size == 1)
      assert(scopeOpenedList(0).message == "google.com")
      val scopeClosedList = rep.scopeClosedEventsReceived
      assert(scopeClosedList.size == 1)
      assert(scopeClosedList(0).message == "google.com")
      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size == 1)
      assert(testStartingList(0).testName == "google.com should change its title based on the term searched")
      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).testName == "google.com should change its title based on the term searched")
    }

    it("should work with Safari", Slow) {
      // Cancel when Safari is not available
      try new SafariDriver catch { case e: Throwable => cancel(e) }
      val s = new GoogleSearchSpecWithFirefox
      val rep = new EventRecordingReporter
      s.run(None, Args(reporter = rep))
      val scopeOpenedList = rep.scopeOpenedEventsReceived
      assert(scopeOpenedList.size == 1)
      assert(scopeOpenedList(0).message == "google.com")
      val scopeClosedList = rep.scopeClosedEventsReceived
      assert(scopeClosedList.size == 1)
      assert(scopeClosedList(0).message == "google.com")
      val testStartingList = rep.testStartingEventsReceived
      assert(testStartingList.size == 1)
      assert(testStartingList(0).testName == "google.com should change its title based on the term searched")
      val testSucceededList = rep.testSucceededEventsReceived
      assert(testSucceededList.size == 1)
      assert(testSucceededList(0).testName == "google.com should change its title based on the term searched")
    }
  }

}