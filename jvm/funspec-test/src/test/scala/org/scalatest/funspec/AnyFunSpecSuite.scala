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
package org.scalatest.funspec

import org.scalatest.SharedHelpers._
import org.scalatest.Args
import org.scalatest.BeforeAndAfterEach
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Reporter
import org.scalatest.StubReporter
import org.scalatest.Status
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.ConfigMap
import org.scalatest.Suite
import org.scalatest.Tracker
import org.scalatest.events._
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AnyFunSpecSuite extends AnyFunSuite {

  test("three plain-old specifiers should be invoked in order") {
    class MySpec extends AnyFunSpec with Matchers {
      var example1WasInvoked = false
      var example2WasInvokedAfterExample1 = false
      var example3WasInvokedAfterExample2 = false
      it("should get invoked") {
        example1WasInvoked = true
        /* ASSERTION_SUCCEED */
      }
      it("should also get invoked") {
        if (example1WasInvoked)
          example2WasInvokedAfterExample1 = true
        /* ASSERTION_SUCCEED */
      }
      it("should also also get invoked") {
        if (example2WasInvokedAfterExample1)
          example3WasInvokedAfterExample2 = true
        /* ASSERTION_SUCCEED */
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.example1WasInvoked)
    assert(a.example2WasInvokedAfterExample1)
    assert(a.example3WasInvokedAfterExample2)
  }

  test("three plain-old specifiers should be invoked in order when two are surrounded by a plain-old describe") {
    class MySpec extends AnyFunSpec with Matchers {
      var example1WasInvoked = false
      var example2WasInvokedAfterExample1 = false
      var example3WasInvokedAfterExample2 = false
      it("should get invoked") {
        example1WasInvoked = true
        /* ASSERTION_SUCCEED */
      }
      describe("Stack") {
        it("should also get invoked") {
          if (example1WasInvoked)
            example2WasInvokedAfterExample1 = true
          /* ASSERTION_SUCCEED */
        }
        it("should also also get invoked") {
          if (example2WasInvokedAfterExample1)
            example3WasInvokedAfterExample2 = true
          /* ASSERTION_SUCCEED */
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.example1WasInvoked)
    assert(a.example2WasInvokedAfterExample1)
    assert(a.example3WasInvokedAfterExample2)
  }
   
  test("two plain-old specifiers should show up in order of appearance in testNames") {
    class MySpec extends AnyFunSpec with Matchers {
      var example1WasInvoked = false
      var example2WasInvokedAfterExample1 = false
      it("should get invoked") {
        example1WasInvoked = true
        /* ASSERTION_SUCCEED */
      }
      it("should also get invoked") {
        if (example1WasInvoked)
          example2WasInvokedAfterExample1 = true
        /* ASSERTION_SUCCEED */
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.testNames.size == 2)
    assert(a.testNames.iterator.toList(0) == "should get invoked")
    assert(a.testNames.iterator.toList(1) == "should also get invoked")
  }
 
  test("plain-old specifier test names should include an enclosing describe string, separated by a space") {
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        it("must allow me to pop") {/* ASSERTION_SUCCEED */}
        it("must allow me to push") {/* ASSERTION_SUCCEED */}
      }
    }
    val a = new MySpec
    assert(a.testNames.size == 2)
    assert(a.testNames.iterator.toList(0) == "A Stack must allow me to pop")
    assert(a.testNames.iterator.toList(1) == "A Stack must allow me to push")
  }

  test("plain-old test names should properly nest plain-old descriptions in test names") {
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        describe("(when not empty)") {
          it("must allow me to pop") {/* ASSERTION_SUCCEED */}
        }
        describe("(when not full)") {
          it("must allow me to push") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    assert(a.testNames.size == 2)
    assert(a.testNames.iterator.toList(0) == "A Stack (when not empty) must allow me to pop")
    assert(a.testNames.iterator.toList(1) == "A Stack (when not full) must allow me to push")
  }
  
  test("should be able to mix in BeforeAndAfterEach with BeforeAndAfterAll without any problems") {
    class MySpec extends AnyFunSpec with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {
      describe("A Stack") {
        describe("(when not empty)") {
          it("should allow me to pop") {/* ASSERTION_SUCCEED */}
        }
        describe("(when not full)") {
          it("should allow me to push") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
  }
  
  // Test for good strings in report for top-level examples  
  test("Top-level plain-old specifiers should yield good strings in a TestSucceeded report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("must start with proper words") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }
    
  test("Top-level plain-old specifiers should yield good strings in a testSucceeded report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("must start with proper words") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }

  test("Top-level plain-old specifiers should yield good strings in a testFailed report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case event: TestFailed =>
            if (event.testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            event.formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("must start with proper words") { fail() }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }

  // Tests for good strings in report for nested-one-level examples
  test("Nested-one-level plain-old specifiers should yield good strings in a TestSucceeded report") {
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvoked = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            scopeOpenedHasBeenInvoked = true
            if (message.indexOf("My Spec") != -1)
              infoReportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My Spec")
                  infoReportHadCorrectSpecText = true
                if (formattedText == "My Spec")
                  infoReportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvoked)
            theOtherMethodHasBeenInvoked = true
            if (testName.indexOf("My Spec must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("My Spec") {
        it("must start with proper words") {/* ASSERTION_SUCCEED */}
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  test("Nested-one-level plain-old specifiers should yield good strings in a testSucceeded report") {
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvoked = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            scopeOpenedHasBeenInvoked = true
            if (message.indexOf("My Spec") != -1)
              infoReportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My Spec")
                  infoReportHadCorrectSpecText = true
                if (formattedText == "My Spec")
                  infoReportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvoked)
            theOtherMethodHasBeenInvoked = true
            if (testName.indexOf("My Spec must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("My Spec") {
        it("must start with proper words") {/* ASSERTION_SUCCEED */}
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  test("Nested-one-level plain-old specifiers should yield good strings in a TestFailed report") {
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvoked = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            scopeOpenedHasBeenInvoked = true
            if (message.indexOf("My Spec") != -1)
              infoReportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My Spec")
                  infoReportHadCorrectSpecText = true
                if (formattedText == "My Spec")
                  infoReportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case event: TestFailed =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvoked)
            theOtherMethodHasBeenInvoked = true
            if (event.testName.indexOf("My Spec must start with proper words") != -1)
              reportHadCorrectTestName = true
            event.formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("My Spec") {
        it("must start with proper words") { fail() }
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  
  // Tests for good strings in report for nested-two-levels examples
  test("Nested-two-levels plain-old specifiers should yield good strings in a TestSucceeded report") { //ZZZ
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvokedOnce = false
    var scopeOpenedHasBeenInvokedTwice = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            if (!scopeOpenedHasBeenInvokedOnce) { 
              scopeOpenedHasBeenInvokedOnce = true
              if (message.indexOf("My") >= 0)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            }
            else {
              scopeOpenedHasBeenInvokedTwice = true
              if (message.indexOf("Spec") < 0)
                infoReportHadCorrectTestName = false
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText != "Spec")
                    infoReportHadCorrectSpecText = false
                  if (formattedText != "  Spec")
                    infoReportHadCorrectFormattedSpecText = false
                case _ =>
              }
            }
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvokedTwice)
            theOtherMethodHasBeenInvoked = true
            if (testName.indexOf("My Spec must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "  - must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("My") {
        describe("Spec") {
          it("must start with proper words") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  test("Nested-two-levels plain-old specifiers should yield good strings in a TestFailed report") { //YYY
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvokedOnce = false
    var scopeOpenedHasBeenInvokedTwice = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            if (!scopeOpenedHasBeenInvokedOnce) { 
              scopeOpenedHasBeenInvokedOnce = true
              if (message.indexOf("My") >= 0)
                infoReportHadCorrectTestName = true
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText == "My")
                    infoReportHadCorrectSpecText = true
                  if (formattedText == "My")
                    infoReportHadCorrectFormattedSpecText = true
                case _ =>
              }
            }
            else {
              scopeOpenedHasBeenInvokedTwice = true
              if (message.indexOf("Spec") < 0)
                infoReportHadCorrectTestName = false
              formatter match {
                case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                  if (rawText != "Spec")
                    infoReportHadCorrectSpecText = false
                  if (formattedText != "  Spec")
                    infoReportHadCorrectFormattedSpecText = false
                case _ =>
              }
            }
          case event: TestFailed =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvokedTwice)
            theOtherMethodHasBeenInvoked = true
            if (event.testName.indexOf("My Spec must start with proper words") != -1)
              reportHadCorrectTestName = true
            event.formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "  - must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("My") {
        describe("Spec") {
          it("must start with proper words") { fail() }
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  // Test for good strings in report for top-level shared behavior examples
  test("Top-level 'shared behavior - fancy specifiers' should yield good strings in a TestSucceeded report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("it should start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "it should start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- it should start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      def myBehavior(i: Int): Unit = {
        it("it should start with proper words") {/* ASSERTION_SUCCEED */}
      }
      it should behave like myBehavior(1)
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }
  
  test("Top-level 'shared behavior - plain-old specifiers' should yield good strings in a TestSucceeded report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      def myBehavior(i: Int): Unit = {
        it("must start with proper words") {/* ASSERTION_SUCCEED */}
      }
      it should behave like myBehavior(1)
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }

  test("Top-level 'shared behavior - plain-old specifiers' should yield good strings in a testSucceeded report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      def myBehavior(i: Int): Unit = {
        it("must start with proper words") {/* ASSERTION_SUCCEED */}
      }
      it should behave like myBehavior(1)
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }

  test("Top-level 'shared behavior - plain-old specifiers' should yield good strings in a TestFailed report") {
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case event: TestFailed =>
            if (event.testName.indexOf("must start with proper words") != -1)
              reportHadCorrectTestName = true
            event.formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "must start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- must start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      def myBehavior(i: Int): Unit = {
        it("must start with proper words") { fail() }
      }
      it should behave like myBehavior(1)
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
  }

  // Tests for good strings in report for shared-behavior, nested-one-level specifiers
  test("Nested-one-level 'shared behavior' should yield good strings in a TestSucceeded report") {
    var infoReportHadCorrectTestName = false
    var infoReportHadCorrectSpecText = false
    var infoReportHadCorrectFormattedSpecText = false
    var reportHadCorrectTestName = false
    var reportHadCorrectSpecText = false
    var reportHadCorrectFormattedSpecText = false
    var scopeOpenedHasBeenInvoked = false
    var theOtherMethodHasBeenInvoked = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the other method
            assert(!theOtherMethodHasBeenInvoked)
            scopeOpenedHasBeenInvoked = true
            if (message.indexOf("My Spec") != -1)
              infoReportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My Spec")
                  infoReportHadCorrectSpecText = true
                if (formattedText == "My Spec")
                  infoReportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            // scopeOpened should be invoked before the this method
            assert(scopeOpenedHasBeenInvoked)
            theOtherMethodHasBeenInvoked = true
            if (testName.indexOf("My Spec should start with proper words") != -1)
              reportHadCorrectTestName = true
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "should start with proper words")
                  reportHadCorrectSpecText = true
                if (formattedText == "- should start with proper words")
                  reportHadCorrectFormattedSpecText = true
              case _ =>
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      def myBehavior(i: Int): Unit = {
        it("should start with proper words") {/* ASSERTION_SUCCEED */}
      }
      describe("My Spec") {
        it should behave like myBehavior(1)
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(reportHadCorrectTestName)
    assert(reportHadCorrectSpecText)
    assert(reportHadCorrectFormattedSpecText)
    assert(infoReportHadCorrectTestName)
    assert(infoReportHadCorrectSpecText)
    assert(infoReportHadCorrectFormattedSpecText)
  }

  // Huh? what was I testing here?
  test("An empty describe shouldn't throw an exception") {
    class MySpec extends AnyFunSpec with Matchers {
      describe("this will be empty") {}
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
  }  
  
  test("Only a passed test name should be invoked.") {
    var correctTestWasInvoked = false
    var wrongTestWasInvoked = false
    class MySpec extends AnyFunSpec with Matchers {
      it("it should be invoked") {
        correctTestWasInvoked = true
        /* ASSERTION_SUCCEED */
      }
      it("it should not be invoked") {
        wrongTestWasInvoked = true
        /* ASSERTION_SUCCEED */
      }
    }
    val a = new MySpec
    a.run(Some("it should be invoked"), Args(StubReporter))
    assert(correctTestWasInvoked)
    assert(!wrongTestWasInvoked)
  }
  
  test("Config map should make it through to runTest") {
    var foundMyGoodie = false
    class MySpec extends AnyFunSpec with Matchers {
      override def runTest(testName: String, args: Args): Status = {
        foundMyGoodie = args.configMap.contains("my goodie")
        super.runTest(testName, args)
      }
      it("it should find my goodie") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(StubReporter, Stopper.default, Filter(), ConfigMap("my goodie" -> "hi"), None, new Tracker))
    assert(foundMyGoodie)  
  }
  
  // I think delete this one. Repeat.
  test("In a TestSucceeded report, the example name should start with '<description> should' if nested two levels inside describe clauses") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("A Stack (when working right) should push and pop properly") != -1) {
              testSucceededReportHadCorrectTestName = true
            }  
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        describe("(when working right)") {
          it("should push and pop properly") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }
  
  test("expectedTestCount is the number of plain-old specifiers if no shares") {
    class MySpec extends AnyFunSpec with Matchers {
      it("must one") {/* ASSERTION_SUCCEED */}
      it("must two") {/* ASSERTION_SUCCEED */}
      describe("behavior") {
        it("must three") {/* ASSERTION_SUCCEED */}
        it("must four") {/* ASSERTION_SUCCEED */}
      }
      it("must five") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    assert(a.expectedTestCount(Filter()) == 5)
  }

  // Testing strings sent in reports
  test("In a TestSucceeded report, the example name should be verbatim if top level if example registered with it") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("this thing must start with proper words") != -1) {
              testSucceededReportHadCorrectTestName = true
            }  
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("this thing must start with proper words") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }

  test("In a testSucceeded report, the example name should be verbatim if top level if example registered with it") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("this thing must start with proper words") != -1) {
              testSucceededReportHadCorrectTestName = true
            }  
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("this thing must start with proper words") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }

  test("In a TestFailed report, the example name should be verbatim if top level if example registered with it") {
    var testFailedReportHadCorrectTestName = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case event: TestFailed =>
            if (event.testName.indexOf("this thing must start with proper words") != -1)
              testFailedReportHadCorrectTestName = true
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("this thing must start with proper words") { fail() }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testFailedReportHadCorrectTestName)
  }
  
  test("In a TestStarting report, the example name should start with '<description> ' if nested one level " +
        "inside a describe clause and registered with it") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestStarting(_, _, _, _, testName, _, _, _, _, _, _, _) =>
            if (testName == "A Stack needs to push and pop properly") {
              testSucceededReportHadCorrectTestName = true
            }
          case _ => 
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        it("needs to push and pop properly") {/* ASSERTION_SUCCEED */}
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }
    
  test("Specs should send defined formatters") {
    class MyReporter extends Reporter {

      var gotAnUndefinedFormatter = false
      var lastEventWithUndefinedFormatter: Option[Event] = None

      private def ensureFormatterIsDefined(event: Event): Unit = {
        if (!event.formatter.isDefined) {
          gotAnUndefinedFormatter = true
          lastEventWithUndefinedFormatter = Some(event)
        }
      }

      def apply(event: Event): Unit = {
        event match {
          case event: RunAborted => ensureFormatterIsDefined(event)
          case event: SuiteAborted => ensureFormatterIsDefined(event)
          case event: SuiteStarting => ensureFormatterIsDefined(event)
          case event: SuiteCompleted => ensureFormatterIsDefined(event)
          case event: TestStarting => ensureFormatterIsDefined(event)
          case event: TestSucceeded => ensureFormatterIsDefined(event)
          case event: TestIgnored => ensureFormatterIsDefined(event)
          case event: TestFailed => ensureFormatterIsDefined(event)
          case event: InfoProvided => ensureFormatterIsDefined(event)
          case _ =>
        }
      }
    }

    class MySpec extends AnyFunSpec with Matchers {
      it("it should send defined formatters") {
        assert(true)
      }
      it("it should also send defined formatters") {
        assert(false)
      }
    }
    val a = new MySpec
    val myRep = new MyReporter
    a.run(None, Args(myRep))
    assert(!myRep.gotAnUndefinedFormatter, myRep.lastEventWithUndefinedFormatter.toString)
  }

  test("SpecText should come through correctly in a SpecReport when registering with it") {
    var testSucceededReportHadCorrectSpecText = false
    var lastSpecText: Option[String] = None
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My spec text must have the proper words")
                  testSucceededReportHadCorrectSpecText = true
                else
                  lastSpecText = Some(rawText)
              case _ => throw new RuntimeException("Got a non-SpecReport")
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      it("My spec text must have the proper words") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
  }

  test("Spec text should come through correctly in a SpecReport when registering with it when nested in one describe") {
    var testSucceededReportHadCorrectSpecText = false
    var lastSpecText: Option[String] = None
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My short name must have the proper words")
                  testSucceededReportHadCorrectSpecText = true
                else
                  lastSpecText = Some(rawText)
              case _ => throw new RuntimeException("Got a non-SpecReport")
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        it("My short name must have the proper words") {/* ASSERTION_SUCCEED */}
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
  }

  test("Spec text should come through correctly in a SpecReport when registering with it when nested in two describes") {
    var testSucceededReportHadCorrectSpecText = false
    var lastSpecText: Option[String] = None
    class MyReporter extends Reporter {
      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                if (rawText == "My short name must have the proper words")
                  testSucceededReportHadCorrectSpecText = true
                else
                  lastSpecText = Some(rawText)
              case _ => throw new RuntimeException("Got a non-SpecReport")
            }
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        describe("(when empty)") {
          it("My short name must have the proper words") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectSpecText, lastSpecText match { case Some(s) => s; case None => "No report"})
  }

  test("Should get ScopeOpened with description if one and only one describe clause") {

    val expectedSpecText = "A Stack"

    class MyReporter extends Reporter {
      var scopeOpenedCalled = false
      var expectedMessageReceived = false
      def apply(event: Event): Unit = {
        event match {
          case event: ScopeOpened =>
            event.formatter match {
              case Some(IndentedText(formattedText, rawText, indentationLevel)) =>
                scopeOpenedCalled = true
                if (!expectedMessageReceived) {
                  expectedMessageReceived = (rawText == expectedSpecText)
                }
              case _ =>
            }
          case _ =>
        }
      }
    }

    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        it("should allow me to push") {/* ASSERTION_SUCCEED */}
      }
    }

    val a = new MySpec
    val myRep = new MyReporter
    a.run(None, Args(myRep))
    assert(myRep.scopeOpenedCalled)
    assert(myRep.expectedMessageReceived)
  }
 
  // Testing Shared behaviors
  test("a shared specifier invoked with 'should behave like a' should get invoked") {
    class MySpec extends AnyFunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      var sharedExampleInvoked = false
      def invocationVerifier(i: Int): Unit = {
        it("should be invoked") {
          sharedExampleInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }
      describe("A Stack") {
        describe("(when not empty)") {
          it("should allow me to pop") {/* ASSERTION_SUCCEED */}
          it should behave like invocationVerifier(1) 
        }
        describe("(when not full)") {
          it("should allow me to push") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.sharedExampleInvoked)
  }
  
  test("two examples in a shared behavior should get invoked") {
    class MySpec extends AnyFunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      var sharedExampleInvoked = false
      var sharedExampleAlsoInvoked = false
      def invocationVerifier(i: Int): Unit = {
        it("should be invoked") {
          sharedExampleInvoked = true
          /* ASSERTION_SUCCEED */
        }
        it("should also be invoked") {
          sharedExampleAlsoInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }
      describe("A Stack") {
        describe("(when not empty)") {
          it("should allow me to pop") {/* ASSERTION_SUCCEED */}
          it should behave like invocationVerifier(1)
        }
        describe("(when not full)") {
          it("should allow me to push") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.sharedExampleInvoked)
    assert(a.sharedExampleAlsoInvoked)
  }

  test("three examples in a shared behavior should be invoked in order") {
    class MySpec extends AnyFunSpec {
      var example1WasInvoked = false
      var example2WasInvokedAfterExample1 = false
      var example3WasInvokedAfterExample2 = false
      def invocationVerifier(i: Int): Unit = {
        it("should get invoked") {
          example1WasInvoked = true
          /* ASSERTION_SUCCEED */
        }
        it("should also get invoked") {
          if (example1WasInvoked)
            example2WasInvokedAfterExample1 = true
          /* ASSERTION_SUCCEED */
        }
        it("should also also get invoked") {
          if (example2WasInvokedAfterExample1)
            example3WasInvokedAfterExample2 = true
          /* ASSERTION_SUCCEED */
        }
      }
      it should behave like invocationVerifier(1)
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(a.example1WasInvoked)
    assert(a.example2WasInvokedAfterExample1)
    assert(a.example3WasInvokedAfterExample2)
  }
  
  test("three examples in a shared behavior should not get invoked at all if the behavior isn't used in a like clause") {
    class MySpec extends AnyFunSpec with Matchers {
      var example1WasInvoked = false
      var example2WasInvokedAfterExample1 = false
      var example3WasInvokedAfterExample2 = false
      def invocationVerifier(i: Int): Unit = {
        it("should get invoked") {
          example1WasInvoked = true
          /* ASSERTION_SUCCEED */
        }
        it("should also get invoked") {
          if (example1WasInvoked)
            example2WasInvokedAfterExample1 = true
          /* ASSERTION_SUCCEED */
        }
        it("should also also get invoked") {
          if (example2WasInvokedAfterExample1)
            example3WasInvokedAfterExample2 = true
          /* ASSERTION_SUCCEED */
        }
      }
      // don't use it: behaves like (an InvocationVerifier())
    }
    val a = new MySpec
    a.run(None, Args(SilentReporter))
    assert(!a.example1WasInvoked)
    assert(!a.example2WasInvokedAfterExample1)
    assert(!a.example3WasInvokedAfterExample2)
  }
  
  // Probably delete
  test("The test name for a shared specifier invoked with 'should behave like a' should be verbatim if top level") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {

      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("it should be invoked") != -1) {
              testSucceededReportHadCorrectTestName = true
            }  
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec with BeforeAndAfterEach with BeforeAndAfterAll {
      var sharedExampleInvoked = false
      def invocationVerifier(i: Int): Unit = {
        it("it should be invoked") {
          sharedExampleInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }
      it should behave like invocationVerifier(1) 
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }
  
  ignore("The example name for a shared example invoked with 'it should behave like' should start with '<description> should' if nested one level in a describe clause") {
    var testSucceededReportHadCorrectTestName = false
    class MyReporter extends Reporter {

      def apply(event: Event): Unit = {
        event match {
          case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, testEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            if (testName.indexOf("A Stack should pop properly") != -1) {
              testSucceededReportHadCorrectTestName = true
            }  
          case _ =>
        }
      }
    }
    class MySpec extends AnyFunSpec {
      var sharedExampleInvoked = false
      def invocationVerifier(i: Int): Unit = {
        it("should pop properly") {
          sharedExampleInvoked = true
          /* ASSERTION_SUCCEED */
        }
      }
      describe("A Stack") {
        it should behave like invocationVerifier(1) 
      }
    }
    val a = new MySpec
    a.run(None, Args(new MyReporter))
    assert(testSucceededReportHadCorrectTestName)
  }
 
  test("expectedTestCount should not include tests in shares if never called") {
    class MySpec extends AnyFunSpec with Matchers {
      class Misbehavior extends AnyFunSpec with Matchers {
        it("should six") {/* ASSERTION_SUCCEED */}
        it("should seven") {/* ASSERTION_SUCCEED */}
      }
      it("should one") {/* ASSERTION_SUCCEED */}
      it("should two") {/* ASSERTION_SUCCEED */}
      describe("behavior") {
        it("should three") {/* ASSERTION_SUCCEED */}
        it("should four") {/* ASSERTION_SUCCEED */}
      }
      it("should five") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    assert(a.expectedTestCount(Filter()) == 5)
  }

  test("expectedTestCount should include tests in a share that is called") {
    class MySpec extends AnyFunSpec {
      def misbehavior(i: Int): Unit = {
        it("should six") {/* ASSERTION_SUCCEED */}
        it("should seven") {/* ASSERTION_SUCCEED */}
      }
      it("should one") {/* ASSERTION_SUCCEED */}
      it("should two") {/* ASSERTION_SUCCEED */}
      describe("behavior") {
        it("should three") {/* ASSERTION_SUCCEED */}
        it should behave like misbehavior(1) 
        it("should four") {/* ASSERTION_SUCCEED */}
      }
      it("should five") {/* ASSERTION_SUCCEED */}
    }
    val a = new MySpec
    assert(a.expectedTestCount(Filter()) == 7)
  }

  test("expectedTestCount should include tests in a share that is called twice") {
    class MySpec extends AnyFunSpec {
      def misbehavior(i: Int): Unit = {
        it("should six") {/* ASSERTION_SUCCEED */}
        it("should seven") {/* ASSERTION_SUCCEED */}
      }
      it("should one") {/* ASSERTION_SUCCEED */}
      it("should two") {/* ASSERTION_SUCCEED */}
      describe("behavior") {
        it("should three") {/* ASSERTION_SUCCEED */}
        it should behave like misbehavior(1) 
        it("should four") {/* ASSERTION_SUCCEED */}
      }
      it("should five") {/* ASSERTION_SUCCEED */}
      it should behave like misbehavior(1) 
    }
    val a = new MySpec
    assert(a.expectedTestCount(Filter()) == 9)
  }

  test("Spec's expectedTestCount includes tests in nested suites") {
    class TwoTestSpec extends AnyFunSpec {
      it("should count this test") {/* ASSERTION_SUCCEED */}
      it("should count this test also") {/* ASSERTION_SUCCEED */}
    }
    class MySpec extends AnyFunSpec {

      override def nestedSuites = Vector(new TwoTestSpec, new TwoTestSpec, new TwoTestSpec)

      it("should count this here test") {/* ASSERTION_SUCCEED */}
    }
    val mySpec = new MySpec
    assert(mySpec.expectedTestCount(Filter()) == 7)
  }

  // End of Share stuff
  ignore("should be able to send info to the reporter") { // Can't do this yet, no info in Spec yet
                                                          // UPDATE 27 August Chee Seng: Probably need to use recordedEvents now.
    val expectedMessage = "this is the expected message"

    class MyReporter extends Reporter {
      var infoProvidedCalled = false
      var expectedMessageReceived = false

      def apply(event: Event): Unit = {
        event match {
          case event: InfoProvided =>
            infoProvidedCalled = true
            if (!expectedMessageReceived) {
              expectedMessageReceived = event.message.indexOf(expectedMessage) != -1
            }
          case _ =>
        }
      }
    }

    class MySpec extends AnyFunSpec with Matchers {
      describe("A Stack") {
        describe("(when not empty)") {
          it("should allow me to pop") {
            info(expectedMessage)
            ()
            /* ASSERTION_SUCCEED */
          }
        }
        describe("(when not full)") {
          it("should allow me to push") {/* ASSERTION_SUCCEED */}
        }
      }
    }
    val a = new MySpec
    val myRep = new MyReporter
    a.run(None, Args(myRep))
    assert(myRep.infoProvidedCalled)
    assert(myRep.expectedMessageReceived)
  }

  test("that a null specText results in a thrown NPE at construction time") {
    intercept[NullArgumentException] {
      new AnyFunSpec {
        it(null) {/* ASSERTION_SUCCEED */}
      }
    }
    intercept[NullArgumentException] {
      new AnyFunSpec {
        ignore(null) {/* ASSERTION_SUCCEED */}
      }
    }
  }

  test("test durations are included in TestFailed and TestSucceeded events fired from Spec") {

    class MySpec extends AnyFunSpec {
      it("should succeed") {/* ASSERTION_SUCCEED */}
      it("should fail") { fail() }
    }

    val mySpec = new MySpec
    val myReporter = new TestDurationReporter
    mySpec.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.testSucceededWasFiredAndHadADuration)
    assert(myReporter.testFailedWasFiredAndHadADuration)
  }

  test("suite durations are included in SuiteCompleted events fired from Spec") {

    class MySpec extends AnyFunSpec {
      override def nestedSuites = Vector(new Suite {})
    }

    val mySuite = new MySpec
    val myReporter = new SuiteDurationReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.suiteCompletedWasFiredAndHadADuration)
  }

  test("suite durations are included in SuiteAborted events fired from Spec") {

    class SuiteThatAborts extends Suite {
      override def run(testName: Option[String], args: Args): Status = {
        throw new RuntimeException("Aborting for testing purposes")
      }
    }

    class MySpec extends AnyFunSpec {
      override def nestedSuites = Vector(new SuiteThatAborts {})
    }

    val mySuite = new MySpec
    val myReporter = new SuiteDurationReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.suiteAbortedWasFiredAndHadADuration)
  }

  test("pending in a Spec should cause TestPending to be fired") {

    class MySpec extends AnyFunSpec {
      it("should be pending") (pending)
    }

    val mySuite = new MySpec
    val myReporter = new PendingReporter
    mySuite.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    assert(myReporter.testPendingWasFired)
  }
}