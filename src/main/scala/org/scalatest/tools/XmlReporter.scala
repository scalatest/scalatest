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
package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._

import Suite.unparsedXml
import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Enumeration
import java.util.Properties
import java.net.UnknownHostException
import java.net.InetAddress

import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer
import scala.xml

/**
 * A <code>Reporter</code> that writes test status information in XML format.
 *
 * A separate file is written for each test suite, named [classname].xml,
 * to the directory specified.
 *
 * @exception IOException if unable to open the file for writing
 *
 * @author George Berger
 */
private[scalatest] class XmlReporter(directory: String) extends Reporter {

  private val events = Set.empty[Event]
  private val propertiesXml = genPropertiesXml

  //
  // Records events in 'events' set.  Generates xml from events upon receipt
  // of SuiteCompleted or SuiteAborted events.
  //
  def apply(event: Event) {
    events += event

    event match {
      case e: SuiteCompleted =>
        writeSuiteFile(e)

      case e: SuiteAborted =>
        writeSuiteFile(e)

      case _ =>
    }
  }

  //
  // Writes the xml file for a single test suite.  Removes processed
  // events from the events Set as they are used.
  //
  private def writeSuiteFile(endEvent: Event) {
    require(endEvent.isInstanceOf[SuiteCompleted] ||
            endEvent.isInstanceOf[SuiteAborted])
    
    val testsuite = getTestsuite(endEvent)
    val xmlStr    = xmlify(testsuite)
    val filespec  = directory + "/" + testsuite.name + ".xml"
    
    val out = new PrintWriter(filespec, "UTF-8")
    out.print(xmlStr)
    out.close()
  }

  //
  // Constructs a Testsuite object corresponding to a specified
  // SuiteCompleted or SuiteAborted event.
  //
  // Scans events reported so far and builds the Testsuite from events
  // associated with the specified suite.  Removes events from
  // the class's events Set as they are consumed.
  //
  // Only looks at events that have the same ordinal prefix as the
  // end event being processed (where an event's ordinal prefix is its
  // ordinal list with last element removed).  Events with the same
  // prefix get processed sequentially, so filtering this way eliminates
  // events from any nested suites being processed concurrently
  // that have not yet completed when the parent's SuiteCompleted or
  // SuiteAborted event is processed.
  //
  private def getTestsuite(endEvent: Event): Testsuite = {
    require(endEvent.isInstanceOf[SuiteCompleted] ||
            endEvent.isInstanceOf[SuiteAborted])

    val ordinalPrefix = endEvent.ordinal.toList.dropRight(1)

    val samePrefixEvents = 
      events.toList.filter(
        e => e.ordinal.toList.dropRight(1) == ordinalPrefix)

    val orderedEvents = samePrefixEvents.sortWith((a, b) => a < b).toArray

    val (startIndex, endIndex) = locateSuite(orderedEvents, endEvent)

    val startEvent = orderedEvents(startIndex).asInstanceOf[SuiteStarting]
    events -= startEvent

    val name =
      startEvent.suiteClassName match {
        case Some(className) => className
        case None            => startEvent.suiteName
      }

    val testsuite = Testsuite(name, startEvent.timeStamp)

    var idx = startIndex + 1
    while (idx <= endIndex) {
      val event = orderedEvents(idx)
      events -= event

      event match {
        case e: TestStarting =>
          val (testEndIndex, testcase) = processTest(orderedEvents, e, idx)
          if (!testcase.pending && !testcase.canceled) {
            testsuite.testcases += testcase
            if (testcase.failure != None) testsuite.failures += 1
          }
          idx = testEndIndex + 1

        case e: TestIgnored =>
          val testcase = ignoreTest(e)
          testsuite.testcases += testcase
          idx += 1

        case e: SuiteAborted =>
          assert(endIndex == idx)
          testsuite.errors += 1
          testsuite.time = e.timeStamp - testsuite.timeStamp
          idx += 1

        case e: SuiteCompleted =>
          assert(endIndex == idx)
          testsuite.time = e.timeStamp - testsuite.timeStamp
          idx += 1

        case e: InfoProvided   => idx += 1
        case e: AlertProvided  => idx += 1
        case e: NoteProvided   => idx += 1
        case e: ScopeOpened    => idx += 1 // TODO: Ask George to verify
        case e: ScopeClosed    => idx += 1
        case e: ScopePending   => idx += 1
        case e: MarkupProvided => idx += 1
        case e: TestPending    => unexpected(e)
        case e: TestCanceled   => unexpected(e)
        case e: RunStarting    => unexpected(e)
        case e: RunCompleted   => unexpected(e)
        case e: RunStopped     => unexpected(e)
        case e: RunAborted     => unexpected(e)
        case e: TestSucceeded  => unexpected(e)
        case e: TestFailed     => unexpected(e)
        case e: SuiteStarting  => unexpected(e)
        case e: DiscoveryStarting  => unexpected(e)
        case e: DiscoveryCompleted => unexpected(e)
      }
    }
    testsuite
  }

  //
  // Finds the indexes for the SuiteStarted and SuiteCompleted or
  // SuiteAborted endpoints of a test suite within an ordered array of
  // events, given the terminating SuiteCompleted or SuiteAborted event.
  //
  // Searches sequentially through the array to find the specified
  // SuiteCompleted event and its preceding SuiteStarting event.
  //
  // (The orderedEvents array does not contain any SuiteStarting events
  // from nested suites running concurrently because of the ordinal-prefix
  // filtering performed in getTestsuite().  It does not contain any from
  // nested suites running sequentially because those get removed when they
  // are processed upon occurrence of their corresponding SuiteCompleted
  // events.)
  //
  private def locateSuite(orderedEvents: Array[Event],
                          endEvent: Event):
  (Int, Int) = {
    require(orderedEvents.size > 0)
    require(endEvent.isInstanceOf[SuiteCompleted] ||
            endEvent.isInstanceOf[SuiteAborted])

    var startIndex = 0
    var endIndex   = 0
    var idx        = 0

    while ((idx < orderedEvents.size) && (endIndex == 0)) {
      val event = orderedEvents(idx)

      event match {
        case e: SuiteStarting =>
          startIndex = idx

        case e: SuiteCompleted =>
          if (event == endEvent) {
            endIndex = idx
            assert(
              e.suiteName ==
                orderedEvents(startIndex).asInstanceOf[SuiteStarting].
                suiteName)
          }

        case e: SuiteAborted =>
          if (event == endEvent) {
            endIndex = idx
            assert(
              e.suiteName ==
                orderedEvents(startIndex).asInstanceOf[SuiteStarting].
                suiteName)
          }

        case _ =>
      }
      idx += 1
    }
    assert(endIndex > 0)
    assert(orderedEvents(startIndex).isInstanceOf[SuiteStarting])

    (startIndex, endIndex)
  }

  //
  // Constructs a Testcase object from events in orderedEvents array.
  //
  // Accepts a TestStarting event and its index within orderedEvents.
  // Returns a Testcase object plus the index to its corresponding
  // test completion event.  Removes events from class's events Set
  // as they are processed.
  //
  private def processTest(orderedEvents: Array[Event],
                          startEvent: TestStarting, startIndex: Int):
  (Int, Testcase) = {
    val testcase = Testcase(startEvent.testName, startEvent.suiteClassName,
                            startEvent.timeStamp)
    var endIndex = 0
    var idx = startIndex + 1

    while ((idx < orderedEvents.size) && (endIndex == 0)) {
      val event = orderedEvents(idx)
      events -= event

      event match {
        case e: TestSucceeded =>
          endIndex = idx
          testcase.time = e.timeStamp - testcase.timeStamp

        case e: TestFailed =>
          endIndex = idx
          testcase.failure = Some(e)
          testcase.time = e.timeStamp - testcase.timeStamp

        case e: TestPending =>
          endIndex = idx
          testcase.pending = true

        case e: TestCanceled =>
          endIndex = idx
          testcase.canceled = true

        case e: SuiteCompleted => unexpected(e)
        case e: TestStarting   => unexpected(e)
        case e: TestIgnored    => unexpected(e)
        case e: InfoProvided   => unexpected(e)
        case e: AlertProvided  => unexpected(e)
        case e: NoteProvided   => unexpected(e)
        case e: ScopeOpened    => unexpected(e) // TODO: Ask George to verify
        case e: ScopeClosed    => unexpected(e)
        case e: ScopePending   => unexpected(e)
        case e: MarkupProvided => unexpected(e)
        case e: SuiteStarting  => unexpected(e)
        case e: RunStarting    => unexpected(e)
        case e: RunCompleted   => unexpected(e)
        case e: RunStopped     => unexpected(e)
        case e: RunAborted     => unexpected(e)
        case e: SuiteAborted   => unexpected(e)
        case e: DiscoveryStarting  => unexpected(e)
        case e: DiscoveryCompleted => unexpected(e)
      }
    }
    (endIndex, testcase)
  }

  //
  // Constructs a Testcase object for an ignored test.
  //
  private def ignoreTest(testIgnoredEvent: TestIgnored): Testcase = {
    val testcase = Testcase(testIgnoredEvent.testName,
                            testIgnoredEvent.suiteClassName,
                            testIgnoredEvent.timeStamp)
    testcase.time = 0
    testcase.ignored = true
    testcase
  }

  //
  // Creates an xml string describing a run of a test suite.
  //
  def xmlify(testsuite: Testsuite): String = {
    val xmlVal =
      <testsuite
        errors    = { "" + testsuite.errors         }
        failures  = { "" + testsuite.failures       }
        hostname  = { "" + findHostname             }
        name      = { "" + testsuite.name           }
        tests     = { "" + testsuite.testcases.size }
        time      = { "" + testsuite.time / 1000.0  }
        timestamp = { "" + formatTimeStamp(testsuite.timeStamp) }>
      { propertiesXml }
      {
        for (testcase <- testsuite.testcases) yield {
          <testcase
            name      = { "" + testcase.name              }
            classname = { "" + strVal(testcase.className) }
            time      = { "" + testcase.time / 1000.0     }
            pending   = { "" + testcase.pending           }
            ignored   = { "" + testcase.ignored           }
          >
          {
            failureXml(testcase.failure)
          }
          </testcase>
        }
      }
        <system-out><![CDATA[]]></system-out>
        <system-err><![CDATA[]]></system-err>
      </testsuite>

    val prettified = (new xml.PrettyPrinter(76, 2)).format(xmlVal)

    // scala xml strips out the <![CDATA[]]> elements, so restore them here
    val withCDATA =
      prettified.
        replace("<system-out></system-out>",
                "<system-out><![CDATA[]]></system-out>").
        replace("<system-err></system-err>",
                "<system-err><![CDATA[]]></system-err>")

    "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n" + withCDATA
  }

  //
  // Returns string representation of stack trace for specified Throwable,
  // including any nested exceptions.
  //
  def getStackTrace(throwable: Throwable): String = {
    "" + throwable +
    Array.concat(throwable.getStackTrace).mkString("\n      at ",
                                                   "\n      at ", "\n") +
    {
      if (throwable.getCause != null) {
        "      Cause: " +
        getStackTrace(throwable.getCause)
      }
      else ""
    }
  }

  //
  // Generates <failure> xml for TestFailed event, if specified Option
  // contains one.
  //
  private def failureXml(failureOption: Option[TestFailed]): xml.NodeSeq = {
    failureOption match {
      case None =>
        xml.NodeSeq.Empty

      case Some(failure) =>
        val (throwableType, throwableText) =
          failure.throwable match {
            case None => ("", "")

            case Some(throwable) =>
              val throwableType = "" + throwable.getClass
              val throwableText = getStackTrace(throwable)
              (throwableType, throwableText)
          }
        
        <failure message = { { unparsedXml(failure.message.replaceAll("\n", "&#010;")) } }
                 type    = { throwableType   } >
          { throwableText }
        </failure>
    }
  }

  //
  // Returns toString value of option contents if Some, or empty string if
  // None.
  //
  private def strVal(option: Option[Any]): String = {
    option match {
      case Some(x) => "" + x
      case None    => ""
    }
  }

  //
  // Determines hostname of local machine.
  //
  private def findHostname: String = {
    val localMachine =
      try {
        InetAddress.getLocalHost();
      } catch {
      case e: UnknownHostException =>
        throw new RuntimeException("unexpected unknown host")
      }
    localMachine.getHostName
  }

  //
  // Generates <properties> element of xml.
  //
  private def genPropertiesXml: xml.Elem = {
    val sysprops = System.getProperties

    <properties> {
      for (name <- propertyNames(sysprops))
        yield
          <property name={ name } value = { sysprops.getProperty(name) }>
          </property>
    }
    </properties>
  }

  //
  // Returns a list of the names of properties in a Properties object.
  //
  private def propertyNames(props: Properties): List[String] = {
    val listBuf = new ListBuffer[String]

    val enumeration = props.propertyNames

    while (enumeration.hasMoreElements)
      listBuf += "" + enumeration.nextElement

    listBuf.toList
  }

  //
  // Formats timestamp into a string for display, e.g. "2009-08-31T14:59:37"
  //
  private def formatTimeStamp(timeStamp: Long): String = {
    val dateFmt = new SimpleDateFormat("yyyy-MM-dd")
    val timeFmt = new SimpleDateFormat("HH:mm:ss")
    dateFmt.format(timeStamp) + "T" + timeFmt.format(timeStamp)
  }

  //
  // Throws an exception if an unexpected Event is encountered.
  //
  def unexpected(event: Event) {
    throw new RuntimeException("unexpected event [" + event + "]")
  }

  //
  // Class to hold information about an execution of a test suite.
  //
  private case class Testsuite(name: String, timeStamp: Long) {
    var errors   = 0
    var failures = 0
    var time     = 0L
    val testcases = new ListBuffer[Testcase]
  }

  //
  // Class to hold information about an execution of a testcase.
  //
  private case class Testcase(name: String, className: Option[String],
                              timeStamp: Long) {
    var time = 0L
    var pending = false
    var canceled = false
    var ignored = false
    var failure: Option[TestFailed] = None
  }
}
