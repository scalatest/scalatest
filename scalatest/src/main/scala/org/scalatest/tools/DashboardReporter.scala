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

import org.scalatest.events._
import org.scalatest.Reporter
import org.scalatest.events.MotionToSuppress
import org.scalatest.exceptions.StackDepthException

import java.io.PrintWriter
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.File
import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat
import java.util.regex.Matcher.quoteReplacement

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.xml.XML
import scala.xml.NodeSeq
import scala.xml.Elem
import scala.xml.Node

/**
 * A <code>Reporter</code> that writes test status information in xml format
 * for use by Flex formatter.
 */
private[scalatest] class DashboardReporter(directory: String,
                                           numOldFilesToKeep: Int)
  extends Reporter
{
  final val BufferSize = 4096

  private val events = ListBuffer[Event]()
  private var index = 0
  private var timestamp = "" // gets set at RunStarting event

  private final val TimestampPattern = """\d{4}-\d{2}-\d{2}-\d{6}-\d{3}"""

  private val runsDir          = new File(directory + "/runs")
  private val durationsDir     = new File(directory + "/durations")
  private val summariesDir     = new File(directory + "/summaries")
  private val summaryFile      = new File(directory + "/summary.xml")
  private val durationsFile    = new File(directory + "/durations.xml")

  runsDir.mkdir()
  durationsDir.mkdir()
  summariesDir.mkdir()

  //
  // Records events as they are received.  Initiates processing once
  // a run-termination event comes in.
  //
  // Ignores info and markup events.
  //
  def apply(event: Event) {
    event match {
      case _: DiscoveryStarting  =>
      case _: DiscoveryCompleted =>
      case _: RunStarting        => timestamp = formatCurrentTime
      case _: InfoProvided       =>
      case _: AlertProvided      =>
      case _: NoteProvided       =>
      case _: ScopeOpened        =>
      case _: ScopeClosed        =>
      case _: ScopePending       =>
      case _: MarkupProvided     =>
      case _: RunCompleted       => writeFiles(event)
      case _: RunStopped         => writeFiles(event)
      case _: RunAborted         => writeFiles(event)
      case _ => events += event
    }
  }

  //
  // Formats current time for use as timestamp to identify current run.
  //
  // Uses GMT time zone to avoid sequencing errors that could occur with
  // time shifts due to daylight savings time or laptop travel if local
  // time zone were used.
  //
  def formatCurrentTime: String = {
    val df = new SimpleDateFormat("yyyy-MM-dd-HHmmss-SSS")
    df.setTimeZone(TimeZone.getTimeZone("GMT"))
    df.format(new Date)
  }

  //
  // Provides sequential index values for xml entries.
  //
  def nextIndex(): Int = {
    index += 1
    index
  }

  //
  // Throws exception for specified unexpected event.
  //
  def unexpectedEvent(e: Event) {
    throw new RuntimeException("unexpected event [" + e + "]")
  }

  //
  // Escapes html entities and curly braces in specified string.
  //
  def escape(s: String): String =
    scala.xml.Utility.escape(s).
      replaceAll("""\{""", """\\{""").
      replaceAll("""\}""", """\\}""")

  //
  // Formats date for inclusion in as 'date' attribute in xml.
  //
  // E.g.: "Mon May 30 10:29:58 PDT 2011"
  //
  def formatDate(timeStamp: Long): String = {
    val df = new SimpleDateFormat("EEE MMM d kk:mm:ss zzz yyyy")
    df.format(new Date(timeStamp))
  }

  //
  // Reads existing summary.xml file, or, if none exists, returns a <summary>
  // xml containing all empty elements.
  //
  def getOldSummaryXml: Elem = {
    if (summaryFile.exists)
      XML.loadFile(summaryFile)
    else
      <summary>
        <runs/>
        <regressions/>
        <recentlySlower/>
      </summary>
  }

  //
  // If a summary file containing previous run histories exists, moves
  // it to the summaries/ subdirectory and renames it to a filename
  // containing the timestamp of the most recent run the file contains.
  //
  // Ditto for the durations.xml file.
  //
  def archiveOldFiles(oldRunsXml: NodeSeq) {
    val previousRunTimestamp = 
      if (oldRunsXml.size > 0) Some("" + oldRunsXml(0) \ "@id")
      else None

    if (previousRunTimestamp.isDefined) {
      if (summaryFile.exists)
        summaryFile.renameTo(
          new File(
            summariesDir + "/summary-" + previousRunTimestamp.get + ".xml"))

      if (durationsFile.exists)
        durationsFile.renameTo(
          new File(
            durationsDir + "/duration-" + previousRunTimestamp.get + ".xml"))

      purgeDir(summariesDir, "summary-")
      purgeDir(durationsDir, "duration-")
    }
  }

  //
  // Deletes older files from specified archive directory.
  //
  // We keep timestamped old copies of summary.xml and durations.xml in
  // summaries/ and durations/ subdirectories.  This method trims the
  // oldest archived files from those directories to maintain a specified
  // maximum number of archived copies.
  //
  def purgeDir(directory: File, prefix: String) {
    directory.
      listFiles().
      filter(_.getName.matches(prefix + TimestampPattern + """\.xml""")).
      sorted.
      dropRight(numOldFilesToKeep).
      foreach(_.delete())
  }

  //
  // Writes dashboard reporter summary, duration, and run files at completion
  // of a run.  Archives old copies of summary and duration files into
  // summaries/ and durations/ subdirectories.
  //
  def writeFiles(terminatingEvent: Event) {
    val durations     = Durations(durationsFile)
    val oldSummaryXml = getOldSummaryXml
    val oldRunsXml    = oldSummaryXml \\ "run"

    archiveOldFiles(oldRunsXml)

    val thisRunFile = new File(runsDir, "run-" + timestamp + ".xml")

    writeRunFile(terminatingEvent, thisRunFile)
    val thisRunXml = XML.loadFile(thisRunFile)

    durations.addTests(timestamp, thisRunXml)

    writeDurationsFile(durations)
    writeSummaryFile(
      terminatingEvent, oldSummaryXml, oldRunsXml, thisRunXml, durations)
  }

  //
  // Writes the durations.xml file.
  //
  def writeDurationsFile(durations: Durations) {
    writeFile("durations.xml", durations.toXml)
  }

  //
  // Writes the summary.xml file.
  //
  def writeSummaryFile(terminatingEvent: Event, oldSummaryXml: NodeSeq,
                       oldRunsXml: NodeSeq, thisRunXml: NodeSeq,
                       durations: Durations)
  {
    val SummaryTemplate =
      """|<summary>
         |  <runs>
         |$runs$  </runs>
         |  <regressions>
         |$regressions$  </regressions>
         |  <recentlySlower>
         |$recentlySlower$  </recentlySlower>
         |</summary>
         |""".stripMargin

    //
    // Formats a <run> element of summary file.
    //
    def formatRun(id: String, succeeded: String, failed: String,
                  ignored: String, canceled: String, pending: String): String =
    {
      "    <run "     +
      "id=\""         + id         + "\" " +
      "succeeded=\""  + succeeded  + "\" " +
      "failed=\""     + failed     + "\" " +
      "ignored=\""    + ignored    + "\" " +
      "canceled=\""   + canceled   + "\" " +
      "pending=\""    + pending    + "\" " + "/>\n"
    }

    //
    // Generates the summary file <run> element for the current run.
    //
    def genThisRun(terminatingEvent: Event): String = {
      val summaryOption = 
        terminatingEvent match {
          case e: RunCompleted => e.summary
          case e: RunAborted   => e.summary
          case e: RunStopped   => e.summary
          case _ => unexpectedEvent(terminatingEvent); None
        }
  
      val summary  = summaryOption.getOrElse(Summary(0, 0, 0, 0, 0, 0, 0, 0))
  
      formatRun(timestamp,
                "" + summary.testsSucceededCount,
                "" + summary.testsFailedCount,
                "" + summary.testsIgnoredCount,
                "" + summary.testsCanceledCount,
                "" + summary.testsPendingCount)
    }

    //
    // Formats <run> elements for previous runs.
    //
    // (We could let scala do this its way, but it makes kind of a mess.)
    //
    def formatOldRuns(oldRunsXml: NodeSeq): String = {
      val buf = new StringBuilder

      for (run <- oldRunsXml) {
        val id        = "" + (run \ "@id")
        val succeeded = "" + (run \ "@succeeded")
        val failed    = "" + (run \ "@failed")
        val ignored   = "" + (run \ "@ignored")
        val canceled  = "" + (run \ "@canceled")
        val pending   = "" + (run \ "@pending")

        buf.append(
          formatRun(id, succeeded, failed, ignored, canceled, pending))
      }
      buf.toString
    }

    //
    // Generates <regressedTest> elements for output summary file.
    //
    def genRegressions(oldSummaryXml: NodeSeq, thisRunXml: NodeSeq): String =
    {
      //
      // Searches through regressions from previous summary to try and
      // find one matching specified test.
      //
      def getOldRegression(suite: Node, test: Node,
                           oldRegressionsXml: NodeSeq): Option[Node] =
      {
        oldRegressionsXml.find(
          node => (((node \ "@testName") == (test \ "@name")) &&
                   ((node \ "@suiteId") == (suite \ "@id"))))
      }

      //
      // Formats a <regressedTest> element.
      //
      def formatRegression(suite: Node, test: Node, result: String,
                           lastSucceeded: String, firstRegressed: String):
      String =
      {
        "    <regressedTest " +
        "suiteId=\""        + (suite \ "@id")    + "\" " +
        "suiteName=\""      + (suite \ "@name")  + "\" " +
        "testName=\""       + (test \ "@name")   + "\" " +
        "status=\""         + result             + "\" " +
        "firstRegressed=\"" + firstRegressed     + "\" " +
        "lastSucceeded=\""  + lastSucceeded      + "\"/>\n"
      }

      //
      // Gets the timestamp of the previous run.
      //
      def getLastRunId: Option[String] = {
        val previousRuns = oldSummaryXml \\ "run"

        if (previousRuns.size > 0) Some("" + (previousRuns(0) \ "@id"))
        else None
      }

      //
      // Retrieves xml of the previous run if available, else Empty.
      //
      def getLastRunXml(lastRunId: Option[String]): NodeSeq = {
        if (lastRunId.isDefined)
          XML.loadFile(directory + "/runs/run-" + lastRunId.get + ".xml")
        else
          NodeSeq.Empty
      }

      //
      // Checks xml from previous run to see if it contains a test with
      // success status that matches specified test.
      //
      def lastRunSucceeded(suite: Node, test: Node, lastRunXml: NodeSeq):
      Boolean =
      {
        var found = false
        var succeeded = false

        val oldSuitesIt = (lastRunXml \\ "suite").iterator
        while (!found && oldSuitesIt.hasNext) {
          val oldSuite = oldSuitesIt.next()

          if (oldSuite \ "@id" == suite \ "@id") {
            val oldTests = oldSuite \ "test"
            val matchingTest =
              oldTests.find(node => (node \ "@name") == test \ "@name")
            if (matchingTest.isDefined) {
              found = true
              val result = "" + matchingTest.get \ "@result"
              succeeded = (result == "succeeded")
            }
          }
        }
        succeeded
      }

      //
      // genRegressions main
      //
      val buf = new StringBuilder
      val suites = thisRunXml \\ "suite"
      val oldRegressionsXml = oldSummaryXml \\ "regressedTest"
      val lastRunId = getLastRunId
      val lastRunXml = getLastRunXml(lastRunId)

      for (suite <- suites) {
        for (test <- suite \ "test") {
          val result = "" + (test \ "@result")

          if (result != "succeeded") {
            val oldRegression = getOldRegression(suite, test,
                                                 oldRegressionsXml)
            val lastSucceeded =
              if (oldRegression.isDefined)
                "" + oldRegression.get \ "@lastSucceeded"
              else if (lastRunSucceeded(suite, test, lastRunXml))
                lastRunId.get
              else
                "never"

            val firstRegressed =
              if (oldRegression.isDefined)
                "" + oldRegression.get \ "@firstRegressed"
              else
                timestamp

            if (!((result == "pending") && (lastSucceeded == "never")))
              buf.append(
                formatRegression(
                  suite, test, result, lastSucceeded, firstRegressed))
          }
        }
      }
      buf.toString
    }

    def genRecentlySlower(durations: Durations): String = {
      var slowRecords = List[SlowRecord]()

      case class SlowRecord(suite: Durations#Suite, test: Durations#Test,
                            oldAvg: Int, newAvg: Int, percentSlower: Int)
      {
        def toXml: String = {
          val SlowerTestTemplate =
            """      <slowerTest suiteId="$suiteId$" """ +
            """suiteName="$suiteName$" testName="$testName$" """ +
            """|oldAvg="$oldAvg$" newAvg="$newAvg$"/>
               |""".stripMargin

        SlowerTestTemplate.
          replaceFirst("""\$suiteId\$""",   quoteReplacement(suite.suiteId)).
          replaceFirst("""\$suiteName\$""", quoteReplacement(suite.suiteName)).
          replaceFirst("""\$testName\$""",  quoteReplacement(test.name)).
          replaceFirst("""\$oldAvg\$""",    "" + oldAvg).
          replaceFirst("""\$newAvg\$""",    "" + newAvg)
        }
      }

      def toXml: String = {
        val buf = new StringBuilder

        for (slowRecord <- slowRecords) buf.append(slowRecord.toXml)

        buf.toString
      }

      for (suite <- durations.suites) {
        for (test <- suite.tests) {
          if (test.runCount > 10) {
            val oldAvg = test.previousAverage
            val newAvg = test.computeNewAvg

            if ((newAvg - oldAvg > 1) && (oldAvg > 0)) {
              val percentSlower =
                (((newAvg - oldAvg).toDouble / oldAvg.toDouble) * 100).toInt

              if (percentSlower > 10)
                slowRecords ::=
                  SlowRecord(suite, test, oldAvg, newAvg, percentSlower)
            }
          }
        }
      }
      slowRecords =
        slowRecords.sortBy(r => r.percentSlower).take(20)

      toXml
    }

    //
    // writeSummaryFile main
    //
    val thisRun        = genThisRun(terminatingEvent)
    val oldRuns        = formatOldRuns(oldRunsXml)
    val regressions    = genRegressions(oldSummaryXml, thisRunXml)
    val recentlySlower = genRecentlySlower(durations)

    val summaryText =
      SummaryTemplate.
        replaceFirst("""\$runs\$""",     quoteReplacement(thisRun + oldRuns)).
        replaceFirst("""\$regressions\$""", quoteReplacement(regressions)).
        replaceFirst("""\$recentlySlower\$""",
                     quoteReplacement(recentlySlower))

    writeFile("summary.xml", summaryText)
  }

  //
  // Writes specified text to specified file in output directory.
  //
  def writeFile(filename: String, text: String) {
    val out = new PrintWriter(directory + "/" + filename)
    out.print(text)
    out.close()
  }

  //
  // Writes timestamped output file to 'runs' subdirectory beneath specified
  // output dir.  Format of file name is, e.g. for timestamp
  // "2011-10-24-105759-563", "run-2011-10-24-105759-563.xml".
  //
  // We write the file piece-by-piece directly, instead of creating a string
  // and writing that, 
  //
  def writeRunFile(event: Event, thisRunFile: File) {
    index = 0
    var suiteRecord: SuiteRecord = null
    val stack = new Stack[SuiteRecord]
    val pw =
      new PrintWriter(
        new BufferedOutputStream(
          new FileOutputStream(thisRunFile), BufferSize))

    //
    // Formats <summary> element of output xml.
    //
    def formatSummary(event: Event): String = {
      val (summaryOption, durationOption) =
        event match {
          case e: RunCompleted => (e.summary, e.duration)
          case e: RunAborted   => (e.summary, e.duration)
          case e: RunStopped   => (e.summary, e.duration)
          case _ => unexpectedEvent(event); (None, None)
        }

      val summary  = summaryOption.getOrElse(Summary(0, 0, 0, 0, 0, 0, 0, 0))
      val duration = durationOption.getOrElse(0)

      "<summary index=\"" + nextIndex() + "\" text=\"\" " +
      "duration=\""             + duration                     + "\" " +
      "testsSucceededCount=\""  + summary.testsSucceededCount  + "\" " +
      "testsFailedCount=\""     + summary.testsFailedCount     + "\" " +
      "testsIgnoredCount=\""    + summary.testsIgnoredCount    + "\" " +
      "testsPendingCount=\""    + summary.testsPendingCount    + "\" " +
      "testsCancelledCount=\""  + summary.testsCanceledCount   + "\" " +
      "suitesCompletedCount=\"" + summary.suitesCompletedCount + "\" " +
      "suitesAbortedCount=\""   + summary.suitesAbortedCount   + "\" " +
      "date=\""                 + formatDate(event.timeStamp)  + "\" " +
      "thread=\""               + event.threadName             + "\"/>\n"
    }

    //
    // Closes out a SuiteRecord.  Gets called upon receipt of a
    // SuiteCompleted or SuiteAborted event.
    //
    // If the suite being closed is nested within another suite, its
    // completed record is added to the record of the suite it is nested
    // in.  Otherwise its xml is written to the output file.
    //
    def endSuite(e: Event) {
      suiteRecord.addEndEvent(e)

      val prevRecord = stack.pop()

      if (prevRecord != null)
        prevRecord.addNestedElement(suiteRecord)
      else
        pw.print(suiteRecord.toXml)

      suiteRecord = prevRecord
    }

    //
    // writeRunFile main
    //
    pw.println("<doc>")
    pw.print(formatSummary(event))

    for (event <- events.sorted) {
      event match {
        case e: SuiteStarting  =>
          stack.push(suiteRecord)
          suiteRecord = new SuiteRecord(e)
          
        case e: TestStarting   => suiteRecord.addNestedElement(e)
        case e: TestSucceeded  => suiteRecord.addNestedElement(e)
        case e: TestIgnored    => suiteRecord.addNestedElement(e)
        case e: TestFailed     => suiteRecord.addNestedElement(e)
        case e: TestPending    => suiteRecord.addNestedElement(e)
        case e: TestCanceled   => suiteRecord.addNestedElement(e)

        case e: SuiteCompleted => endSuite(e)
        case e: SuiteAborted   => endSuite(e)

        case e: DiscoveryStarting  => unexpectedEvent(e)
        case e: DiscoveryCompleted => unexpectedEvent(e)
        case e: RunStarting        => unexpectedEvent(e)
        case e: RunCompleted       => unexpectedEvent(e)
        case e: RunStopped         => unexpectedEvent(e)
        case e: RunAborted         => unexpectedEvent(e)
        case e: InfoProvided       => unexpectedEvent(e)
        case e: AlertProvided      => unexpectedEvent(e)
        case e: NoteProvided       => unexpectedEvent(e)
        case e: ScopeOpened        => unexpectedEvent(e)
        case e: ScopeClosed        => unexpectedEvent(e)
        case e: ScopePending       => unexpectedEvent(e)
        case e: MarkupProvided     => unexpectedEvent(e)
      }
    }
    pw.println("</doc>")
    pw.flush()
    pw.close()
  }

  //
  // Generates xml for a TestIgnored event.
  //
  def formatTestIgnored(event: TestIgnored): String = {
    "<test index=\"" + nextIndex() + "\" " +
    "result=\"ignored\" " +
    "text=\"" + testMessage(event.testName, event.formatter) + "\" " +
    "name=\"" + escape(event.testName) + "\" " +
    "thread=\"" + event.threadName + "\"" +
    "/>\n"
  }

  //
  // Extracts message from specified formatter if there is one, otherwise
  // returns test name.
  //
  def testMessage(testName: String, formatter: Option[Formatter]): String = {
    val message =
      formatter match {
        case Some(IndentedText(_, rawText, _)) => rawText
        case _ => testName
      }
    escape(message)
  }

  //
  // Class that aggregates events that make up a suite.
  //
  // Holds all the events encountered from SuiteStarting through its
  // corresponding end event (e.g. SuiteCompleted).  Once the end event
  // is received, this class's toXml method can be called to generate the
  // complete xml string for the <suite> element.
  //
  class SuiteRecord(startEvent: SuiteStarting) {
    var nestedElements = List[Any]()
    var endEvent: Event = null

    //
    // Adds either an Event or a nested SuiteRecord to this object's
    // list of elements.
    //
    def addNestedElement(element: Any) {
      nestedElements ::= element
    }

    //
    // Adds suite closing event (SuiteCompleted or SuiteAborted) to the
    // object.
    //
    def addEndEvent(event: Event) {
      def isEndEvent(e: Event): Boolean = {
        e match {
          case _: SuiteCompleted => true
          case _: SuiteAborted   => true
          case _ => false
        }
      }

      require(endEvent == null)
      require(isEndEvent(event))

      endEvent = event
    }

    //
    // Generates value to be used in <suite> element's 'result' attribute.
    //
    def result: String = {
      endEvent match {
        case _: SuiteCompleted => "completed"
        case _: SuiteAborted   => "aborted"
        case _ => unexpectedEvent(endEvent); ""
      }
    }

    //
    // Generates xml string representation of SuiteRecord object.
    //
    def toXml: String = {
      val buf = new StringBuilder
      var testRecord: TestRecord = null

      //
      // Generates opening <suite ...> element
      //
      def formatStartOfSuite: String = {
        val duration = endEvent.timeStamp - startEvent.timeStamp
        "\n" +
        "<suite index=\"" + nextIndex()                   + "\" " +
        "id=\""           + startEvent.suiteId            + "\" " +
        "result=\""       + result                        + "\" " +
        "name=\""         + escape(startEvent.suiteName)  + "\" " +
        "duration=\""     + duration                      + "\" " +
        "thread=\""       + startEvent.threadName         + "\">\n"
      }

      //
      // Indicates whether a test record is currently open during
      // event processing.
      //
      def inATest: Boolean =
        (testRecord != null) && (testRecord.endEvent == null)

      //
      // toXml main
      //
      if (startEvent.suiteName != "DiscoverySuite")
        buf.append(formatStartOfSuite)

      for (element <- nestedElements.reverse) {
        if (inATest) {
          testRecord.addEvent(element.asInstanceOf[Event])

          if (testRecord.isComplete)
            buf.append(testRecord.toXml)
        }
        else {
          element match {
            case e: TestIgnored    => buf.append(formatTestIgnored(e))
            case e: SuiteRecord    => buf.append(e.toXml)
            case e: TestStarting   => testRecord = new TestRecord(e)
            case _ =>
              throw new RuntimeException("unexpected [" + element + "]")
          }
        }
      }
      if (startEvent.suiteName != "DiscoverySuite")
        buf.append("</suite>\n")

      buf.toString
    }
  }

  //
  // Class that aggregates events that make up a test.
  //
  // Holds all the events encountered from TestStarting through its
  // corresponding end event (e.g. TestSucceeded).  Once the end event
  // is received, this class's toXml method can be called to generate
  // the complete xml string for the <test> element.
  //
  // (We no longer record info-provided or markup events for tests,
  // so nested events within the TestRecord have been removed.)
  //
  class TestRecord(startEvent: TestStarting) {
    var endEvent: Event = null

    //
    // Adds specified event to object's list of nested events.
    //
    def addEvent(event: Event) {
      def isEndEvent: Boolean = {
        event match {
          case _: TestSucceeded => true
          case _: TestFailed => true
          case _: TestPending => true
          case _: TestCanceled => true
          case _ => false
        }
      }

      if (isEndEvent)
        endEvent = event
      else
        unexpectedEvent(event)
    }

    //
    // Indicates whether an end event has been received yet for this
    // record.
    //
    def isComplete: Boolean = (endEvent != null)

    //
    // Generates value for use as 'result' attribute of <test> element.
    //
    def result: String = {
      endEvent match {
        case _: TestSucceeded => "succeeded"
        case _: TestFailed    => "failed"
        case _: TestPending   => "pending"
        case _: TestCanceled  => "canceled"
        case _ => unexpectedEvent(endEvent); ""
      }
    }

    object Duration {
      def unapply(event: Event): Option[Long] =
        event match {
          case TestSucceeded(_, _, _, _, _, _, _, duration, _, _, _, _, _, _)
            => duration
          case TestFailed(_, _, _, _, _, _, _, _, _, duration, _, _, _, _, _, _) 
            => duration
          case TestPending(_, _, _, _, _, _, _, duration, _, _, _, _, _) 
            => duration
          case TestCanceled(_, _, _, _, _, _, _, _, _, duration, _, _, _, _, _, _) 
            => duration
          case _ => None
        }
    }

    //
    // Generates initial <test> element of object's xml.
    //
    def formatTestStart: String = {
      val duration = 
        endEvent match {
          case Duration(d) => d
          case _ => endEvent.timeStamp - startEvent.timeStamp
        }

      "<test index=\"" + nextIndex()                 + "\" " +
      "result=\""      + result                      + "\" " +
      "text=\""        + testMessage(startEvent.testName, endEvent.formatter) +
      "\" " +
      "name=\""        + escape(startEvent.testName) + "\" " +
      "duration=\""    + duration                    + "\" " +
      "thread=\""      + startEvent.threadName       + "\"" +
      ">\n"
    }

    //
    // Generates <exception> xml for a test failure.
    //
    def formatException(event: TestFailed): String = {
      val buf = new StringBuilder
      var depth = -1

      def nextDepth: Int = {
        depth += 1
        depth
      }

      buf.append("<exception ")

      if (event.suiteClassName.isDefined)
        buf.append("className=\"" + event.suiteClassName.get + "\"")

      buf.append(">\n")
      
      if (event.throwable.isDefined) {
        val throwable = event.throwable.get
        val stackTrace = throwable.getStackTrace
        require(stackTrace.size > 0)

        // PCData will enclose the message in CDATA.
        buf.append("<message>" + scala.xml.PCData(event.message) + "</message>\n")

        if (throwable.isInstanceOf[StackDepthException]) {
          val sde = throwable.asInstanceOf[StackDepthException]

          if (sde.failedCodeFileName.isDefined &&
              sde.failedCodeLineNumber.isDefined)
          {
            buf.append(
              "<stackDepth>\n" +
              "<depth>" + sde.failedCodeStackDepth + "</depth>\n" +
              "<fileName>" + sde.failedCodeFileName.get + "</fileName>\n" +
              "<lineNumber>" +
                sde.failedCodeLineNumber.get +
              "</lineNumber>\n" +
              "</stackDepth>\n")
          }
        }

        buf.append("<stackTrace>\n")
        for (frame <- stackTrace) {
          buf.append(
            "<stackFrame depth=\"" + nextDepth + "\">" +
              frame.getClassName + "(" + frame.getFileName + ":" +
              frame.getLineNumber + ")" +
            "</stackFrame>\n")
        }
        buf.append("</stackTrace>\n")
      }
      buf.append("</exception>\n")

      buf.toString
    }

    //
    // Generates xml string representation of object.
    //
    def toXml: String = {
      val buf = new StringBuilder

      if (endEvent == null)
        throw new IllegalStateException("toXml called without endEvent")

      buf.append(formatTestStart)

      if (endEvent.isInstanceOf[TestFailed])
        buf.append(formatException(endEvent.asInstanceOf[TestFailed]))

      buf.append("</test>\n")

      buf.toString
    }
  }
}

