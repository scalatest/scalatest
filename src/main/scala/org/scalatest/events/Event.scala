/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.events

import org.scalatest._
import java.util.Date
import scala.xml.Elem
import java.io.StringWriter
import java.io.PrintWriter
import java.io.BufferedWriter

import exceptions.StackDepthException

/**
 * A base class for the events that can be passed to the report function passed
 * to the <code>execute</code> method of a <code>Suite</code>.
 *
 * @author Bill Venners
 */
sealed abstract class Event extends Ordered[Event] with java.io.Serializable {

  /**
   * An <code>Ordinal</code> that can be used to place this event in order in the context of
   * other events reported during the same run.
   */
  val ordinal: Ordinal

  /**
   * An optional formatter that provides extra information that can be used by reporters in determining
   * how to present this event to the user.
   */
  val formatter: Option[Formatter]

  /**
   * An optional location that provides information indicating where in the source code an event originated.
   * IDEs can use this information, for example, to allow the user to hop from an event report to the relevant
   * line of source code.
   */
  val location: Option[Location]

  /**
   * An optional object that can be used to pass custom information to the reporter about this event.
   */
  val payload: Option[Any]

  /**
   * A name for the <code>Thread</code> about whose activity this event was reported.
   */
  val threadName: String

  /**
   * A <code>Long</code> indicating the time this event was reported, expressed in terms of the
   * number of milliseconds since the standard base time known as "the epoch":
   * January 1, 1970, 00:00:00 GMT.
   */
  val timeStamp: Long

  /**
   * Comparing <code>this</code> event with the event passed as <code>that</code>. Returns
   * x, where x < 0 iff this < that, x == 0 iff this == that, x > 0 iff this > that.
   *
   * @param that the event to compare to this event
   * @param return an integer indicating whether this event is less than, equal to, or greater than
   * the passed event
   */
  def compare(that: Event): Int = ordinal.compare(that.ordinal)
  
  /**
   * 
   */
  private [scalatest] def toXml: Elem
  
  private[events] object EventXmlHelper {
    def stringOption(strOption: Option[String]) = strOption.getOrElse("")
    def longOption(longOption: Option[Long]) = if (longOption.isDefined) longOption.get.toString else ""
    def booleanOption(booleanOption: Option[Boolean]) = if (booleanOption.isDefined) booleanOption.get.toString else ""
    def formatterOption(formatterOption: Option[Formatter]) = {
      formatterOption match {
        case Some(formatter) =>
          formatter match {
            case MotionToSuppress => 
              <MotionToSuppress/>
            case indentedText: IndentedText => 
              <IndentedText>
                 <formattedText>{ indentedText.formattedText }</formattedText>
                 <rawText>{ indentedText.rawText }</rawText>
                 <indentationLevel>{ indentedText.indentationLevel }</indentationLevel>
              </IndentedText>
          }
        case None => ""
      }
    }
    def locationOption(locationOption: Option[Location]) = {
      locationOption match {
        case Some(location) =>
          location match {
            case topOfClass: TopOfClass =>
              <TopOfClass>
                <className>{ topOfClass.className }</className>
              </TopOfClass>
            case topOfMethod: TopOfMethod => 
              <TopOfMethod>
                <className>{ topOfMethod.className }</className>
                <methodId>{ topOfMethod.methodId }</methodId>
              </TopOfMethod>
            case lineInFile: LineInFile => 
              <LineInFile>
                <lineNumber>{ lineInFile.lineNumber }</lineNumber>
                <fileName>{ lineInFile.fileName }</fileName>
              </LineInFile>
            case SeeStackDepthException => 
              <SeeStackDepthException />
            case _ =>
              ""
          } 
        case None => ""
      }
    }
    def getThrowableStackDepth(throwable: Throwable) = {
      throwable match { 
        case sde: StackDepthException => sde.failedCodeStackDepth 
        case _ => -1
      }
    }
    def throwableOption(throwableOption: Option[Throwable]) = {
      throwableOption match {
        case Some(throwable) => 
          <message>{ throwable.getMessage }</message>
          <depth>{ getThrowableStackDepth(throwable) }</depth>
          <stackTraces>
            {
              val stackTraces = throwable.getStackTrace
              for (stackTrace <- stackTraces) yield {
                <stackTrace>
                  <className>{ stackTrace.getClassName }</className>
                  <methodName>{ stackTrace.getMethodName }</methodName>
                  <fileName>{ stackTrace.getFileName }</fileName>
                  <lineNumber>{ stackTrace.getLineNumber }</lineNumber>
                  <isNative>{ stackTrace.isNativeMethod }</isNative>
                  <toString>{ stackTrace.toString }</toString>
                </stackTrace>
              }
              /*val stringWriter = new StringWriter()
              val writer = new PrintWriter(new BufferedWriter(stringWriter))
              throwable.printStackTrace(writer)
              writer.flush()
              stringWriter.toString*/
            }
          </stackTraces>
        case None => ""
      }
    }
    def summaryOption(summaryOption: Option[Summary]) = {
      summaryOption match {
        case Some(summary) =>
          <testsSucceededCount>{ summary.testsSucceededCount }</testsSucceededCount>
          <testsFailedCount>{ summary.testsFailedCount }</testsFailedCount>
          <testsIgnoredCount>{ summary.testsIgnoredCount }</testsIgnoredCount>
          <testsPendingCount>{ summary.testsPendingCount }</testsPendingCount>
          <testsCanceledCount>{ summary.testsCanceledCount }</testsCanceledCount>
          <suitesCompletedCount>{ summary.suitesCompletedCount }</suitesCompletedCount>
          <suitesAbortedCount>{ summary.suitesAbortedCount }</suitesAbortedCount>
          <scopesPendingCount>{ summary.scopesPendingCount }</scopesPendingCount>
        case None => ""
      }
    }
    def nameInfoOption(nameInfoOption: Option[NameInfo]) = {
      nameInfoOption match {
        case Some(nameInfo) => 
          <suiteName>{ nameInfo.suiteName }</suiteName>
          <suiteId>{ nameInfo.suiteId }</suiteId>
          <suiteClassName>{ stringOption(nameInfo.suiteClassName) }</suiteClassName>
          <testName>{ stringOption(nameInfo.testName) }</testName>
        case None => 
          ""
      }
    }
  }
}

/**
 * Marker trait for test completed event's recordedEvents.
 */
sealed trait RecordableEvent extends Event

/**
 * Event that indicates a suite (or other entity) is about to start running a test.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestStarting</code> to report
 * that a test method of a <code>Suite</code> is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestStarting(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that is starting, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
 * @param testName the name of the test that is starting
 * @param testText the text of the test that is starting (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the test that is starting. (If <code>None</code>
 *        is passed, the test cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestStarting (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestStarting>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestStarting>
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that succeeded.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestSucceeded</code> to report
 * that a test method of a <code>Suite</code> returned normally
 * (without throwing an <code>Exception</code>).
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestSucceeded$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestSucceeded</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestSucceeded(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that has succeeded, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
 * @param testName the name of the test that has succeeded
 * @param testText the text of the test that has succeeded (may be the test name, or a suffix of the test name)
 * @param recordedEvents recorded events in the test.
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the test that has succeeded. (If <code>None</code>
 *        is passed, the test cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestSucceeded</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestSucceeded (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], 
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestSucceeded>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <recordedEvents>{ recordedEvents.map(_.toXml) }</recordedEvents>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestSucceeded>
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that failed.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestFailed</code> to report
 * that a test method of a <code>Suite</code> completed abruptly with an <code>Exception</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestFailed$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestFailed</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestFailed(ordinal, userFriendlyName, message, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param suiteName a localized name identifying the suite containing the test that has failed, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
 * @param testName the name of the test that has failed
 * @param testText the text of the test that has failed (may be the test name, or a suffix of the test name)
 * @param recordedEvents recorded events in the test.
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the test that has failed. (If <code>None</code>
 *        is passed, the test cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestFailed</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestFailed (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], 
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestFailed>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <recordedEvents>{ recordedEvents.map(_.toXml) }</recordedEvents>
      <throwable>{ throwableOption(throwable) }</throwable>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestFailed>
}

/**
 * Event that indicates a suite (or other entity) has ignored a test.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestIgnored</code> to report
 * that a test method of a <code>Suite</code> was ignored because it was annotated with <code>@Ignore</code>. 
 * Ignored tests will not be run, but will usually be reported as reminder to fix the broken test.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestIgnored$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestIgnored</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestIgnored(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that was ignored, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was ignored
 * @param testName the name of the test that was ignored
 * @param testText the text of the test that was ignored (may be the test name, or a suffix of the test name)
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestIgnored</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestIgnored (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
    
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestIgnored>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestIgnored>
}

/**
 * Event that indicates a test is pending, <em>i.e.</em>, it hasn't yet been implemented.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="TestPending$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestPending</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestPending(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite containing the test that is pending, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is pending
 * @param testName the name of the test that is pending
 * @param testText the text of the test that is pending (may be the test name, or a suffix of the test name)
 * @param recordedEvents recorded events in the test.
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that is pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestPending</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class TestPending (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], 
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestPending>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <recordedEvents>{ recordedEvents.map(_.toXml) }</recordedEvents>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestPending>
}

/**
 * Event that indicates a test was canceled, <em>i.e.</em>, it couldn't run because some precondition was not met.
 *
 * <p>
 * To create instances of this class you may
 * use the factory methods provided in its <a href="TestCanceled$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestCanceled</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(TestCanceled(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName), testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param suiteName a localized name identifying the suite containing the test that was canceled, suitable for presenting to the user
 * @param suiteId a string ID for the suite containing the test that is starting, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was canceled
 * @param testName the name of the test that was canceled
 * @param testText the text of the test that was canceled (may be the test name, or a suffix of the test name)
 * @param recordedEvents recorded events in the test.
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test was canceled,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required to run the test that was canceled
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestCanceled</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
// TODO: Probably add a rerunnable to TestCanceled
final case class TestCanceled (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], 
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (testText == null)
    throw new NullPointerException("testText was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <TestCanceled>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <testName>{ testName }</testName>
      <testText>{ testText }</testText>
      <recordedEvents>{ recordedEvents.map(_.toXml) }</recordedEvents>
      <throwable>{ throwableOption(throwable) }</throwable>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </TestCanceled>
}

/**
 * Event that indicates a suite of tests is about to start executing.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteStarting</code> to report
 * that the <code>execute</code> method of a <code>Suite</code> is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteStarting(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite that is starting, suitable for presenting to the user
 * @param suiteId a string ID for the suite that is starting, intended to be unique across all suites in a run XXX 
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the suite that is starting. (If <code>None</code>
 *        is passed, the suite cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteStarting (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <SuiteStarting>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </SuiteStarting>
}

/**
 * Event that indicates a suite of tests has completed executing.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteCompleted</code> to report
 * that the <code>execute</code> method of a <code>Suite</code>
 * has returned normally (without throwing a <code>RuntimeException</code>).
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteCompleted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteCompleted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteCompleted(ordinal, userFriendlyName, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param suiteName a localized name identifying the suite that has completed, suitable for presenting to the user
 * @param suiteId a string ID for the suite that has completed, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
 * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the suite that has completed. (If <code>None</code>
 *        is passed, the suite cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteCompleted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteCompleted (
  ordinal: Ordinal,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <SuiteCompleted>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </SuiteCompleted>
}

/**
 * Event that indicates the execution of a suite of tests has aborted, likely because of an error, prior
 * to completion.
 *
 * <p>
 * For example, trait <code>Suite</code> and object <code>Runner</code> use <code>SuiteAborted</code> to report
 * that the <code>execute</code> method of a <code>Suite</code>
 * has completed abruptly with a <code>RuntimeException</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="SuiteAborted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>SuiteAborted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(SuiteAborted(ordinal, userFriendlyName, message, suiteName, Some(thisSuite.getClass.getName)))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param name a localized name identifying the suite that has aborted, which should include the
 *        suite name, suitable for presenting to the user
 * @param message a localized message suitable for presenting to the user
 * @param suiteName a localized name identifying the suite that has aborted, suitable for presenting to the user
 * @param suiteId a string ID for the suite that has aborted, intended to be unique across all suites in a run
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param rerunner an optional <code>String</code> giving the fully qualified name of the class that can be used to rerun the suite that has aborted. (If <code>None</code>
 *        is passed, the suite cannot be rerun.)
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteAborted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class SuiteAborted (
  ordinal: Ordinal,
  message: String,
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  throwable: Option[Throwable] = None,
  duration: Option[Long] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  rerunner: Option[String] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteId == null)
    throw new NullPointerException("suiteId was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (rerunner == null)
    throw new NullPointerException("rerunner was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <SuiteAborted>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <suiteName>{ suiteName }</suiteName>
      <suiteId>{ suiteId }</suiteId>
      <suiteClassName>{ stringOption(suiteClassName) }</suiteClassName>
      <duration>{ longOption(duration) }</duration>
      <throwable>{ throwableOption(throwable) }</throwable>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <rerunner>{ stringOption(rerunner) }</rerunner>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </SuiteAborted>
}

// TODO: Put location as a val set to None
/**
 * Event that indicates a runner is about run a suite of tests.
 *
 * <p>
 * For example, object <code>Runner</code> reports <code>RunStarting</code> to indicate
 * that the first <code>execute</code> method of a run's initial <code>Suite</code>
 * is about to be invoked.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunStarting$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunStarting</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunStarting(ordinal, testCount))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param testCount the number of tests expected during this run
 * @param configMap a <code>ConfigMap</code> of key-value pairs that can be used by custom <code>Reporter</code>s
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStarting</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @throws IllegalArgumentException if <code>testCount</code> is less than zero.
 *
 * @author Bill Venners
 */
final case class RunStarting (
  ordinal: Ordinal,
  testCount: Int,
  configMap: ConfigMap,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
    
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (testCount < 0)
    throw new IllegalArgumentException("testCount was less than zero: " + testCount)
  if (configMap == null)
    throw new NullPointerException("configMap was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <RunStarting>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <testCount>{ testCount }</testCount>
      <configMap>
        { 
          for ((key, value) <- configMap) yield {
            <entry>
              <key>{ key }</key>
              <value>{ value }</value>
            </entry>
          }
        }
      </configMap>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </RunStarting>
}

/**
 * Event that indicates a runner has completed running a suite of tests.
 *
 * <p>
 * <code>Suite</code>'s <code>execute</code> method takes a <code>Stopper</code>, whose <code>stopRequested</code>
 * method indicates a stop was requested. If <code>true</code> is returned by
 * <code>stopRequested</code> while a suite of tests is running, the
 * <code>execute</code> method should promptly
 * return even if that suite hasn't finished running all of its tests.
 * </p>
 *
 * <p>If a stop was requested via the <code>Stopper</code>.
 * <code>Runner</code> will report <code>RunStopped</code>
 * when the <code>execute</code> method of the run's starting <code>Suite</code> returns.
 * If a stop is not requested, <code>Runner</code> will report <code>RunCompleted</code>
 * when the last <code>execute</code> method of the run's starting <code>Suite</code>s returns.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunCompleted</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunCompleted</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunCompleted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunCompleted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunCompleted(ordinal))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunCompleted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunCompleted (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <RunCompleted>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <duration>{ longOption(duration) }</duration>
      <summary>{ summaryOption(summary) }</summary>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </RunCompleted>
}

/**
 * Event that indicates a runner has stopped running a suite of tests prior to completion, likely
 * because of a stop request.
 *
 * <p>
 * <code>Suite</code>'s <code>execute</code> method takes a <code>Stopper</code>, whose <code>stopRequested</code>
 * method indicates a stop was requested. If <code>true</code> is returned by
 * <code>stopRequested</code> while a suite of tests is running, the
 * <code>execute</code> method should promptly
 * return even if that suite hasn't finished running all of its tests.
 * </p>
 *
 * <p>If a stop was requested via the <code>Stopper</code>.
 * <code>Runner</code> will report <code>RunStopped</code>
 * when the <code>execute</code> method of the run's starting <code>Suite</code> returns.
 * If a stop is not requested, <code>Runner</code> will report <code>RunCompleted</code>
 * when the last <code>execute</code> method of the run's starting <code>Suite</code>s returns.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunStopped</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunStopped</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunStopped$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunStopped</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunStopped(ordinal))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStopped</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunStopped (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <RunStopped>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <duration>{ longOption(duration) }</duration>
      <summary>{ summaryOption(summary) }</summary>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </RunStopped>
}

/**
 * Event that indicates a runner encountered an error while attempting to run a suite of tests.
 *
 * <p>
 * For example, object <code>Runner</code> reports <code>RunAborted</code> if the
 * <code>execute</code> method of any of the run's starting <code>Suite</code>s completes
 * abruptly with a <code>Throwable</code>.
 * </p>
 *
 * <p>
 * ScalaTest's <code>Runner</code> fires a <code>RunAborted</code> report with an empty <code>summary</code>, because
 * the reporter is responsible for keeping track of the total number of tests reported as succeeded, failed, ignored, and pending.
 * ScalaTest's internal reporter replaces the <code>RunAborted</code> with a new one that is identical except that is
 * has a defined <code>summary</code>.
 * </p>
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="RunAborted$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>RunAborted</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(RunAborted(ordinal, message, Some(exception)))
 * </pre>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
 * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunAborted</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class RunAborted (
  ordinal: Ordinal,
  message: String,
  throwable: Option[Throwable],
  duration: Option[Long] = None,
  summary: Option[Summary] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (summary == null)
    throw new NullPointerException("summary was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <RunAborted>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <throwable>{ throwableOption(throwable) }</throwable>
      <duration>{ longOption(duration) }</duration>
      <summary>{ summaryOption(summary) }</summary>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </RunAborted>
}

/**
 * Event used to provide information that is not appropriate to report via any other <code>Event</code>.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="InfoProvided$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>InfoProvided</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(InfoProvided(ordinal, message, Some(NameInfo(suiteName, suiteId, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * An <code>InfoProvided</code> event may be fired from anywhere. In this respect <code>InfoProvided</code> is different
 * from the other events, for which it is defined whether they are fired in the context of a suite or test.
 * If fired in the context of a test, the <code>InfoProvided</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>InfoProvided</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined. If fired within the context
 * of neither a suite nor a test, the <code>nameInfo</code> of the <code>InfoProvided</code> event (an <code>Option[NameInfo]</code>) should be <code>None</code>.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
 *        in the context of which the information was provided
 * @param throwable an optional <code>Throwable</code>
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>InfoProvided</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class InfoProvided (
  ordinal: Ordinal,
  message: String,
  nameInfo: Option[NameInfo],
  throwable: Option[Throwable] = None,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends RecordableEvent {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <InfoProvided>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <nameInfo>{ nameInfoOption(nameInfo) }</nameInfo>
      <throwable>{ throwableOption(throwable) }</throwable>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </InfoProvided>
}

/**
 * Event used to provide markup text for document-style reports.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="MarkupProvided$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>MarkupProvided</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(MarkupProvided(ordinal, text, Some(NameInfo(suiteName, suiteId, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>MarkupProvided</code> event may be fired from anywhere. In this respect <code>MarkupProvided</code> is different
 * from the other events, for which it is defined whether they are fired in the context of a suite or test.
 * If fired in the context of a test, the <code>MarkupProvided</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>MarkupProvided</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined. If fired within the context
 * of neither a suite nor a test, the <code>nameInfo</code> of the <code>MarkupProvided</code> event (an <code>Option[NameInfo]</code>) should be <code>None</code>.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param text a snippet of markup text (in Markdown format)
 * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
 *        in the context of which the information was provided
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>MarkupProvided</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class MarkupProvided (
  ordinal: Ordinal,
  text: String,
  nameInfo: Option[NameInfo],
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends RecordableEvent {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (text == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <MarkupProvided>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <text>{ text }</text>
      <nameInfo>{ nameInfoOption(nameInfo) }</nameInfo>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </MarkupProvided>
}

/**
 * Event that indicates a new scope has been opened.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="ScopeOpened$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>ScopeOpened</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(ScopeOpened(ordinal, message, Some(NameInfo(suiteName, suiteId, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>ScopeOpened</code> event may be fired from within suites or tests. 
 * If fired in the context of a test, the <code>ScopeOpened</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>ScopeOpened</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo a <code>NameInfo</code> that provides names for the suite and optionally the test 
 *        in the context of which the scope was opened
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>ScopeOpened</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class ScopeOpened (
  ordinal: Ordinal,
  message: String,
  nameInfo: NameInfo,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <ScopeOpened>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <nameInfo>{ nameInfoOption(if (nameInfo != null) Some(nameInfo) else None) }</nameInfo>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </ScopeOpened>
}

/**
 * Event that indicates a scope has been closed.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="ScopeClosed$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>ScopeClosed</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(ScopeClosed(ordinal, message, Some(NameInfo(suiteName, suiteId, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>ScopeClosed</code> event may be fired from within suites or tests. 
 * If fired in the context of a test, the <code>ScopeClosed</code> event should include a <code>NameInfo</code> in which
 * <code>testName</code> is defined. If fired in the context of a suite, but not a test, the <code>ScopeClosed</code> event
 * should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo a <code>NameInfo</code> that provides names for the suite and optionally the test 
 *        in the context of which the scope was closed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>ScopeClosed</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class ScopeClosed (
  ordinal: Ordinal,
  message: String,
  nameInfo: NameInfo,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <ScopeClosed>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <nameInfo>{ nameInfoOption(if (nameInfo != null) Some(nameInfo) else None) }</nameInfo>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </ScopeClosed>
}

/**
 * Event that indicates a scope is pending.
 *
 * <p>
 * To create instances of this class you may
 * use the factory method provided in its <a href="ScopePending$.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>ScopePending</code> event like this:
 * </p>
 *
 * <pre class="stHighlight">
 * report(ScopePending(ordinal, message, Some(NameInfo(suiteName, Some(thisSuite.getClass.getName), Some(testName)))))
 * </pre>
 *
 * <p>
 * A <code>ScopePending</code> event is fired from within suites, and not tests. 
 * The <code>ScopePending</code> event should include a <code>NameInfo</code> in which <code>testName</code> is <em>not</em> defined.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param message a localized message suitable for presenting to the user
 * @param nameInfo a <code>NameInfo</code> that provides names for the suite and optionally the test 
 *        in the context of which the scope was closed
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param location An optional location that provides information indicating where in the source code an event originated.
 * @param payload an optional object that can be used to pass custom information to the reporter about the <code>ScopePending</code> event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class ScopePending (
  ordinal: Ordinal,
  message: String,
  nameInfo: NameInfo,
  formatter: Option[Formatter] = None,
  location: Option[Location] = None,
  payload: Option[Any] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (message == null)
    throw new NullPointerException("message was null")
  if (nameInfo == null)
    throw new NullPointerException("nameInfo was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (location == null)
    throw new NullPointerException("location was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <ScopePending>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <message>{ message }</message>
      <nameInfo>{ nameInfoOption(if (nameInfo != null) Some(nameInfo) else None) }</nameInfo>
      <formatter>{ formatterOption(formatter) }</formatter>
      <location>{ locationOption(location) }</location>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </ScopePending>
}

/**
 * Event that indicates a runner is beginning search for suites to run.
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param configMap a <code>ConfigMap</code> of key-value pairs that can be used by custom <code>Reporter</code>s
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class DiscoveryStarting (
  ordinal: Ordinal,
  configMap: ConfigMap,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {
 
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (configMap == null)
    throw new NullPointerException("configMap was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")

  /**
   * Location in a <code>DiscoveryStarting</code> is always set to <code>None</code>.
   */
  val location: Option[Location] = None

  /**
   * Payload in a <code>DiscoveryStarting</code> is always set to <code>None</code>.
   */
  val payload: Option[Any] = None

  /**
   * Formatter in a <code>DiscoveryStarting</code> is always set to <code>None</code>.
   */
  val formatter: Option[Formatter] = None

  import EventXmlHelper._
  private [scalatest] def toXml = 
    <DiscoveryStarting>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <configMap>
        { 
          for ((key, value) <- configMap) yield {
            <entry>
              <key>{ key }</key>
              <value>{ value }</value>
            </entry>
          }
        }
      </configMap>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </DiscoveryStarting>
}

/**
 * Event that indicates a runner has completed searching for suites.
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 *
 * @author Bill Venners
 */
final case class DiscoveryCompleted (
  ordinal: Ordinal,
  duration: Option[Long] = None,
  threadName: String = Thread.currentThread.getName,
  timeStamp: Long = (new Date).getTime
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (duration == null)
    throw new NullPointerException("duration was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")

  /**
   * Location in a <code>DiscoveryCompleted</code> is always set to <code>None</code>.
   */
  val location: Option[Location] = None

  /**
   * Payload in a <code>DiscoveryCompleted</code> is always set to <code>None</code>.
   */
  val payload: Option[Any] = None

  /**
   * Formatter in a <code>DiscoveryCompleted</code> is always set to <code>None</code>.
   */
  val formatter: Option[Formatter] = None
  
  import EventXmlHelper._
  private [scalatest] def toXml = 
    <DiscoveryCompleted>
      <ordinal>
        <runStamp>{ ordinal.runStamp }</runStamp>
      </ordinal>
      <duration>{ longOption(duration) }</duration>
      <threadName>{ threadName }</threadName>
      <timeStamp>{ timeStamp }</timeStamp>
    </DiscoveryCompleted>
}

/**
 * Deprecated singleton object for the <a href="TestStarting.html"><code>TestStarting</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestStarting</code> objects.
 * This object contains methods that were in the <code>TestStarting</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use TestStarting with named and/or default parameters instead.")
object DeprecatedTestStarting {

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that is starting (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestStarting</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   *
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): TestStarting = {
    TestStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that is starting (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): TestStarting = {
    TestStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter]
  ): TestStarting = {
    TestStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestStarting = {
    TestStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="TestSucceeded.html"><code>TestSucceeded</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestSucceeded</code> objects.
 * This object contains methods that were in the <code>TestSucceeded</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use TestSucceeded with named and/or default parameters instead.")
object DeprecatedTestSucceeded {

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has succeeded (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestSucceeded</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): TestSucceeded = {
    TestSucceeded(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, duration, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has succeeded (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): TestSucceeded = {
    TestSucceeded(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, duration, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    duration: Option[Long],
    formatter: Option[Formatter]
  ): TestSucceeded = {
    TestSucceeded(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, duration, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has succeeded
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    duration: Option[Long]
  ): TestSucceeded = {
    TestSucceeded(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestSucceeded = {
    TestSucceeded(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="TestFailed.html"><code>TestFailed</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestFailed</code> objects.
 * This object contains methods that were in the <code>TestFailed</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use TestFailed with named and/or default parameters instead.")
object DeprecatedTestFailed {

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has failed (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestFailed</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): TestFailed = {
    TestFailed(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, throwable, duration, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the test that has failed (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): TestFailed = {
    TestFailed(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, throwable, duration, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter]
  ): TestFailed = {
    TestFailed(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, throwable, duration, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to run the test that has failed
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    duration: Option[Long]
  ): TestFailed = {
    TestFailed(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, throwable, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable]
  ): TestFailed = {
    TestFailed(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, throwable, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="TestIgnored.html"><code>TestIgnored</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestIgnored</code> objects.
 * This object contains methods that were in the <code>TestIgnored</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use TestIgnored with named and/or default parameters instead.")
object DeprecatedTestIgnored {

  /**
   * Constructs a new <code>TestIgnored</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that was ignored
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was ignored
   * @param testName the name of the test that was ignored
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestIgnored</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestIgnored</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    payload: Option[Any]
  ): TestIgnored = {
    TestIgnored(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestIgnored</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that was ignored
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was ignored
   * @param testName the name of the test that was ignored
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestIgnored</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter]
  ): TestIgnored = {
    TestIgnored(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestIgnored</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that was ignored
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that was ignored
   * @param testName the name of the test that was ignored
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestIgnored</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestIgnored = {
    TestIgnored(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="TestPending.html"><code>TestPending</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestPending</code> objects.
 * This object contains methods that were in the <code>TestPending</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use TestPending with named and/or default parameters instead.")
object DeprecatedTestPending {

  /**
   * Constructs a new <code>TestPending</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is pending
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is pending
   * @param testName the name of the test that is pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>TestPending</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestPending</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    payload: Option[Any]
  ): TestPending = {
    TestPending(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, None, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestPending</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is pending
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is pending
   * @param testName the name of the test that is pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestPending</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter]
  ): TestPending = {
    TestPending(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, None, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestPending</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the test that is pending
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is pending
   * @param testName the name of the test that is pending
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>TestPending</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestPending = {
    TestPending(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, testName, testName, Vector.empty, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="SuiteStarting.html"><code>SuiteStarting</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>SuiteStarting</code> objects.
 * This object contains methods that were in the <code>SuiteStarting</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use SuiteStarting with named and/or default parameters instead.")
object DeprecatedSuiteStarting {

  /**
   * Constructs a new <code>SuiteStarting</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName a localized name identifying the suite that is starting, which should include the
   *        suite name, suitable for presenting to the user
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that is starting (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteStarting</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): SuiteStarting = {
    SuiteStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName a localized name identifying the suite that is starting, which should include the
   *        suite name, suitable for presenting to the user
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that is starting (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): SuiteStarting = {
    SuiteStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName a localized name identifying the suite that is starting, which should include the
   *        suite name, suitable for presenting to the user
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    formatter: Option[Formatter]
  ): SuiteStarting = {
    SuiteStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteStarting</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName a localized name identifying the suite that is starting, which should include the
   *        suite name, suitable for presenting to the user
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name of the suite that is starting
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String]
  ): SuiteStarting = {
    SuiteStarting(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="SuiteCompleted.html"><code>SuiteCompleted</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>SuiteCompleted</code> objects.
 * This object contains methods that were in the <code>SuiteCompleted</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use SuiteCompleted with named and/or default parameters instead.")
object DeprecatedSuiteCompleted {

  /**
   * Constructs a new <code>SuiteCompleted</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the suite that has completed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has completed (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteCompleted</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): SuiteCompleted = {
    SuiteCompleted(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, duration, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteCompleted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the suite that has completed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has completed (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): SuiteCompleted = {
    SuiteCompleted(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, duration, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteCompleted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the suite that has completed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    duration: Option[Long],
    formatter: Option[Formatter]
  ): SuiteCompleted = {
    SuiteCompleted(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, duration, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteCompleted</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param suiteName the name of the suite containing the suite that has completed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has completed
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String],
    duration: Option[Long]
  ): SuiteCompleted = {
    SuiteCompleted(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteCompleted</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the suite that has completed, which should include the
   *        suite name, suitable for presenting to the user
   * @param suiteName the name of the suite containing the suite that has completed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has completed
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    suiteName: String,
    suiteClassName: Option[String]
  ): SuiteCompleted = {
    SuiteCompleted(ordinal, suiteName, suiteClassName getOrElse suiteName, suiteClassName, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="SuiteAborted.html"><code>SuiteAborted</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>SuiteAborted</code> objects.
 * This object contains methods that were in the <code>SuiteAborted</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use SuiteAborted with named and/or default parameters instead.")
object DeprecatedSuiteAborted {

  /**
   * Constructs a new <code>SuiteAborted</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the suite that has aborted, which should include the
   *        suite name, suitable for presenting to the user
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the suite that has aborted
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has aborted (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>SuiteAborted</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner],
    payload: Option[Any]
  ): SuiteAborted = {
    SuiteAborted(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, throwable, duration, formatter, None, suiteClassName, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteAborted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the suite that has aborted
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunner an optional <code>Rerunner</code> that can be used to rerun the suite that has aborted (if <code>None</code>
   *        is passed, the suite cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter],
    rerunner: Option[Rerunner]
  ): SuiteAborted = {
    SuiteAborted(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, throwable, duration, formatter, None, suiteClassName, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteAborted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunner</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
   * @param suiteName the name of the suite containing the suite that has aborted
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    throwable: Option[Throwable],
    duration: Option[Long],
    formatter: Option[Formatter]
  ): SuiteAborted = {
    SuiteAborted(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, throwable, duration, formatter, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteAborted</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the suite that has aborted
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required to execute the suite that has aborted
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    throwable: Option[Throwable],
    duration: Option[Long]
  ): SuiteAborted = {
    SuiteAborted(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, throwable, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>SuiteAborted</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>rerunner</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param suiteName the name of the suite containing the suite that has aborted
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the suite that has aborted
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>SuiteAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    suiteName: String,
    suiteClassName: Option[String],
    throwable: Option[Throwable]
  ): SuiteAborted = {
    SuiteAborted(ordinal, message, suiteName, suiteClassName getOrElse suiteName, suiteClassName, throwable, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="RunStarting.html"><code>RunStarting</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>RunStarting</code> objects.
 * This object contains methods that were in the <code>RunStarting</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>, and <code>IllegalArgumentException</code> if
 * <code>testCount</code> is less than zero.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use RunStarting with named and/or default parameters instead.")
object DeprecatedRunStarting {

  /**
   * Constructs a new <code>RunStarting</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param testCount the number of tests expected during this run
   * @param configMap a <code>ConfigMap</code> of key-value pairs that can be used by custom <code>Reporter</code>s
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStarting</code> event
   *
   * @throws IllegalArgumentException if <code>testCount</code> is less than zero.
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    testCount: Int,
    configMap: ConfigMap,
    formatter: Option[Formatter],
    payload: Option[Any]
  ): RunStarting = {
    RunStarting(ordinal, testCount, configMap, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param testCount the number of tests expected during this run
   * @param configMap a <code>ConfigMap</code> of key-value pairs that can be used by custom <code>Reporter</code>s
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    testCount: Int,
    configMap: ConfigMap,
    formatter: Option[Formatter]
  ): RunStarting = {
    RunStarting(ordinal, testCount, configMap, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStarting</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param testCount the number of tests expected during this run
   * @param configMap a <code>ConfigMap</code> of key-value pairs that can be used by custom <code>Reporter</code>s
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    testCount: Int,
    configMap: ConfigMap
  ): RunStarting = {
    RunStarting(ordinal, testCount, configMap, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="RunCompleted.html"><code>RunCompleted</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>RunCompleted</code> objects.
 * This object contains methods that were in the <code>RunCompleted</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use RunCompleted with named and/or default parameters instead.")
object DeprecatedRunCompleted {

  /**
   * Constructs a new <code>RunCompleted</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunCompleted</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter],
    payload: Option[Any]
  ): RunCompleted = {
    RunCompleted(ordinal, duration, summary, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunCompleted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter]
  ): RunCompleted = {
    RunCompleted(ordinal, duration, summary, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunCompleted</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary]
  ): RunCompleted = {
    RunCompleted(ordinal, duration, summary, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunCompleted</code> event with the passed parameters, passing <code>None</code> for <code>summary</code>,
   *  <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has completed
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long]
  ): RunCompleted = {
    RunCompleted(ordinal, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunCompleted</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>summary</code>, <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunCompleted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal
  ): RunCompleted = {
    RunCompleted(ordinal, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="RunStopped.html"><code>RunStopped</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>RunStopped</code> objects.
 * This object contains methods that were in the <code>RunStopped</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use RunStopped with named and/or default parameters instead.")
object DeprecatedRunStopped {

  /**
   * Constructs a new <code>RunStopped</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunStopped</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStopped</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter],
    payload: Option[Any]
  ): RunStopped = {
    RunStopped(ordinal, duration, summary, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStopped</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStopped</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter]
  ): RunStopped = {
    RunStopped(ordinal, duration, summary, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStopped</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStopped</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long],
    summary: Option[Summary]
  ): RunStopped = {
    RunStopped(ordinal, duration, summary, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStopped</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has stopped
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStopped</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    duration: Option[Long]
  ): RunStopped = {
    RunStopped(ordinal, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunStopped</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunStopped</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal
  ): RunStopped = {
    RunStopped(ordinal, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="RunAborted.html"><code>RunAborted</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>RunAborted</code> objects.
 * This object contains methods that were in the <code>RunAborted</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use RunAborted with named and/or default parameters instead.")
object DeprecatedRunAborted {

  /**
   * Constructs a new <code>RunAborted</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>RunAborted</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter],
    payload: Option[Any]
  ): RunAborted = {
    RunAborted(ordinal, message, throwable, duration, summary, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunAborted</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    summary: Option[Summary],
    formatter: Option[Formatter]
  ): RunAborted = {
    RunAborted(ordinal, message, throwable, duration, summary, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunAborted</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
   * @param summary an optional summary of the number of tests that were reported as succeeded, failed, ignored, and pending
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    throwable: Option[Throwable],
    duration: Option[Long],
    summary: Option[Summary]
  ): RunAborted = {
    RunAborted(ordinal, message, throwable, duration, summary, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunAborted</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param duration an optional amount of time, in milliseconds, that was required by the run that has aborted
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    throwable: Option[Throwable],
    duration: Option[Long]
  ): RunAborted = {
    RunAborted(ordinal, message, throwable, duration, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>RunAborted</code> event with the passed parameters, passing <code>None</code> for <code>duration</code>,
   * <code>None</code> for <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the suite has aborted,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>RunAborted</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    throwable: Option[Throwable]
  ): RunAborted = {
    RunAborted(ordinal, message, throwable, None, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Deprecated singleton object for the <a href="InfoProvided.html"><code>InfoProvided</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>InfoProvided</code> objects.
 * This object contains methods that were in the <code>InfoProvided</code> companion object prior to ScalaTest 2.0. If you get a compiler error when upgrading
 * to 2.0 for one of the methods formerly in the companion object, a quick way to fix it is to put <code>Deprecated</code> in front of your call.
 * Eventually you will need to fix it properly, as this singleton object is deprecated and will be removed in a future version of ScalaTest, but
 * this will work as a quick fix to get you compiling again.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 *
 * @author Bill Venners
 */
@deprecated("Use InfoProvided with named and/or default parameters instead.")
object DeprecatedInfoProvided {

  /**
   * Constructs a new <code>InfoProvided</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
   *        in the context of which the information was provided
   * @param aboutAPendingTest indicates whether the information being provided via this event is about a pending test
   * @param throwable an optional <code>Throwable</code>
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param payload an optional object that can be used to pass custom information to the reporter about the <code>InfoProvided</code> event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>InfoProvided</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    nameInfo: Option[NameInfo],
    aboutAPendingTest: Option[Boolean],
    throwable: Option[Throwable],
    formatter: Option[Formatter],
    payload: Option[Any]
  ): InfoProvided = {
    InfoProvided(ordinal, message, nameInfo, throwable, formatter, None, payload, Thread.currentThread.getName, (new Date).getTime)
  }


  /**
   * Constructs a new <code>InfoProvided</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test
   *        in the context of which the information was provided
   * @param throwable an optional <code>Throwable</code>
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>InfoProvided</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    nameInfo: Option[NameInfo],
    throwable: Option[Throwable],
    formatter: Option[Formatter]
  ): InfoProvided = {
    InfoProvided(ordinal, message, nameInfo, throwable, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>InfoProvided</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test 
   *        in the context of which the information was provided
   * @param throwable an optional <code>Throwable</code>
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>InfoProvided</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    nameInfo: Option[NameInfo],
    throwable: Option[Throwable]
  ): InfoProvided = {
    InfoProvided(ordinal, message, nameInfo, throwable, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>InfoProvided</code> event with the passed parameters, passing <code>None</code> for
   * the <code>throwable</code>, <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param message a localized message suitable for presenting to the user
   * @param nameInfo an optional <code>NameInfo</code> that if defined, provides names for the suite and optionally the test
   *        in the context of which the information was provided
   * @param throwable an optional <code>Throwable</code>
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   *
   * @return a new <code>InfoProvided</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    message: String,
    nameInfo: Option[NameInfo]
  ): InfoProvided = {
    InfoProvided(ordinal, message, nameInfo, None, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}
