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
package org.scalatest.events

import org.scalatest.exceptions.StackDepthException
import scala.xml.Elem

private[scalatest] object EventXmlHelper {
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
              <filePathname>{ filePathnameOption(lineInFile.filePathname) }</filePathname>
            </LineInFile>
          case SeeStackDepthException =>
              <SeeStackDepthException />
          case _ =>
            ""
        }
      case None => ""
    }
  }
  def filePathnameOption(filePathnameOpt: Option[String]) = {
    filePathnameOpt match {
      case Some(filePathname) => filePathname
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
  def toXml(event: Event): Elem = 
    event match {
      case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunner, payload,
                        threadName, timeStamp) =>
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

      case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter,
                        location, rerunner, payload, threadName, timeStamp) =>
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
          <recordedEvents>{ recordedEvents.map(EventXmlHelper.toXml) }</recordedEvents>
          <formatter>{ formatterOption(formatter) }</formatter>
          <location>{ locationOption(location) }</location>
          <rerunner>{ stringOption(rerunner) }</rerunner>
          <threadName>{ threadName }</threadName>
          <timeStamp>{ timeStamp }</timeStamp>
        </TestSucceeded>

      case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, analysis, throwable,
                      duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
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
          <recordedEvents>{ recordedEvents.map(EventXmlHelper.toXml) }</recordedEvents>
          <analysis>analysis.map(a => <message>a</message>)</analysis>
          <throwable>{ throwableOption(throwable) }</throwable>
          <formatter>{ formatterOption(formatter) }</formatter>
          <location>{ locationOption(location) }</location>
          <rerunner>{ stringOption(rerunner) }</rerunner>
          <threadName>{ threadName }</threadName>
          <timeStamp>{ timeStamp }</timeStamp>
        </TestFailed>                

      case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload,
                       threadName, timeStamp) =>
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

      case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter,
                       location, payload, threadName, timeStamp) =>
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
          <recordedEvents>{ recordedEvents.map(EventXmlHelper.toXml) }</recordedEvents>
          <formatter>{ formatterOption(formatter) }</formatter>
          <location>{ locationOption(location) }</location>
          <threadName>{ threadName }</threadName>
          <timeStamp>{ timeStamp }</timeStamp>
        </TestPending>

      case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration,
                        formatter, location, rerunner, payload, threadName, timeStamp) =>
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
          <recordedEvents>{ recordedEvents.map(EventXmlHelper.toXml) }</recordedEvents>
          <throwable>{ throwableOption(throwable) }</throwable>
          <formatter>{ formatterOption(formatter) }</formatter>
          <location>{ locationOption(location) }</location>
          <rerunner>{ stringOption(rerunner) }</rerunner>
          <threadName>{ threadName }</threadName>
          <timeStamp>{ timeStamp }</timeStamp>
        </TestCanceled>

      case SuiteStarting (ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunner, payload, threadName, timeStamp) =>                                              
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

      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>               
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

      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
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

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) =>
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

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
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

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
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

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) =>
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

      case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
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

      case AlertProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
        <AlertProvided>
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
        </AlertProvided>

      case NoteProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
        <NoteProvided>
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
        </NoteProvided>

      case MarkupProvided(ordinal, text, nameInfo, formatter, location, payload, threadName, timeStamp) => 
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

      case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
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

      case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
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

      case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
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

      case DiscoveryStarting(ordinal, configMap, threadName, timeStamp) =>
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

      case DiscoveryCompleted(ordinal, duration, threadName, timeStamp) => 
        <DiscoveryCompleted>
          <ordinal>
            <runStamp>{ ordinal.runStamp }</runStamp>
          </ordinal>
          <duration>{ longOption(duration) }</duration>
          <threadName>{ threadName }</threadName>
          <timeStamp>{ timeStamp }</timeStamp>
        </DiscoveryCompleted>                         
    }
}