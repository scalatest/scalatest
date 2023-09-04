/*
 * Copyright 2001-2023 Artima, Inc.
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
}