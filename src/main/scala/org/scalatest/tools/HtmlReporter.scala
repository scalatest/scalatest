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
package org.scalatest.tools

import org.scalatest._
import Suite.unparsedXml
import Suite.xmlContent
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.util.Iterator
import java.util.Set
import java.io.StringWriter
import org.scalatest.events._
import org.scalatest.exceptions.TestFailedException
import PrintReporter.{BufferSize, makeDurationString}
import HtmlReporter._
import org.pegdown.PegDownProcessor
import scala.collection.mutable.ListBuffer
import scala.xml.NodeSeq
import scala.xml.XML
import java.util.UUID
import scala.xml.Node
import scala.xml.NodeBuffer
import scala.annotation.tailrec
import java.net.URL
import scala.io.Source
import java.nio.channels.Channels
import java.text.DecimalFormat

/**
 * A <code>Reporter</code> that prints test status information in HTML format to a file.
 */
private[scalatest] class HtmlReporter(directoryPath: String, presentAllDurations: Boolean,
        presentInColor: Boolean, presentStackTraces: Boolean, presentFullStackTraces: Boolean, 
        cssUrl: Option[URL], resultHolder: Option[SuiteResultHolder]) extends ResourcefulReporter {

  private val specIndent = 15
  private val targetDir = new File(directoryPath)
  private val imagesDir = new File(targetDir, "images")
  private val jsDir = new File(targetDir, "js")
  private val cssDir = new File(targetDir, "css")
  
  if (!targetDir.exists)
    targetDir.mkdirs()
    
  if (!imagesDir.exists)
    imagesDir.mkdirs()
    
  if (!jsDir.exists)
    jsDir.mkdirs()
    
  if (!cssDir.exists)
    cssDir.mkdirs()
    
  private def copyResource(url: URL, toDir: File, targetFileName: String) {
    val inputStream = url.openStream
    try {
      val outputStream = new FileOutputStream(new File(toDir, targetFileName))
      try {
        outputStream getChannel() transferFrom(Channels.newChannel(inputStream), 0, Long.MaxValue)
      }
      finally {
        outputStream.flush()
      outputStream.close()
      }
    }
    finally {
      inputStream.close()
    }
  }
  
  private def getResource(resourceName: String): URL = 
    classOf[Suite].getClassLoader.getResource(resourceName)
  
  cssUrl match {
    case Some(cssUrl) => copyResource(cssUrl, cssDir, "custom.css")
    case None => // Do nothing.
  }
  copyResource(getResource("org/scalatest/HtmlReporter.css"), cssDir, "styles.css")
  copyResource(getResource("org/scalatest/sorttable.js"), jsDir, "sorttable.js")
  copyResource(getResource("org/scalatest/d3.v2.min.js"), jsDir, "d3.v2.min.js")
  
  copyResource(getResource("images/greenbullet.gif"), imagesDir, "testsucceeded.gif")
  copyResource(getResource("images/redbullet.gif"), imagesDir, "testfailed.gif")
  copyResource(getResource("images/yellowbullet.gif"), imagesDir, "testignored.gif")
  copyResource(getResource("images/yellowbullet.gif"), imagesDir, "testcanceled.gif")
  copyResource(getResource("images/yellowbullet.gif"), imagesDir, "testpending.gif")
  copyResource(getResource("images/graybullet.gif"), imagesDir, "infoprovided.gif")
  
  private val results = resultHolder match {
    case Some(holder) => holder
    case None => new SuiteResultHolder()
  }
  private val pegDown = new PegDownProcessor

  private def withPossibleLineNumber(stringToPrint: String, throwable: Option[Throwable]): String = {
    throwable match {
      case Some(testFailedException: TestFailedException) =>
        testFailedException.failedCodeFileNameAndLineNumberString match {
          case Some(lineNumberString) =>
            Resources("printedReportPlusLineNumber", stringToPrint, lineNumberString)
          case None => stringToPrint
        }
      case _ => stringToPrint
    }
  }
  
  private def stringsToPrintOnError(noteResourceName: String, errorResourceName: String, message: String, throwable: Option[Throwable],
    formatter: Option[Formatter], suiteName: Option[String], testName: Option[String], duration: Option[Long]): String = {

    formatter match {
      case Some(IndentedText(_, rawText, _)) =>
        Resources("specTextAndNote", rawText, Resources(noteResourceName))
      case _ =>
        // Deny MotionToSuppress directives in error events, because error info needs to be seen by users
          suiteName match {
            case Some(sn) =>
              testName match {
                case Some(tn) => Resources(errorResourceName, sn + ": " + tn)
                case None => Resources(errorResourceName, sn)
              }
            // Should not get here with built-in ScalaTest stuff, but custom stuff could get here.
            case None => Resources(errorResourceName, Resources("noNameSpecified"))
          }
      }
  }

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String]): Option[String] =
    stringToPrintWhenNoError(resourceName, formatter, suiteName, testName, None)

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String], duration: Option[Long]): Option[String] = {

    formatter match {
      case Some(IndentedText(_, rawText, _)) =>
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", rawText, makeDurationString(milliseconds)))
            else
              Some(rawText)
          case None => Some(rawText)
        }
      case Some(MotionToSuppress) => None
      case _ =>
        val arg =
          testName match {
            case Some(tn) => suiteName + ": " + tn
            case None => suiteName
          }
        val unformattedText = Resources(resourceName, arg)
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", unformattedText, makeDurationString(milliseconds)))
            else
              Some(unformattedText)
          case None => Some(unformattedText)
        }

    }
  }
  
  private def getIndentLevel(formatter: Option[Formatter]) = 
    formatter match {
      case Some(IndentedText(formattedText, rawText, indentationLevel)) => indentationLevel
      case _ => 0
  }
  
  private def getSuiteFileName(suiteResult: SuiteResult) = 
    suiteResult.suiteClassName match {
      case Some(suiteClassName) => suiteClassName
      case None => suiteResult.suiteName
    }
  
  private def makeSuiteFile(suiteResult: SuiteResult) {
    val name = getSuiteFileName(suiteResult)
    
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(targetDir, name + ".html")), BufferSize))
    try {
      pw.println {
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n" + 
        "<!DOCTYPE html" + "\n" + 
        "  PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" + "\n" + 
        "  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" + "\n" + 
        getSuiteHtml(name, suiteResult) 
      }
    }
    finally {
      pw.flush()
      pw.close()
    }
  }
  
  private def appendCombinedStatus(name: String, r: SuiteResult) = 
    if (r.testsFailedCount > 0)
      name + "_with_failed"
    else if (r.testsIgnoredCount > 0 || r.testsPendingCount > 0 || r.testsCanceledCount > 0)
      name + "_passed"
    else
      name + "_passed_all"
  
  private def transformStringForResult(s: String, suiteResult: SuiteResult): String =
    s + (if (suiteResult.testsFailedCount > 0) "_failed" else "_passed")

  private def getSuiteHtml(name: String, suiteResult: SuiteResult) = 
    <html>
      <head>
        <title>ScalaTest Suite { name } Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <link href="css/styles.css" rel="stylesheet" />
        { 
          cssUrl match {
            case Some(cssUrl) => 
              <link href="css/custom.css" rel="stylesheet" />
            case None => NodeSeq.Empty
          }
        }
        <script type="text/javascript">
          { unparsedXml(
            "function toggleDetails(contentId, linkId) {" + "\n" + 
            "  var ele = document.getElementById(contentId);" + "\n" + 
            "  var text = document.getElementById(linkId);" + "\n" + 
            "  if(ele.style.display == \"block\") {" + "\n" + 
            "    ele.style.display = \"none\";" + "\n" + 
            "    text.innerHTML = \"(Show Details)\";" + "\n" + 
            "  }" + "\n" + 
            "  else {" + "\n" + 
            "    ele.style.display = \"block\";" + "\n" + 
            "    text.innerHTML = \"(Hide Details)\";" + "\n" + 
            "  }" + "\n" + 
            "}" + "\n" + 
            "function hideOpenInNewTabIfRequired() {" + "\n" + 
            "  if (top === self) { document.getElementById('printlink').style.display = 'none'; }" + "\n" + 
            "}" + "\n")
          }
        </script>
      </head>
      <body class="specification">
        <div id="suite_header_name">{ suiteResult.suiteName }</div>
        <div id={ transformStringForResult("suite_header_statistic", suiteResult) }>
          { "Tests: total " + (suiteResult.testsSucceededCount + suiteResult.testsFailedCount + suiteResult.testsCanceledCount + suiteResult.testsIgnoredCount + suiteResult.testsPendingCount) + ", succeeded " + 
            suiteResult.testsSucceededCount + ", failed " + suiteResult.testsFailedCount + ", canceled " + suiteResult.testsCanceledCount + ", ignored " + suiteResult.testsIgnoredCount + ", pending " + 
            suiteResult.testsPendingCount }
        </div>
        {
          val scopeStack = new collection.mutable.Stack[String]()
          suiteResult.eventList.map { e => 
            e match {
              case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
                val testNameInfo = nameInfo.testName
                val stringToPrint = stringToPrintWhenNoError("scopeOpened", formatter, nameInfo.suiteName, nameInfo.testName)
                stringToPrint match {
                  case Some(string) => 
                    val elementId = generateElementId
                    scopeStack.push(elementId)
                    scope(elementId, string, getIndentLevel(formatter) + 1)
                  case None => 
                    NodeSeq.Empty
                }
            
              case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
                scopeStack.pop
                NodeSeq.Empty
                
              case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
                val testNameInfo = nameInfo.testName
                val stringToPrint = stringToPrintWhenNoError("scopePending", formatter, nameInfo.suiteName, nameInfo.testName)
                stringToPrint match {
                  case Some(string) => 
                    val elementId = generateElementId
                    scope(elementId, string, getIndentLevel(formatter) + 1)
                  case None => 
                    NodeSeq.Empty
                }
          
              case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 
            
                val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(testName), duration)

                val nodeSeq = 
                  stringToPrint match {
                    case Some(string) => 
                      val elementId = generateElementId
                      test(elementId, List(string), getIndentLevel(formatter) + 1, "test_passed")
                    case None =>
                      NodeSeq.Empty
                  }
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

                val stringToPrint = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(testName), duration)
                val elementId = generateElementId
                val nodeSeq = testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_failed")            
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

                val stringToPrint =
                  formatter match {
                    case Some(IndentedText(_, rawText, _)) => Some(Resources("specTextAndNote", rawText, Resources("ignoredNote")))
                    case Some(MotionToSuppress) => None
                    case _ => Some(Resources("testIgnored", suiteName + ": " + testName))
                  }
 
                stringToPrint match {
                  case Some(string) => 
                    val elementId = generateElementId
                    test(elementId, List(string), getIndentLevel(formatter) + 1, "test_ignored")
                  case None =>
                    NodeSeq.Empty
                }
              
              case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

                val stringToPrint =
                  formatter match {
                    case Some(IndentedText(_, rawText, _)) => Some(Resources("specTextAndNote", rawText, Resources("pendingNote")))
                    case Some(MotionToSuppress) => None
                    case _ => Some(Resources("testPending", suiteName + ": " + testName))
                  }

                val nodeSeq = 
                  stringToPrint match {
                    case Some(string) => 
                      val elementId = generateElementId
                      test(elementId, List(string), getIndentLevel(formatter) + 1, "test_pending")
                    case None =>
                      NodeSeq.Empty
                  }
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

                val stringToPrint = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(testName), duration)
                val elementId = generateElementId
                val nodeSeq = testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_canceled")
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case infoProvided: InfoProvided =>
                processInfoMarkupProvided(infoProvided)
        
              case markupProvided: MarkupProvided => 
                processInfoMarkupProvided(markupProvided)
                // TO CONTINUE: XML element must be last
            
              case _ => NodeSeq.Empty
            }
          }
        }
        <table id="suite_footer">
          <tr id="suite_footer_id">
            <td id={ transformStringForResult("suite_footer_id_label", suiteResult) }>Suite ID</td>
            <td id="suite_footer_id_value" colspan="5">{ suiteResult.suiteId }</td>
          </tr>
          <tr id="suite_footer_class">
            <td id={ transformStringForResult("suite_footer_class_label", suiteResult) }>Class name</td>
            <td id="suite_footer_class_value" colspan="5">{ suiteResult.suiteClassName.getOrElse("-") }</td>
          </tr>
          <tr id="suite_footer_duration">
            <td id={ transformStringForResult("suite_footer_duration_label", suiteResult) }>Total duration</td>
            <td id="suite_footer_duration_value" colspan="2">
              { 
                suiteResult.duration match {
                  case Some(duration) => makeDurationString(duration)
                  case None => "-"
                } 
              }
              </td>
          </tr>
        </table>
           <div id="printlink">(<a href={ getSuiteFileName(suiteResult) + ".html" } target="_blank">Open { suiteResult.suiteName } in new tab</a>)</div>
      </body>
      <script type="text/javascript">
        { unparsedXml("hideOpenInNewTabIfRequired();") }
      </script>
    </html>
        
  private def processInfoMarkupProvided(event: Event) = {
    event match {
      case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
        val (suiteName, testName) =
          nameInfo match {
            case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
            case None => (None, None)
          }
        val infoContent = stringsToPrintOnError("infoProvidedNote", "infoProvided", message, throwable, formatter, suiteName, testName, None)
            
        val elementId = generateElementId
        test(elementId, List(infoContent), getIndentLevel(formatter) + 1, "info")
        
      case MarkupProvided(ordinal, text, nameInfo, formatter, location, payload, threadName, timeStamp) => 
        val (suiteName, testName) =
          nameInfo match {
            case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
            case None => (None, None)
          }
        
        val elementId = generateElementId
        markup(elementId, text, getIndentLevel(formatter) + 1, "markup")
        
      case _ => NodeSeq.Empty
    }
  }
  
  private def makeIndexFile(resourceName: String, duration: Option[Long]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(targetDir, "index.html")), BufferSize))
    try {
      pw.println {
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
        "<!DOCTYPE html\n" + 
        "PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n" +
        "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" + 
        getIndexHtml(resourceName, duration) 
      }
    }
    finally {
      pw.flush()
      pw.close()
    }
  }
  
  private def getHeaderStatusColor(summary: Summary) = 
    if (summary.testsFailedCount == 0) "scalatest-header-passed" else "scalatest-header-failed"
  
  private def getPieChartScript(summary: Summary) = {
    import summary._
    
    "/* modified from http://www.permadi.com/tutorial/cssGettingBackgroundColor/index.html - */" + "\n" + 
    "function getBgColor(elementId)" +  "\n" + 
    "{" + "\n" + 
    "  var element = document.getElementById(elementId);" + "\n" + 
    "  if (element.currentStyle)" + "\n" + 
    "    return element.currentStyle.backgroundColor;" + "\n" + 
    "  if (window.getComputedStyle)" + "\n" + 
    "  {" + "\n" + 
    "    var elementStyle=window.getComputedStyle(element,\"\");" + "\n" + 
    "    if (elementStyle)" + "\n" + 
    "      return elementStyle.getPropertyValue(\"background-color\");" + "\n" + 
    "  }" + "\n" + 
    "  // Return 0 if both methods failed." + "\n" + 
    "  return 0;" + "\n" + 
    "}" + "\n" + 
    "var data = [" + testsSucceededCount + ", " + testsFailedCount + ", " + testsIgnoredCount + ", " + testsPendingCount + ", " + testsCanceledCount + "];" + "\n" +  
    "var color = [getBgColor('summary_view_row_1_legend_succeeded_label'), " + "\n" + 
    "             getBgColor('summary_view_row_1_legend_failed_label'), " + "\n" + 
    "             getBgColor('summary_view_row_1_legend_ignored_label'), " + "\n" + 
    "             getBgColor('summary_view_row_1_legend_pending_label'), " + "\n" + 
    "             getBgColor('summary_view_row_1_legend_canceled_label')" +  "\n" + 
    "            ];" + "\n" + 
    "var width = document.getElementById('chart_div').offsetWidth," + "\n" + 
    "    height = document.getElementById('chart_div').offsetHeight," + "\n" + 
    "    outerRadius = Math.min(width, height) / 2," + "\n" + 
    "    innerRadius = 0," + "\n" + 
    "    donut = d3.layout.pie()," + "\n" +  
    "    arc = d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius);" + "\n" +  
    "var vis = d3.select(\"#chart_div\")" + "\n" + 
    "            .append(\"svg\")" + "\n" + 
    "            .data([data])" + "\n" + 
    "            .attr(\"width\", width)" + "\n" + 
    "            .attr(\"height\", height);" + "\n" + 
    "var arcs = vis.selectAll(\"g.arc\")" + "\n" + 
    "              .data(donut)" + "\n" + 
    "              .enter().append(\"g\")" + "\n" + 
    "              .attr(\"class\", \"arc\")" + "\n" + 
    "              .attr(\"transform\", \"translate(\" + outerRadius + \",\" + outerRadius + \")\");" + "\n" + 
    "arcs.append(\"path\")" + "\n" + 
    "    .attr(\"fill\", function(d, i) { return color[i]; })" + "\n" +  
    "    .attr(\"d\", arc);\n"
  }
  
  private def getIndexHtml(resourceName: String, duration: Option[Long]) = {
    val summary = results.summary
    import summary._

    val decimalFormat = new DecimalFormat("#.##")
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
        <title>ScalaTest Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <link href="css/styles.css" rel="stylesheet" />
        { 
          cssUrl match {
            case Some(cssUrl) => 
              <link href="css/custom.css" rel="stylesheet" />
            case None => NodeSeq.Empty
          }
        }
        <script type="text/javascript" src="js/d3.v2.min.js"></script>
        <script type="text/javascript" src="js/sorttable.js"></script>
        <script type="text/javascript">
          { unparsedXml(
            "var tagMap = {};" + "\n" +     
            "var SUCCEEDED_BIT = 1;" + "\n" + 
            "var FAILED_BIT = 2;" + "\n" + 
            "var IGNORED_BIT = 4;" + "\n" + 
            "var PENDING_BIT = 8;" + "\n" + 
            "var CANCELED_BIT = 16;" + "\n" + 
            "function applyFilter() {" + "\n" + 
            "  var mask = 0;" + "\n" + 
            "  if (document.getElementById('succeeded_checkbox').checked)" + "\n" + 
            "    mask |= SUCCEEDED_BIT;" + "\n" +  
            "  if (document.getElementById('failed_checkbox').checked)" + "\n" + 
            "    mask |= FAILED_BIT;" + "\n" + 
            "  if (document.getElementById('ignored_checkbox').checked)" + "\n" + 
            "    mask |= IGNORED_BIT;" + "\n" + 
            "  if (document.getElementById('pending_checkbox').checked)" + "\n" + 
            "    mask |= PENDING_BIT;" + "\n" + 
            "  if (document.getElementById('canceled_checkbox').checked)" + "\n" + 
            "    mask |= CANCELED_BIT;" + "\n" + 
            "  for (var key in tagMap) {" + "\n" + 
            "    if (tagMap.hasOwnProperty(key)) {" + "\n" + 
            "      var bitSet = tagMap[key];" + "\n" + 
            "      var element = document.getElementById(key);" + "\n" + 
            "      if ((bitSet & mask) != 0)" + "\n" +  
            "        element.style.display = \"table-row\";" + "\n" + 
            "      else " + "\n" +  
            "        element.style.display = \"none\";" + "\n" + 
            "    }" + "\n" + 
            "  }" + "\n" + 
            "}" + "\n" + 
            "function showDetails(suiteName) {" + "\n" + 
            "  document.getElementById('details_view').innerHTML = \"<iframe src='\" + suiteName + \".html' width='100%' height='100%'></iframe>\";" + "\n" + 
            "}" + "\n" + 
            "function resizeDetailsView() {" + "\n" + 
            "  var headerView = document.getElementById('scalatest-header');" + "\n" + 
            "  var detailsView = document.getElementById('details_view');" + "\n" + 
            "  var summaryView = document.getElementById('summary_view');" + "\n" + 
            "  var left = summaryView.offsetWidth + 30;" + "\n" + 
            "  detailsView.style.left = left + \"px\";" + "\n" + 
            "  detailsView.style.width = (window.innerWidth - left - 30) + \"px\";" + "\n" + 
            "  detailsView.style.height = (window.innerHeight - headerView.offsetHeight - 20) + \"px\";" + "\n" + 
            "}\n") }
        </script>
      </head>
      <body onresize="resizeDetailsView()">
        <div class="scalatest-report"> 
          { header(resourceName, duration, summary) }
          <table id="summary_view">
            <tr id="summary_view_row_1">
              <td id="summary_view_row_1_chart">
                <div id="chart_div"></div>
              </td>
              <td id="summary_view_row_1_legend">
                <table id="summary_view_row_1_legend_table">
                  <tr id="summary_view_row_1_legend_table_row_succeeded">
                    <td id="summary_view_row_1_legend_succeeded_label">Succeeded</td>
                    <td id="summary_view_row_1_legend_succeeded_count">{ testsSucceededCount }</td>
                    <td id="summary_view_row_1_legend_succeeded_percent">({ decimalFormat.format(testsSucceededCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_failed">
                    <td id="summary_view_row_1_legend_failed_label">Failed</td>
                    <td id="summary_view_row_1_legend_failed_count">{ testsFailedCount }</td>
                    <td id="summary_view_row_1_legend_failed_percent">({ decimalFormat.format(testsFailedCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_canceled">
                    <td id="summary_view_row_1_legend_canceled_label">Canceled</td>
                    <td id="summary_view_row_1_legend_canceled_count">{ testsCanceledCount }</td>
                    <td id="summary_view_row_1_legend_canceled_percent">({ decimalFormat.format(testsCanceledCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_ignored">
                    <td id="summary_view_row_1_legend_ignored_label">Ignored</td>
                    <td id="summary_view_row_1_legend_ignored_count">{ testsIgnoredCount }</td>
                    <td id="summary_view_row_1_legend_ignored_percent">({ decimalFormat.format(testsIgnoredCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_pending">
                    <td id="summary_view_row_1_legend_pending_label">Pending</td>
                    <td id="summary_view_row_1_legend_pending_count">{ testsPendingCount }</td>
                    <td id="summary_view_row_1_legend_pending_percent">({ decimalFormat.format(testsPendingCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                </table>
              </td>
            </tr>
            <tr id="summary_view_row_2">
              <td id="summary_view_row_2_results" colspan="2">
                { getStatistic(summary) }
                { suiteResults }
              </td>
            </tr>
          </table>
          <div id="details_view">
            <span id="click_suite_hint">Click on suite name to view details.</span> <br />
            <span id="click_column_hint">Click on column name to sort.</span>
          </div>
        </div>
        <script type="text/javascript">
          { unparsedXml(getPieChartScript(summary)) }
        </script>
        <script type="text/javascript">
          { unparsedXml(tagMapScript) }
        </script>
        <script type="text/javascript">
          { unparsedXml("resizeDetailsView();") }
        </script>
      </body>
    </html>
  }      
          
  // TODO: This needs to be internationalized
  private def getStatistic(summary: Summary) = 
    <div id="display-filters">
      <input id="succeeded_checkbox" name="succeeded_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label id="succeeded_checkbox_label" for="passed_checkbox">Succeeded</label>
      <input id="failed_checkbox" name="failed_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label id="failed_checkbox_label" for="failed_checkbox">Failed</label>
      <input id="canceled_checkbox" name="canceled_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label id="canceled_checkbox_label" for="canceled_checkbox">Canceled</label>
      <input id="ignored_checkbox" name="ignored_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label id="ignored_checkbox_label" for="ignored_checkbox">Ignored</label>
      <input id="pending_checkbox" name="pending_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label id="pending_checkbox_label" for="pending_checkbox">Pending</label>
    </div>
  
  private def header(resourceName: String, duration: Option[Long], summary: Summary) = 
    <div id="scalatest-header" class={ getHeaderStatusColor(summary) }>
      <div id="title">
        ScalaTest Results
      </div>

      <div id="summary">
        <p id="duration">{ getDuration(resourceName, duration) }</p>    
        <p id="totalTests">{ getTotalTests(summary) }</p>
        <p id="suiteSummary">{ getSuiteSummary(summary) }</p>
        <p id="testSummary">{ getTestSummary(summary) }</p>
      </div>
    </div>
        
  private def generateElementId = UUID.randomUUID.toString
  
  private def setBit(stack: collection.mutable.Stack[String], tagMap: collection.mutable.HashMap[String, Int], bit: Int) {
    stack.foreach { scopeElementId => 
      val currentBits = tagMap(scopeElementId)
      tagMap.put(scopeElementId, currentBits | bit)
    }
  }
  
  val tagMap = collection.mutable.HashMap[String, Int]()
        
  private def suiteResults = 
    <table class="sortable">
      <tr>
        <td>Suite</td>
        <td>Duration (ms.)</td>
        <td>Succeeded</td>
        <td>Failed</td>
        <td>Canceled</td>
        <td>Ignored</td>
        <td>Pending</td>
        <td>Total</td>
      </tr>
    {
      val sortedSuiteList = results.suiteList.sortWith { (a, b) => 
        if (a.testsFailedCount == b.testsFailedCount) { 
          if (a.testsCanceledCount == b.testsCanceledCount) {
            if (a.testsIgnoredCount == b.testsIgnoredCount) {
              if (a.testsPendingCount == b.testsPendingCount)
                a.startEvent.suiteName < b.startEvent.suiteName
              else
                a.testsPendingCount > b.testsPendingCount
            }
            else
              a.testsIgnoredCount > b.testsIgnoredCount
          }
          else
            a.testsCanceledCount > b.testsCanceledCount
        }
        else
          a.testsFailedCount > b.testsFailedCount
      }.toArray
      sortedSuiteList map { r =>
        val elementId = generateElementId
        import r._
        val bits = 
          (if (testsSucceededCount > 0) SUCCEEDED_BIT else 0) +
          (if (testsFailedCount > 0) FAILED_BIT else 0) +
          (if (testsIgnoredCount > 0) IGNORED_BIT else 0) + 
          (if (testsPendingCount > 0) PENDING_BIT else 0) + 
          (if (testsCanceledCount > 0) CANCELED_BIT else 0)
        tagMap.put(elementId, bits)
        suiteSummary(elementId,  getSuiteFileName(r), r)
      }
    }
    </table>
      
  private def countStyle(prefix: String, count: Int) = 
    if (count == 0)
      prefix + "_zero"
    else
      prefix
      
  private def durationDisplay(duration: Option[Long]) = 
    duration match {
      case Some(duration) => duration
      case None => "-"
    }
    
  private def suiteSummary(elementId: String, suiteFileName: String, suiteResult: SuiteResult) = {
    import suiteResult._
    <tr id={ elementId }>
      <td class={ appendCombinedStatus("suite_name", suiteResult) }><a href={ "javascript: showDetails('" + suiteFileName + "')" }>{ suiteName }</a></td>
      <td class={ appendCombinedStatus("duration", suiteResult) }>{ durationDisplay(duration) }</td>
      <td class={ countStyle("succeeded", testsSucceededCount) }>{ testsSucceededCount }</td>
      <td class={ countStyle("failed", testsFailedCount) }>{ testsFailedCount }</td>
      <td class={ countStyle("canceled", testsCanceledCount) }>{ testsCanceledCount }</td>
      <td class={ countStyle("ignored", testsIgnoredCount) }>{ testsIgnoredCount }</td>
      <td class={ countStyle("pending", testsPendingCount) }>{ testsPendingCount }</td>
      <td class={ appendCombinedStatus("total", suiteResult) }>{ testsSucceededCount + testsFailedCount + testsIgnoredCount + testsPendingCount + testsCanceledCount }</td>
    </tr>
  }
        
  private def twoLess(indentLevel: Int): Int =
    indentLevel - 2 match {
      case lev if lev < 0 => 0
      case lev => lev
    }

  private def oneLess(indentLevel: Int): Int =
    indentLevel - 1 match {
      case lev if lev < 0 => 0
      case lev => lev
    }

  private def scope(elementId: String, message: String, indentLevel: Int) = 
    <div id={ elementId } class="scope" style={ "margin-left: " + (specIndent * oneLess(indentLevel)) + "px;" }>
      { message }
    </div>
      
  private def test(elementId: String, lines: List[String], indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (specIndent * twoLess(indentLevel)) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
    </div>
  
  private def testWithDetails(elementId: String, lines: List[String], message: String, throwable: Option[Throwable], indentLevel: Int, styleName: String) = {
    def getHTMLForStackTrace(stackTraceList: List[StackTraceElement]) =
              stackTraceList.map((ste: StackTraceElement) => <div>{ ste.toString }</div>)
    
    def displayErrorMessage(errorMessage: String) = {
      // scala automatically change <br /> to <br></br>, which will cause 2 line breaks, use unparsedXml("<br />") to solve it.
      val messageLines = errorMessage.split("\n")
      if (messageLines.size > 1)
        messageLines.map(line => <span>{ xmlContent(line) }{ unparsedXml("<br />") }</span>)
      else
        <span>{ message }</span>
    }
              
    def getHTMLForCause(throwable: Throwable): NodeBuffer = {
      val cause = throwable.getCause
      if (cause != null) {
        <table>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsCause") + ":" }</span></td>
            <td align="left">{ cause.getClass.getName }</td>
          </tr>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td>
            <td align="left">
              { 
                if (cause.getMessage != null) 
                  displayErrorMessage(cause.getMessage)
                else 
                  <span>{ Resources("None") }</span> 
              }
            </td>
          </tr>
        </table>
        <table>
          <tr valign="top">
            <td align="left" colspan="2">{ getHTMLForStackTrace(cause.getStackTrace.toList) }</td>
          </tr>
        </table> &+ getHTMLForCause(cause)
      }
      else new scala.xml.NodeBuffer
    }
    
    val (grayStackTraceElements, blackStackTraceElements) =
      throwable match {
        case Some(throwable) =>
          val stackTraceElements = throwable.getStackTrace.toList
          throwable match {
            case sde: exceptions.StackDepthException =>
              (stackTraceElements.take(sde.failedCodeStackDepth), stackTraceElements.drop(sde.failedCodeStackDepth))
            case _ => (List(), stackTraceElements)
          } 
        case None => (List(), List())
      }
    
    val throwableTitle = 
      throwable match {
        case Some(throwable) => Some(throwable.getClass.getName)
        case None => None
      }
    
    val fileAndLineOption: Option[String] = 
      throwable match {
        case Some(throwable) =>
          throwable match {
            case stackDepth: StackDepth =>
              stackDepth.failedCodeFileNameAndLineNumberString
            case _ => None
          }
        case None => None
      }
    
    val linkId = UUID.randomUUID.toString
    val contentId = UUID.randomUUID.toString
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (specIndent * twoLess(indentLevel)) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
      <div class="detailstoggle">
      <a id={ linkId } href={ "javascript:toggleDetails('" + contentId + "', '" + linkId + "');" }>(Show Details)</a>
      </div>
      <div id={ contentId } style="display: none">
        <table>
          {
            <tr valign="top">
              <td align="left"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td>
              <td align="left">{ displayErrorMessage(message) }</td>
            </tr>
          }
          {
            fileAndLineOption match {
              case Some(fileAndLine) =>
                <tr valign="top"><td align="left"><span class="label">{ Resources("LineNumber") + ":" }</span></td><td align="left"><span>{ "(" + fileAndLine + ")" }</span></td></tr>
              case None =>
            }
          }
          {
            throwableTitle match {
              case Some(title) =>
                <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsThrowable") + ":" }</span></td><td align="left">{ title }</td></tr>
              case None => new scala.xml.NodeBuffer
            }
          }
        </table>
        <table>
          <tr valign="top"><td align="left" colspan="2">
            { grayStackTraceElements.map((ste: StackTraceElement) => <div class="gray">{ ste.toString }</div>) }
            { blackStackTraceElements.map((ste: StackTraceElement) => <div>{ ste.toString }</div>) }
            </td>
          </tr>
        </table>
        {
          throwable match {
            case Some(t) => getHTMLForCause(t)
            case None =>
          }
        }
      </div>
    </div>
  }
        
  private def markup(elementId: String, text: String, indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (specIndent * oneLess(indentLevel)) + "px;" }>
       { XML.loadString(pegDown.markdownToHtml(text)) }
    </div>
       
  private def tagMapScript = 
    "tagMap = { \n" + 
      tagMap.map { case (elementId, bitSet) => "\"" + elementId + "\": " + bitSet }.mkString(", \n") + 
    "};\n" + 
    "applyFilter();"
    
  private var eventList = new ListBuffer[Event]()
  private var runEndEvent: Option[Event] = None
        
  def apply(event: Event) {
        
    event match {
      case _: DiscoveryStarting  =>
      case _: DiscoveryCompleted =>

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
        runEndEvent = Some(event)

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)
        
      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        if (sortedSuiteEvents.length == 0)
          throw new IllegalStateException("Expected SuiteStarting for completion event: " + event + " in the head of suite events, but we got no suite event at all")
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            val suiteResult = sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, duration, suiteStarting, event, Vector.empty ++ sortedSuiteEvents.tail, 0, 0, 0, 0, 0, 0, true)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case scopePending: ScopePending => r.copy(scopesPendingCount = r.scopesPendingCount + 1)
                case _ => r
              }
            }
            results += suiteResult
            makeSuiteFile(suiteResult)
          case other => 
            throw new IllegalStateException("Expected SuiteStarting for completion event: " + event +  " in the head of suite events, but we got: " + other)
        }
            
      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        if (sortedSuiteEvents.length == 0)
          throw new IllegalStateException("Expected SuiteStarting for completion event: " + event + " in the head of suite events, but we got no suite event at all")
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            val suiteResult = sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, duration, suiteStarting, event, Vector.empty ++ sortedSuiteEvents.tail, 0, 0, 0, 0, 0, 0, false)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case scopePending: ScopePending => r.copy(scopesPendingCount = r.scopesPendingCount + 1)
                case _ => r
              }
            }
            results += suiteResult
            makeSuiteFile(suiteResult)
          case other => 
            throw new IllegalStateException("Expected SuiteStarting for completion event: " + event + " in the head of suite events, but we got: " + other)
        }
      
      case _ => eventList += event
    }
  }
      
  def extractSuiteEvents(suiteId: String) = eventList partition { e => 
    e match {
      case e: TestStarting => e.suiteId == suiteId
      case e: TestSucceeded  => e.suiteId == suiteId
      case e: TestIgnored    => e.suiteId == suiteId
      case e: TestFailed     => e.suiteId == suiteId
      case e: TestPending    => e.suiteId == suiteId
      case e: TestCanceled   => e.suiteId == suiteId
      case e: InfoProvided   => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteId == suiteId
          case None => false
        }
      case e: MarkupProvided => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteId == suiteId
          case None => false
        }
      case e: ScopeOpened    => e.nameInfo.suiteId == suiteId
      case e: ScopeClosed    => e.nameInfo.suiteId == suiteId
      case e: ScopePending   => e.nameInfo.suiteId == suiteId
      case e: SuiteStarting  => e.suiteId == suiteId
      case _ => false
    }
  }
      
  def dispose() {
    runEndEvent match {
      case Some(event) => 
        event match {
          case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runCompleted", duration)

          case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
            makeIndexFile("runStopped", duration)

          case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runAborted", duration)
            
          case other =>
            throw new IllegalStateException("Expected run ending event only, but got: " + other.getClass.getName)
        }
      case None => // If no run end event (e.g. when run in sbt), just use runCompleted with sum of suites' duration.
        makeIndexFile("runCompleted", Some(results.totalDuration))
    }
  }
  
  private def getDuration(resourceName: String, duration: Option[Long]) = {
    duration match {
      case Some(msSinceEpoch) =>
        Resources(resourceName + "In", makeDurationString(msSinceEpoch))
      case None =>
        Resources(resourceName)
    }
  }
  
  private def getTotalTests(summary: Summary) = 
    Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString)    
    
  // Suites: completed {0}, aborted {1}
  private def getSuiteSummary(summary: Summary) = 
    if (summary.scopesPendingCount > 0)       
      Resources("suiteScopeSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString, summary.scopesPendingCount.toString)
    else
      Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString)

  // Tests: succeeded {0}, failed {1}, canceled {4}, ignored {2}, pending {3}
  private def getTestSummary(summary: Summary) = 
    Resources("testSummary", summary.testsSucceededCount.toString, summary.testsFailedCount.toString, summary.testsCanceledCount.toString, summary.testsIgnoredCount.toString,
      summary.testsPendingCount.toString)

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources("indentOnce", indent(s, times - 1)) 
}

private[tools] object HtmlReporter {  
  final val SUCCEEDED_BIT = 1
  final val FAILED_BIT = 2
  final val IGNORED_BIT = 4
  final val PENDING_BIT = 8
  final val CANCELED_BIT = 16
}
