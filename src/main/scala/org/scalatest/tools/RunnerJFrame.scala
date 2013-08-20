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
import java.awt.BorderLayout
import java.awt.Container
import java.awt.Dimension
import java.awt.GridLayout
import java.awt.Point
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintWriter
import java.net.URL
import javax.swing.AbstractAction
import javax.swing.DefaultListModel
import javax.swing.ImageIcon
import javax.swing.JButton
import javax.swing.JCheckBoxMenuItem
import javax.swing.JDialog
import javax.swing.JFrame
import javax.swing.WindowConstants
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JMenu
import javax.swing.JMenuBar
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JEditorPane
import javax.swing.KeyStroke
import javax.swing.ListSelectionModel
import javax.swing.border.BevelBorder
import javax.swing.border.EmptyBorder
import javax.swing.event.ListSelectionEvent
import javax.swing.event.ListSelectionListener
import Runner.usingEventDispatchThread
import Runner.withClassLoaderAndDispatchReporter
import java.util.concurrent.Semaphore
import java.util.regex.Pattern
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.EventQueue
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.events._
import EventToPresent.eventToEventToPresent
import org.scalatest.exceptions.StackDepth

/**
 * The main class for Runner's GUI.
 *
 * eventTypesToCollect are the types of events that should be collected as a run runs.
 * This comes from the set of config options following the -g in the invocation of Runner.
 * If it is -gZ, for example, only test starting events will be collected as the runs run.
 * We don't collect options that aren't selected, because long runs can generate a lot of
 * events that would take up a lot of memory.
 *
 * @author Bill Venners
 */                 
private[scalatest] class RunnerJFrame(
  val eventTypesToCollect: Set[EventToPresent],
  reporterConfigurations: ReporterConfigurations,
  suitesList: List[SuiteParam],
  agains: List[String],
  testSpecs: List[TestSpec],
  junitsList: List[String],
  runpathList: List[String], 
  tagsToIncludeSet: Set[String],
  tagsToExcludeSet: Set[String],
  propertiesMap: ConfigMap,
  concurrent: Boolean,
  memberOfList: List[String], 
  beginsWithList: List[String],
  testNGList: List[String],
  passFailReporter: Option[Reporter],
  concurrentConfig: ConcurrentConfig,
  suffixes: Option[Pattern],
  chosenStyleSet: Set[String],
  detectSlowpokes: Boolean,
  slowpokeDetectionDelay: Long,
  slowpokeDetectionPeriod: Long
) extends JFrame(Resources("ScalaTestTitle")) with RunDoneListener with RunnerGUI {
  
  // This should only be updated by the event handler thread.
  private var currentState: RunnerGUIState = RunningState

  // The default options in the graphic view. Just show runs
  // and failures. This is also a selection in the View menu.
  private val runsAndFailures: Set[EventToPresent] =
    Set(
      PresentRunStarting,
      PresentTestFailed,
      PresentSuiteAborted,
      PresentRunStopped,
      PresentRunAborted,
      PresentRunCompleted
    )

  // These are the actual options to view in the list of events.
  // This must be the same set or a subset of eventTypesToCollect,
  // because you can't view something that wasn't collected.
  // This should only be updated by the event handler thread.
  private var viewOptions = runsAndFailures

  private val optionsMap: Map[EventToPresent, JCheckBoxMenuItem] = initializeOptionsMap

  private val aboutBox: AboutJDialog = initializeAboutBox()

  // The list of events collected from the most recent run
  // The most recently added event is at the head of the list.
  // This is volatile because it is sorted outside the event handler thread.
  @volatile private var collectedEvents: List[EventHolder] = Nil

  // The eventsListModel and eventsJList are used to display the current
  // collected events of types selected by the view menu
  private val eventsListModel: DefaultListModel/*[EventHolder]ForJava17*/ = new DefaultListModel()
  private val eventsJList: JList = new JList/*[EventHolder]ForJava17*/(eventsListModel)

  // The detailsJEditorPane displays the text details of a event.
  private val detailsJEditorPane: JEditorPane = new JEditorPane("text/html", null)

  private val progressBarPanel: ProgressBarPanel = new ProgressBarPanel()
  private val statusJPanel: StatusJPanel = new StatusJPanel()
  private val rerunColorBox: ColorBar = new ColorBar()
  private val runJButton: JButton = new JButton(Resources("Run"))
  private val rerunJButton: JButton = new JButton(Resources("Rerun"))

  private var testsCompletedCount: Int = 0
  private var rerunTestsCompletedCount: Int = 0

  private val graphicRunReporter: Reporter = new GraphicRunReporter
  private val graphicRerunReporter: Reporter = new GraphicRerunReporter
  
  class ResettableStopper extends Stopper {
    @volatile private var stopWasRequested = false
    def stopRequested: Boolean = stopWasRequested
    def requestStop() {
      stopWasRequested = true
    }
    def reset() {
      stopWasRequested = false
    }
  }

  private val stopper = new ResettableStopper

  private val exitSemaphore = new Semaphore(1)

  private var nextRunStamp = 1

  initialize()

  private def initialize() = {

    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

    val ambientURL: URL = classOf[Suite].getClassLoader().getResource("images/greendot.gif")
    val ambientIcon: ImageIcon = new ImageIcon(ambientURL)
    setIconImage(ambientIcon.getImage())

    setupMenus()

    runJButton.setMnemonic(KeyEvent.VK_R)
    runJButton.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          currentState = currentState.runButtonPressed(RunnerJFrame.this)
        }
      }
    )

    val pBarRunBtnJPanel: JPanel = new JPanel()

    pBarRunBtnJPanel.setLayout(new BorderLayout(5, 5))
    pBarRunBtnJPanel.add(progressBarPanel, BorderLayout.CENTER)
    pBarRunBtnJPanel.add(runJButton, BorderLayout.EAST)

    val progressJPanel: JPanel = new JPanel()

    progressJPanel.setLayout(new GridLayout(2, 1))
    progressJPanel.add(statusJPanel)
    progressJPanel.add(pBarRunBtnJPanel)
    val eventsJLabel: JLabel = new JLabel(Resources("eventsLabel"))

    val southHuggingEventsLabelJPanel: JPanel = new JPanel()

    southHuggingEventsLabelJPanel.setLayout(new BorderLayout())
    southHuggingEventsLabelJPanel.add(eventsJLabel, BorderLayout.SOUTH)
    southHuggingEventsLabelJPanel.setBorder(new EmptyBorder(0, 1, 0, 0))

    eventsJList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    eventsJList.setCellRenderer(new IconEmbellishedListCellRenderer())
    val eventsJScrollPane: JScrollPane = new JScrollPane(eventsJList)

    val eventsJPanel: JPanel = new JPanel()

    eventsJPanel.setLayout(new BorderLayout())
    eventsJPanel.add(southHuggingEventsLabelJPanel, BorderLayout.NORTH)
    eventsJPanel.add(eventsJScrollPane, BorderLayout.CENTER)
    val detailsJLabel: JLabel = new JLabel(Resources("detailsLabel"))

    val southHuggingDetailsLabelJPanel: JPanel = new JPanel()

    southHuggingDetailsLabelJPanel.setLayout(new BorderLayout())
    southHuggingDetailsLabelJPanel.add(detailsJLabel, BorderLayout.SOUTH)
    southHuggingDetailsLabelJPanel.setBorder(new EmptyBorder(0, 1, 0, 0))

    rerunJButton.setMnemonic(KeyEvent.VK_E)
    rerunJButton.setEnabled(false)

    val rerunColorBoxHolder: JPanel = new JPanel()

    rerunColorBoxHolder.setLayout(new BorderLayout())
    rerunColorBoxHolder.setBorder(new BevelBorder(BevelBorder.LOWERED))
    rerunColorBoxHolder.add(rerunColorBox, BorderLayout.CENTER)
    val rerunJPanel: JPanel = new JPanel()

    rerunJPanel.setLayout(new GridLayout(1, 2, 5, 5))
    rerunJPanel.add(rerunColorBoxHolder)
    rerunJPanel.add(rerunJButton)
    rerunJPanel.setBorder(new EmptyBorder(0, 0, 5, 0))
    val detailsNorthJPanel: JPanel = new JPanel()

    detailsNorthJPanel.setLayout(new BorderLayout())
    detailsNorthJPanel.add(BorderLayout.WEST, southHuggingDetailsLabelJPanel)
    detailsNorthJPanel.add(BorderLayout.EAST, rerunJPanel)

    detailsJEditorPane.setEditable(false)

    val detailsJScrollPane: JScrollPane = new JScrollPane(detailsJEditorPane)

    val detailsJPanel: JPanel = new JPanel()

    detailsJPanel.setLayout(new BorderLayout())
    detailsJPanel.add(detailsNorthJPanel, BorderLayout.NORTH)
    detailsJPanel.add(detailsJScrollPane, BorderLayout.CENTER)
    val eventsDetailsPanel: JPanel = new JPanel()

    eventsDetailsPanel.setLayout(new GridLayout(2, 1, 5, 5))
    eventsDetailsPanel.add(eventsJPanel)
    eventsDetailsPanel.add(detailsJPanel)
    val reporterJPanel: JPanel = new JPanel()

    reporterJPanel.setLayout(new BorderLayout(5, 5))
    reporterJPanel.add(progressJPanel, BorderLayout.NORTH)
    reporterJPanel.add(eventsDetailsPanel, BorderLayout.CENTER)
    eventsJList.addListSelectionListener(
      new ListSelectionListener() {
        def valueChanged(e: ListSelectionEvent) {

          val holder: EventHolder = eventsJList.getSelectedValue().asInstanceOf[EventHolder]

          if (holder == null) {

            // This means nothing is currently selected
            detailsJEditorPane.setText("")
            currentState = currentState.listSelectionChanged(RunnerJFrame.this)
          }
          else {

            val event: Event = holder.event
            val isRerun: Boolean = holder.isRerun
  
            val fontSize = eventsJList.getFont.getSize

            val title = 
              if (isRerun)
                Resources("RERUN_" + RunnerJFrame.getUpperCaseName(event))
              else
                Resources(RunnerJFrame.getUpperCaseName(event))

            val isFailureEvent =
              event match {
                case _: TestFailed => true
                case _: SuiteAborted => true
                case _: RunAborted => true
                case _ => false
              }

            val fileAndLineOption: Option[String] = 
              holder.throwable match {
                case Some(throwable) =>
                  throwable match {
                    case stackDepth: StackDepth =>
                      stackDepth.failedCodeFileNameAndLineNumberString
                    case _ => None
                  }
                case None => None
              }

              val throwableTitle = 
                holder.throwable match {
                  case Some(throwable) => Some(throwable.getClass.getName)
                  case None => None
                }

              // Any stack trace elements lower than a TestFailedException's failedCodeStackDepth
              // will show up as gray in the displayed stack trace, because those are ScalaTest methods.
              // The rest will show up as black.
              val (grayStackTraceElements, blackStackTraceElements) =
                holder.throwable match {
                  case Some(throwable) =>
                    val stackTraceElements = throwable.getStackTrace.toList
                    throwable match {
                      case tfe: TestFailedException =>
                        (stackTraceElements.take(tfe.failedCodeStackDepth), stackTraceElements.drop(tfe.failedCodeStackDepth))
                      case _ => (List(), stackTraceElements)
                    } 
                  case None => (List(), List())
                }

            def getHTMLForStackTrace(stackTraceList: List[StackTraceElement]) =
              stackTraceList.map((ste: StackTraceElement) => <span>{ ste.toString }</span><br />)

            def getHTMLForCause(throwable: Throwable): scala.xml.NodeBuffer = {
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
                  <span>
                  { 
                    if (cause.getMessage != null) 
                      // scala automatically change <br /> to <br></br>, which will cause 2 line breaks, use unparsedXml("<br />") to solve it.
                      for (line <- cause.getMessage.split('\n')) yield <span>{ xmlContent(line) }{ unparsedXml("<br />") }</span>
                    else 
                      Resources("None") 
                  }
                  </span>
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

            val mainMessage =
              holder.message match {
                case Some(msg) =>
                  val trimmed = msg.trim
                  if (trimmed.length > 0) Some(trimmed) else None
                    case _ => None
              }
            
            import EventHolder.suiteAndTestName

            def nameFromNameInfo(nameInfo: Option[NameInfo]): Option[String] = 
              nameInfo match {
                case Some(NameInfo(suiteName, suiteId, suiteClassName, testName)) =>
                  testName match {
                    case Some(testName) => Some(suiteAndTestName(suiteName, testName))
                    case None => Some(suiteName)
                  }
                case None => None
              }

            val name =
              holder.event match {
                case event: DiscoveryStarting => None
                case event: DiscoveryCompleted => None
                case event: RunStarting => None
                case event: RunStopped => None
                case event: RunAborted => None
                case event: RunCompleted => None
                case event: InfoProvided => nameFromNameInfo(event.nameInfo)
                case event: AlertProvided => nameFromNameInfo(event.nameInfo)
                case event: UpdateProvided => nameFromNameInfo(event.nameInfo)
                case event: ScopeOpened => nameFromNameInfo(Some(event.nameInfo))
                case event: ScopeClosed => nameFromNameInfo(Some(event.nameInfo))
                case event: ScopePending => nameFromNameInfo(Some(event.nameInfo))
                case event: MarkupProvided => nameFromNameInfo(event.nameInfo) // Should not get here because I'm not registering MarkupInfos
                case event: SuiteStarting => Some(event.suiteName)
                case event: SuiteCompleted => Some(event.suiteName)
                case event: SuiteAborted => Some(event.suiteName)
                case event: TestStarting => Some(suiteAndTestName(event.suiteName, event.testName))
                case event: TestPending => Some(suiteAndTestName(event.suiteName, event.testName))
                case event: TestCanceled => Some(suiteAndTestName(event.suiteName, event.testName))
                case event: TestIgnored => Some(suiteAndTestName(event.suiteName, event.testName))
                case event: TestSucceeded => Some(suiteAndTestName(event.suiteName, event.testName))
                case event: TestFailed => Some(suiteAndTestName(event.suiteName, event.testName))
              }
            
            def suiteIdFromNameInfo(nameInfo: Option[NameInfo]): Option[String] = 
              nameInfo match {
                case Some(NameInfo(suiteName, suiteId, suiteClassName, testName)) =>
                  Some(suiteId)
                case None => None
              }
            
            val suiteId = 
              holder.event match {
                case event: DiscoveryStarting => None
                case event: DiscoveryCompleted => None
                case event: RunStarting => None
                case event: RunStopped => None
                case event: RunAborted => None
                case event: RunCompleted => None
                case event: InfoProvided => suiteIdFromNameInfo(event.nameInfo)
                case event: AlertProvided => suiteIdFromNameInfo(event.nameInfo)
                case event: UpdateProvided => suiteIdFromNameInfo(event.nameInfo)
                case event: ScopeOpened => suiteIdFromNameInfo(Some(event.nameInfo))
                case event: ScopeClosed => suiteIdFromNameInfo(Some(event.nameInfo))
                case event: ScopePending => suiteIdFromNameInfo(Some(event.nameInfo))
                case event: MarkupProvided => suiteIdFromNameInfo(event.nameInfo) // Should not get here because I'm not registering MarkupInfos
                case event: SuiteStarting => Some(event.suiteId)
                case event: SuiteCompleted => Some(event.suiteId)
                case event: SuiteAborted => Some(event.suiteId)
                case event: TestStarting => Some(event.suiteId)
                case event: TestPending => Some(event.suiteId)
                case event: TestCanceled => Some(event.suiteId)
                case event: TestIgnored => Some(event.suiteId)
                case event: TestSucceeded => Some(event.suiteId)
                case event: TestFailed => Some(event.suiteId)
              }

            val duration =
              holder.event match {
                case event: DiscoveryStarting => None
                case event: DiscoveryCompleted => event.duration
                case event: RunStarting => None
                case event: RunStopped => event.duration
                case event: RunAborted => event.duration
                case event: RunCompleted => event.duration
                case event: InfoProvided => None
                case event: AlertProvided => None
                case event: UpdateProvided => None
                case event: ScopeOpened => None
                case event: ScopeClosed => None
                case event: ScopePending => None
                case event: MarkupProvided => None // Shouldn't get here because not registering MarkupInfos
                case event: SuiteStarting => None
                case event: SuiteCompleted => event.duration
                case event: SuiteAborted => event.duration
                case event: TestStarting => None
                case event: TestPending => None
                case event: TestCanceled => event.duration
                case event: TestIgnored => None
                case event: TestSucceeded => event.duration
                case event: TestFailed => event.duration
              }

            val detailsHTML =
              <html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
                <head>
                  <style type="text/css">
                    body {{ font-family: sans-serif; font-size: { fontSize }pt; }}
                    .label {{ color: #444444; font-weight: bold; }}
                    .gray {{ color: black; }}
                    .dark {{ font-weight: bold; color: #111111; }}
                  </style>
                </head>
                <body>
                  <table>
                  <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsEvent") + ":" }</span></td><td align="left"><span>{ title }</span></td></tr>
                  {
                    if (name.isDefined) {
                      <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsName") + ":" }</span></td><td align="left">{ name.get }</td></tr>
                    }
                    else <!-- -->
                  }
                  {
                    if (name.isDefined) {
                      <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsSuiteId") + ":" }</span></td><td align="left">{ suiteId.get }</td></tr>
                    }
                    else <!-- -->
                  }
                  {
                    if (mainMessage.isDefined) {
                      <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td><td align="left">
                      { 
                        // scala automatically change <br /> to <br></br>, which will cause 2 line breaks, use unparsedXml("<br />") to solve it.
                        def lineSpans = for (line <- mainMessage.get.split('\n')) yield <span>{ xmlContent(line) }{ unparsedXml("<br />") }</span>
                        if (isFailureEvent) {
                          <span class="dark">{ lineSpans }</span>
                        } else {
                          <span>{ lineSpans }</span>
                        }
                      }
                      </td></tr>
                    }
                    else <!-- -->
                  }
                  {
                    fileAndLineOption match {
                      case Some(fileAndLine) =>
                        <tr valign="top"><td align="right"><span class="label">{ Resources("LineNumber") + ":" }</span></td><td align="left"><span class="dark">{ "(" + fileAndLine + ")" }</span></td></tr>
                      case None =>
                    }
                  }
                  {
                    holder.summary match {
                      case Some(summary) => 

                        <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsSummary") + ":" }</span></td><td align="left"><strong>{ Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString) }</strong></td></tr>
                        <tr valign="top"><td align="right"><span class="label">&nbsp;</span></td><td align="left"><strong>{ Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString) }</strong></td></tr>
                        <tr valign="top"><td align="right"><span class="label">&nbsp;</span></td><td align="left"><strong>
                          {
                            Resources(
                              "testSummary",
                              summary.testsSucceededCount.toString,
                              summary.testsFailedCount.toString,
                              summary.testsCanceledCount.toString,
                              summary.testsIgnoredCount.toString,
                              summary.testsPendingCount.toString
                            )
                          }
                        </strong></td></tr>

                      case None => new scala.xml.NodeBuffer
                    }
                  }
                  {
                    duration match {
                      case Some(milliseconds) =>
                        <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsDuration") + ":" }</span></td><td align="left">{ PrintReporter.makeDurationString(milliseconds) }</td></tr>
                      case None => new scala.xml.NodeBuffer
                    }
                  }
                  <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsDate") + ":" }</span></td><td align="left">{ new java.util.Date(event.timeStamp) }</td></tr>
                  <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsThread") + ":" }</span></td><td align="left">{ event.threadName }</td></tr>
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
                  { grayStackTraceElements.map((ste: StackTraceElement) => <span class="gray">{ ste.toString }</span><br />) }
                  { blackStackTraceElements.map((ste: StackTraceElement) => <span>{ ste.toString }</span><br />) }
                  </td></tr>
                  </table>
                  {
                    holder.throwable match {
                      case Some(t) => getHTMLForCause(t)
                      case None =>
                    }
                  }
                </body>
              </html>

            detailsJEditorPane.setText(detailsHTML.toString)
            detailsJEditorPane.setCaretPosition(0)
            currentState = currentState.listSelectionChanged(RunnerJFrame.this)
          }
        }
      }
    )

    rerunJButton.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          currentState = currentState.rerunButtonPressed(RunnerJFrame.this)
        }
      }
    )
    val mainJPanel: JPanel = new JPanel()

    mainJPanel.setLayout(new BorderLayout(5, 5))
    mainJPanel.setBorder(new EmptyBorder(5, 5, 5, 5))
    mainJPanel.add(reporterJPanel, BorderLayout.CENTER)
    val pane: Container = getContentPane()

    pane.setLayout(new GridLayout(1, 1))
    pane.add(mainJPanel)

    // Set the size of both buttons to the max of the localized labels Run, Rerun, and Stop
    val runButtonSize: Dimension = runJButton.getPreferredSize()
    val rerunButtonSize: Dimension = rerunJButton.getPreferredSize()
    // Create a throw away button to get the size of Stop
    val stopButtonSize: Dimension = new JButton(Resources("Stop")).getPreferredSize()

    val preferredSize = new Dimension(
      runButtonSize.width.max(rerunButtonSize.width.max(stopButtonSize.width)),
      runButtonSize.height.max(rerunButtonSize.height.max(stopButtonSize.height))
    )

    runJButton.setPreferredSize(preferredSize)
    rerunJButton.setPreferredSize(preferredSize)

    exitSemaphore.acquire()
    addWindowListener(
      new WindowAdapter {
        override def windowClosed(e: WindowEvent) { exitSemaphore.release() }
      }
    )

    pack()

    val dim: Dimension = getSize()
    dim.height = dim.height / 5 + dim.height
    dim.width = dim.height / 3 * 4
    setSize(dim)
  }

  private[scalatest] def blockUntilWindowClosed() {
    exitSemaphore.acquire()
  }

  // This initialize method idiom is a way to get rid of a var
  // when you have verbose initialization.
  private def initializeAboutBox() = {
    val title2: String = Resources("AboutBoxTitle")
    new AboutJDialog(RunnerJFrame.this, title2)
  }

  private def setupMenus() {

    val menuBar: JMenuBar = new JMenuBar()

    // The ScalaTest menu 
    val scalaTestMenu: JMenu = new JMenu(Resources("ScalaTestMenu"))
    scalaTestMenu.setMnemonic(KeyEvent.VK_S)
    menuBar.add(scalaTestMenu)

    // The ScalaTest.About menu item
    val aboutItem: JMenuItem = new JMenuItem(Resources("About"), KeyEvent.VK_A)
    scalaTestMenu.add(aboutItem)
    aboutItem.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          val location: Point = getLocation()
          location.x += 20
          location.y += 6
          aboutBox.setLocation(location)
          aboutBox.setVisible(true)
        }
      }
    )

    scalaTestMenu.addSeparator()

    // The ScalaTest.Exit menu item
    val exitItem: JMenuItem = new JMenuItem(Resources("Exit"), KeyEvent.VK_X)
    scalaTestMenu.add(exitItem)
    exitItem.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          dispose()
          // Only exit if started from main(), not run(). If starting from run(),
          // we want to return a pass/fail status from run(). Actually, if we
          // have a passFailReporter, then that means we want to indicate status,
          // so that's why it is used here to determine whether or not to exit.
          passFailReporter match {
            case Some(_) =>
            case None => System.exit(0)
          }
        }
      }
    )

    // The View menu
    val viewMenu = new JMenu(Resources("ViewMenu"))
    viewMenu.setMnemonic(KeyEvent.VK_V)

    // the View.Runs and Failures menu item
    val runsFailuresItem: JMenuItem = new JMenuItem(Resources("runsFailures"), KeyEvent.VK_F)
    runsFailuresItem.setAccelerator(KeyStroke.getKeyStroke("control F"))
    viewMenu.add(runsFailuresItem)
    runsFailuresItem.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          viewOptions = runsAndFailures intersect eventTypesToCollect
          updateViewOptionsAndEventsList()
        }
      }
    )

    val allEventsItem: JMenuItem = new JMenuItem(Resources("allEvents"), KeyEvent.VK_A)
    allEventsItem.setAccelerator(KeyStroke.getKeyStroke("control L"))
    viewMenu.add(allEventsItem)
    allEventsItem.addActionListener(
      new ActionListener() {
        def actionPerformed(ae: ActionEvent) {
          viewOptions = eventTypesToCollect
          updateViewOptionsAndEventsList()
        }
      }
    )

    viewMenu.addSeparator()

    // Add the checkboxes in the correct order
    viewMenu.add(optionsMap(PresentRunStarting))
    viewMenu.add(optionsMap(PresentTestStarting))
    viewMenu.add(optionsMap(PresentTestSucceeded))
    viewMenu.add(optionsMap(PresentTestFailed))
    viewMenu.add(optionsMap(PresentTestIgnored))
    viewMenu.add(optionsMap(PresentTestPending))
    viewMenu.add(optionsMap(PresentTestCanceled))
    viewMenu.add(optionsMap(PresentScopeOpened))
    viewMenu.add(optionsMap(PresentScopeClosed))
    viewMenu.add(optionsMap(PresentScopePending))
    viewMenu.add(optionsMap(PresentSuiteStarting))
    viewMenu.add(optionsMap(PresentSuiteCompleted))
    viewMenu.add(optionsMap(PresentSuiteAborted))
    viewMenu.add(optionsMap(PresentInfoProvided))
    viewMenu.add(optionsMap(PresentAlertProvided))
    viewMenu.add(optionsMap(PresentUpdateProvided))
    viewMenu.add(optionsMap(PresentMarkupProvided))
    viewMenu.add(optionsMap(PresentRunStopped))
    viewMenu.add(optionsMap(PresentRunCompleted))
    viewMenu.add(optionsMap(PresentRunAborted))

    menuBar.add(viewMenu)
    setJMenuBar(menuBar)
  }

  private def initializeOptionsMap(): Map[EventToPresent, JCheckBoxMenuItem] = {

    // TODO: Why am I using an immutable map here. Better a val with a mutable map I'd think.
    var map: Map[EventToPresent, JCheckBoxMenuItem] = Map()

    for (option <- EventToPresent.allEventsToPresent) {

      val rawOptionName = RunnerJFrame.getUpperCaseName(option)
      val menuItemText: String = Resources("MENU_PRESENT_" + rawOptionName)

      val itemAction: AbstractAction =
        new AbstractAction(menuItemText) {
          def actionPerformed(ae: ActionEvent) {

            val checkBox: JCheckBoxMenuItem = ae.getSource().asInstanceOf[JCheckBoxMenuItem]
            val option = getValue("option").asInstanceOf[EventToPresent]

            if (viewOptions.contains(option))
              viewOptions = viewOptions - option
            else
              viewOptions = viewOptions + option

            val checked: Boolean = viewOptions.contains(option)
            checkBox.setState(checked)

            // Now, since the configuration changed, we need to update the
            // list display appropriately:
            refreshEventsJList()
          }
        }

      val checked: Boolean = viewOptions.contains(option)
      val checkBox: JCheckBoxMenuItem = new JCheckBoxMenuItem(itemAction)

      checkBox.setState(checked)

      // Put the option into the checkbox's AbstractAction, so it can be
      // taken out when the checkbox is checked or unchecked.
      itemAction.putValue("option", option)

      map = map + (option -> checkBox)
    }

    map
  }

  def requestStop() {
    stopper.requestStop()
  }

  private def updateViewOptionsAndEventsList() {

    for (option <- EventToPresent.allEventsToPresent) {

      val box: JCheckBoxMenuItem = optionsMap(option)

      if (eventTypesToCollect.contains(option)) {
        box.setEnabled(true)
        if (viewOptions.contains(option))
          box.setSelected(true)
        else
          box.setSelected(false)
      }
      else {
        box.setSelected(false)
        box.setEnabled(false)
      }
    }

    // Now, since the configuration changed, we need to update the
    // list display appropriately:
    refreshEventsJList()
  }

  private def reorderCollectedEvents() {
    collectedEvents = collectedEvents.sortWith((a, b) => a.event.ordinal > b.event.ordinal)
  }

  private def refreshEventsJList() {

    val formerlySelectedItem: EventHolder = eventsJList.getSelectedValue().asInstanceOf[EventHolder]

    // clear the list of events and the detail area
    eventsListModel.clear()
    detailsJEditorPane.setText("")

    for (holder <- collectedEvents.reverse; if viewOptions.contains(eventToEventToPresent(holder.event))) {
      val shouldAddElement = holder.event.formatter match {
        case Some(MotionToSuppress) => false
        case _ => true
      }
      if (shouldAddElement) eventsListModel.addElement(holder)
    }

    // Isn't there a risk that the formerly selected item will no longer exist in the list?
    // Does this result in an exception? Of course the stupid JavaDoc API docs is silent on this.
    // TODO: try this and fix if need be
    eventsJList.setSelectedValue(formerlySelectedItem, true)
  }

  private def registerEvent(event: Event): EventHolder = {
    registerRunOrRerunEvent(event, false)
  }

  private def registerRerunEvent(event: Event): EventHolder = {
    registerRunOrRerunEvent(event, true)
  }

  private def registerRunOrRerunEvent(event: Event, isRerun: Boolean): EventHolder = {

    val (message, throwable) =
      event match {
        case e: TestFailed => (Some(e.message), e.throwable)
        case e: SuiteAborted => (Some(e.message), e.throwable)
        case e: RunAborted => (Some(e.message), e.throwable)
        case e: InfoProvided => (Some(e.message), e.throwable)
        case e: AlertProvided => (Some(e.message), e.throwable)
        case e: UpdateProvided => (Some(e.message), e.throwable)
        case _ => (None, None)
      }

    val rerunner =
      event match {
        case e: TestStarting => e.rerunner
        case e: TestSucceeded => e.rerunner
        case e: TestFailed => e.rerunner
        case e: TestCanceled => e.rerunner
        case e: SuiteStarting => e.rerunner
        case e: SuiteCompleted => e.rerunner
        case e: SuiteAborted => e.rerunner
        case _ => None
      }

    val summary =
      event match {
        case e: RunCompleted => e.summary
        case e: RunAborted => e.summary
        case e: RunStopped => e.summary
        case _ => None
      }

    val eventHolder: EventHolder = new EventHolder(event, message, throwable, rerunner, summary, isRerun)

    if (eventTypesToCollect.contains(eventToEventToPresent(event))) {
      collectedEvents = eventHolder :: collectedEvents
      if (viewOptions.contains(eventToEventToPresent(event))) {
        val shouldAddElement = event.formatter match {
          case Some(MotionToSuppress) => false
          case _ => true
        }
        if (shouldAddElement) eventsListModel.addElement(eventHolder)
      }
    }

    eventHolder
  }

  private class GraphicRunReporter extends Reporter {

    override def apply(event: Event) {
      event match {
        case _: DiscoveryStarting  =>
          usingEventDispatchThread {
            progressBarPanel.discoveryStarting()
          }

        case _: DiscoveryCompleted =>
          usingEventDispatchThread {
            progressBarPanel.discoveryCompleted()
          }

        case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) =>

          // Create the Event outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is wrong to boot.
          val eventHolder: EventHolder = new EventHolder(event, None, None, None)

          usingEventDispatchThread {
            testsCompletedCount = 0
            progressBarPanel.runStarting(testCount)
            statusJPanel.reset()
            statusJPanel.setTestsExpected(testCount)
  
            // This should already have been cleared by prepUIForStarting, but
            // doing it again here for the heck of it.
            collectedEvents = eventHolder :: Nil
            eventsListModel.clear()
  
            detailsJEditorPane.setText("")

            if (viewOptions.contains(PresentRunStarting))
              eventsListModel.addElement(eventHolder)
          }

        case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

          // Create the Report outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is wrong to boot.
          // Reordering this outside the event handler thread so that the GUI won't be frozen
          // during a long sort. The RunCompleted event isn't yet in the list when the sort happens, so
          // it will just be added at the end. 
          reorderCollectedEvents()
          usingEventDispatchThread {
            registerEvent(event)
            refreshEventsJList()
          }
  
        case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            progressBarPanel.runAborted()
            registerEvent(event)
            // Must do this here, not in RunningState.runFinished, because the runFinished
            // invocation can happen before this runCompleted invocation, which means that 
            // the first error in the run may not be in the JList model yet. So must wait until
            // a run completes. I was doing it in runCompleted, which works, but for long runs
            // you must wait a long time for that thing to be selected. Nice if it gets selected
            // right away.
            selectFirstFailureIfExistsAndNothingElseAlreadySelected()
          }

        case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

          // Create the Report outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is actually wrong.
          usingEventDispatchThread {
            registerEvent(event)
          }

        case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }
  
        case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) => 
  
          usingEventDispatchThread {
            registerEvent(event)
          }

        case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            progressBarPanel.suiteAborted()
            registerEvent(event)
            // Must do this here, not in RunningState.runFinished, because the runFinished
            // invocation can happen before this runCompleted invocation, which means that 
            // the first error in the run may not be in the JList model yet. So must wait until
            // a run completes. I was doing it in runCompleted, which works, but for long runs
            // you must wait a long time for that thing to be selected. Nice if it gets selected
            // right away.
            selectFirstFailureIfExistsAndNothingElseAlreadySelected()
          }

        case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunner, payload, threadName, timeStamp) =>
  
          usingEventDispatchThread {
            registerEvent(event)
          }

        case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            registerEvent(event)
          }
  
        case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            testsCompletedCount += 1
            statusJPanel.setTestsRun(testsCompletedCount, true)
            progressBarPanel.setTestsRun(testsCompletedCount)
            registerEvent(event)
            recordedEvents.foreach(registerEvent(_))
          }

        case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            testsCompletedCount += 1
            statusJPanel.setTestsRun(testsCompletedCount, true)
            progressBarPanel.setTestsRun(testsCompletedCount)
            registerEvent(event)
            recordedEvents.foreach(registerEvent(_))
          }

        case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
  
          usingEventDispatchThread {
            testsCompletedCount += 1
            statusJPanel.setTestsRun(testsCompletedCount, true)
            progressBarPanel.setTestsRun(testsCompletedCount)
            registerEvent(event)
            recordedEvents.foreach(registerEvent(_))
          }
  
        case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            testsCompletedCount += 1
            // Passing in false here increments the test failed count
            // in the statusJPanel, which updates the counter on the GUI
            statusJPanel.setTestsRun(testsCompletedCount, false)
            progressBarPanel.testFailed(testsCompletedCount)
            registerEvent(event)
            recordedEvents.foreach(registerEvent(_))
            // Must do this here, not in RunningState.runFinished, because the runFinished
            // invocation can happen before this runCompleted invocation, which means that 
            // the first error in the run may not be in the JList model yet. So must wait until
            // a run completes. I was doing it in runCompleted, which works, but for long runs
            // you must wait a long time for that thing to be selected. Nice if it gets selected
            // right away.
            selectFirstFailureIfExistsAndNothingElseAlreadySelected()
          }

        case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }

        case AlertProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }

        case UpdateProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }

        case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }

        case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerEvent(event)
          }
          
        case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
          usingEventDispatchThread {
            registerEvent(event)
          }
          
        case MarkupProvided(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
          usingEventDispatchThread {
            registerEvent(event)
          }
      }
    }
  }

  // Invoked when a test is done. This is used to turn the Run button back on after
  // a Stop request has disabled it. When this method is invoked by the runner, it
  // means that the run has finished, so that it is OK to enable Run again.
  override def done() {
    usingEventDispatchThread {
      currentState = currentState.runFinished(RunnerJFrame.this)
    }
  }

  // Called from the main thread initially, thereafter from the event handler thread
  override def runFromGUI() {
    (new RunnerThread).start()
  }

  // Must be called from event handler thread
  override def rerunFromGUI(rerunner: Rerunner) {
    (new RerunnerThread(rerunner)).start()
  }

  // This must be called by the event handler thread
  def prepUIForRunning() {
    val stopText: String = Resources("Stop")
    val rerunText: String = Resources("Rerun")
    runJButton.setText(stopText)
    rerunJButton.setText(rerunText)
    runJButton.setEnabled(true)
    rerunJButton.setEnabled(false)
    rerunColorBox.setGray()
    progressBarPanel.reset()
    statusJPanel.reset()
    statusJPanel.setTestsExpected(0)
    collectedEvents = Nil
    eventsListModel.clear()
    detailsJEditorPane.setText("")
  }

  // This must be called by the event handler thread
  def prepUIWhileRunning() {
    val stopText: String = Resources("Stop")
    val rerunText: String = Resources("Rerun")
    runJButton.setText(stopText)
    rerunJButton.setText(rerunText)
    runJButton.setEnabled(true)
    rerunJButton.setEnabled(false)
    rerunColorBox.setGray()
  }

  // This must be called by the event handler thread
  def prepUIForRerunning() {
    val runText: String = Resources("Run")
    val stopText: String = Resources("Stop")
    runJButton.setText(runText)
    rerunJButton.setText(stopText)
    runJButton.setEnabled(false)
    rerunJButton.setEnabled(true)
    rerunColorBox.setGray()

    // Clear the selection, so it can scroll to an error
    eventsJList.clearSelection() 
  }

  // This must be called by the event handler thread
  def prepUIWhileRerunning() {
    val runText: String = Resources("Run")
    val stopText: String = Resources("Stop")
    runJButton.setText(runText)
    rerunJButton.setText(stopText)
    runJButton.setEnabled(false)
    rerunJButton.setEnabled(true)
  }

  // This must be called by the event handler thread
  def prepUIForReady() {
    val runText: String = Resources("Run")
    val rerunText: String = Resources("Rerun")
    runJButton.setText(runText)
    rerunJButton.setText(rerunText)
    runJButton.setEnabled(true)
    val holder: EventHolder = eventsJList.getSelectedValue.asInstanceOf[EventHolder]
    rerunJButton.setEnabled(holder != null && holder.rerunner.isDefined)
  }

  // This must be called by the event handler thread
  def prepUIForStopping() {
    val stopText: String = Resources("Stop")
    val rerunText: String = Resources("Rerun")
    runJButton.setText(stopText)
    rerunJButton.setText(rerunText)
    runJButton.setEnabled(false)
    rerunJButton.setEnabled(false)
  }

  // This must be called by the event handler thread
  def prepUIForReStopping() {
    val runText: String = Resources("Run")
    val stopText: String = Resources("Stop")
    runJButton.setText(runText)
    rerunJButton.setText(stopText)
    runJButton.setEnabled(false)
    rerunJButton.setEnabled(false)
  }

  private def getModelAsList: List[EventHolder] = {
    val model = eventsJList.getModel
    val listBuf = new scala.collection.mutable.ListBuffer[EventHolder]
    for (i <- 0 until model.getSize) {
      listBuf += model.getElementAt(i).asInstanceOf[EventHolder]
    }
    listBuf.toList
  }

  private def isFailureEvent(eventHolder: EventHolder) =
    eventHolder.event match {
      case _: TestFailed => true
      case _: SuiteAborted => true
      case _: RunAborted => true
      case _ => false
    }

  // This must be called by the event handler thread
  /*
  After a rerun, there will usually always be a selected item,
  which is what was selected to rerun. If the rerun resulted in
  an error, it would be nice to select that first error and scroll down.
  So this will do that, which means it doesn't care if something is
  already selected. It will always select the first error in the
  last rerun if one exists. This is called as errors come in during
  a rerun. The error's eventHolder is passed. It will be selected only
  if it is the first error in the last rerun. Any other time this method will
  do nothing. The reason is that reruns can take a while, and the user may be
  selecting and exploring the results as it runs. So I don't want to keep forcing
  a different selection. Only the first time an error comes in in a rerun will it happen.
  (During a run, the first error will be selected only if there is no other selection. But
  here it happens even if something else is selected, because during a rerun normally the
  thing you wanted to rerun will already be selected.)
  */
  private def selectFirstErrorInLastRerunIfThisIsThatError(candidateEventHolder: EventHolder) {

    // First get the model into a List
    val modelList = getModelAsList

    if (modelList.exists(_.isRerun)) {
      val listOfEventsForLastRerunExcludingRunStarting =
        modelList.reverse.takeWhile(eventHolder => eventHolder.isRerun && !eventHolder.event.isInstanceOf[RunStarting])
      val firstTestFailedEventInLastRerun =
        listOfEventsForLastRerunExcludingRunStarting.reverse.find(isFailureEvent(_))
      firstTestFailedEventInLastRerun match {
        case Some(eventHolder) =>
          if (eventHolder == candidateEventHolder) // Only select it if the one passed is the first one
            eventsJList.setSelectedValue(eventHolder, true)
        case None => // do nothing if no failure events in last rerun
      }
    }
  }

  private def scrollTheRerunStartingEventToTheTopOfVisibleEvents() {

    def indexOfRunStartingEventForLastRerunOption: Option[Int] = {
      var i = eventsListModel.getSize - 1
      var found = false
      while (i >= 0 && !found) {
        val holder = eventsListModel.getElementAt(i).asInstanceOf[EventHolder]
        if (holder.event.isInstanceOf[RunStarting]) {
          found = true
        }
        if (!found) i -= 1
      }
      if (found) Some(i) else None
    }

    val selectedEventHandler = eventsJList.getSelectedValue.asInstanceOf[EventHolder]

    if (selectedEventHandler == null || selectedEventHandler.event.isInstanceOf[RunStarting]) { // only scroll if there's no selection, which means no error happened

      val firstVisibleIndex = eventsJList.getFirstVisibleIndex
      val lastVisibleIndex = eventsJList.getLastVisibleIndex

      if (lastVisibleIndex > firstVisibleIndex) { // should always be true, but this is better than an assert because things will keep going

        val numCellsVisible = lastVisibleIndex - firstVisibleIndex

        val indexOfLastEvent = eventsListModel.getSize - 1

        indexOfRunStartingEventForLastRerunOption match {
          case Some(indexOfRunStartingEventForLastRerun) =>

            val indexToEnsureIsVisible =
              if (indexOfRunStartingEventForLastRerun + numCellsVisible < indexOfLastEvent) 
                indexOfRunStartingEventForLastRerun + numCellsVisible
              else
                indexOfLastEvent

            eventsJList.ensureIndexIsVisible(indexToEnsureIsVisible)

            // Select one event after the rerun starting event, if it is a test starting, test succeeded, or suite starting event,
            // because this should be the one they requested was rerun. So that's the most intuitive one to select
            // after a run if there was no error. (Test succeeded is possible because Spec's will send MotionToSupress formatters that
            // say not to display test starting events.)
            val indexOfSecondEventInRerun = indexOfRunStartingEventForLastRerun + 1
            if (indexOfSecondEventInRerun <= indexOfLastEvent) { // Should always be true, but an if is better than an assert

              val firstEventAfterRerunStarting = eventsListModel.getElementAt(indexOfSecondEventInRerun).asInstanceOf[EventHolder]
              if (firstEventAfterRerunStarting.event.isInstanceOf[TestStarting] ||
                  firstEventAfterRerunStarting.event.isInstanceOf[SuiteStarting] ||
                  firstEventAfterRerunStarting.event.isInstanceOf[TestSucceeded]) {
                eventsJList.setSelectedIndex(indexOfSecondEventInRerun)
              }
              // If they have display only Runs and Failures selected, it won't show successful tests. In that case
              // just select the run starting event.
              else eventsJList.setSelectedIndex(indexOfRunStartingEventForLastRerun)
            }
          case None =>
        }
      }
    }
  }

  // This must be called by the event handler thread
  private def selectFirstFailureIfExistsAndNothingElseAlreadySelected() {

    val holder: EventHolder = eventsJList.getSelectedValue.asInstanceOf[EventHolder]

    if (holder == null) { // Only do this if something isn't already selected

      // First get the model into a List
      val modelList = getModelAsList

      val firstFailureEvent = modelList.find(isFailureEvent(_))
      firstFailureEvent match {
        case Some(eventHolder) => eventsJList.setSelectedValue(eventHolder, true)
        case None => // do nothing if no failure events in the run
      }
    }
  }

  def getSelectedRerunner(): Option[Rerunner] = {
    val holder: EventHolder = eventsJList.getSelectedValue().asInstanceOf[EventHolder]
    if (holder == null)
      None
    else {
      holder.rerunner match {
        case Some(rerunner) => 
          holder.event match {
            case e: TestStarting => Some(new TestRerunner(rerunner, e.testName))
            case e: TestSucceeded => Some(new TestRerunner(rerunner, e.testName))
            case e: TestFailed => Some(new TestRerunner(rerunner, e.testName))
            case e: TestCanceled => Some(new TestRerunner(rerunner, e.testName))
            case e: SuiteStarting => Some(new SuiteRerunner(rerunner))
            case e: SuiteCompleted => Some(new SuiteRerunner(rerunner))
            case e: SuiteAborted => Some(new SuiteRerunner(rerunner))
            case _ => None
          }
        case None =>
          None
      }
    }
  }

  private class GraphicRerunReporter extends Reporter {

    // This is written by the event handler thread to avoid having the event handler thread spend time
    // determining if an error has previously occurred by looking through the events. This way if a
    // rerun has a lot of errors, you don't hang up the GUI giving the event handler thread too much
    // work to do.
    var anErrorHasOccurredAlready = false

    def apply(event: Event) {

      event match {
        case _: DiscoveryStarting => None
        case _: DiscoveryCompleted => None

        case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) =>

          // Create the Report outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is actually wrong.
  
          usingEventDispatchThread {
            rerunTestsCompletedCount = 0
            rerunColorBox.setMax(testCount)
            rerunColorBox.setValue(0)
            rerunColorBox.setGreen()
  
            registerRerunEvent(event)
            anErrorHasOccurredAlready = false;
          }

        case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

          // Create the Report outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is actually wrong.

          usingEventDispatchThread {
            registerRerunEvent(event)
            scrollTheRerunStartingEventToTheTopOfVisibleEvents()
          }
  
        case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            progressBarPanel.runAborted()
            rerunColorBox.setRed()
            val eventHolder = registerRerunEvent(event)
            if (!anErrorHasOccurredAlready) {
              selectFirstErrorInLastRerunIfThisIsThatError(eventHolder)
              anErrorHasOccurredAlready = true
            }
          }

        case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
  
          // Create the Report outside of the event handler thread, because otherwise
          // the event handler thread shows up as the originating thread of this event,
          // and that looks bad and is actually wrong.
          usingEventDispatchThread {
            registerRerunEvent(event)
            scrollTheRerunStartingEventToTheTopOfVisibleEvents()
          }

        case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) => 
  
          usingEventDispatchThread {
            registerRerunEvent(event)
          }

        case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            rerunColorBox.setRed()
            val eventHolder = registerRerunEvent(event)
            if (!anErrorHasOccurredAlready) {
              selectFirstErrorInLastRerunIfThisIsThatError(eventHolder)
              anErrorHasOccurredAlready = true
            }
          }
 
        case TestStarting(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case TestIgnored(ordinal, suiteName, suiteId, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

          usingEventDispatchThread {
            rerunColorBox.setValue(rerunTestsCompletedCount)
            registerRerunEvent(event)
          }

        case TestPending(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            rerunColorBox.setValue(rerunTestsCompletedCount)
            registerRerunEvent(event)
            recordedEvents.foreach(registerRerunEvent(_))
          }

        case TestCanceled(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            rerunColorBox.setValue(rerunTestsCompletedCount)
            registerRerunEvent(event)
            recordedEvents.foreach(registerRerunEvent(_))
          }

        case TestSucceeded(ordinal, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            rerunTestsCompletedCount += 1
            rerunColorBox.setValue(rerunTestsCompletedCount)
            registerRerunEvent(event)
            recordedEvents.foreach(registerRerunEvent(_))
          }

        case TestFailed(ordinal, message, suiteName, suiteId, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            rerunTestsCompletedCount += 1
            rerunColorBox.setValue(rerunTestsCompletedCount)
            rerunColorBox.setRed()
            val eventHolder = registerRerunEvent(event)
            recordedEvents.foreach(registerRerunEvent(_))
            if (!anErrorHasOccurredAlready) {
              selectFirstErrorInLastRerunIfThisIsThatError(eventHolder)
              anErrorHasOccurredAlready = true
            }
          }
  
        case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case AlertProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case UpdateProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
  
        case ScopePending(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>

          usingEventDispatchThread {
            registerRerunEvent(event)
          }
          
        case MarkupProvided(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
          usingEventDispatchThread {
            registerRerunEvent(event)
          }
      }
    }
  }

  // Invoked by ReadyState if can't run when the Run or Rerun buttons
  // are pressed. May never happen. If so, delete this. Before it was
  // commented that the problem could occur when they change the prefs.
  def showErrorDialog(title: String, msg: String) {
    val jOptionPane: JOptionPane = new NarrowJOptionPane(msg, JOptionPane.ERROR_MESSAGE)
    val jd: JDialog = jOptionPane.createDialog(RunnerJFrame.this, title)
    jd.setVisible(true)
  }

  private class RunnerThread extends Thread {

    override def run() {
  
      withClassLoaderAndDispatchReporter(
        runpathList,
        reporterConfigurations,
        Some(graphicRunReporter),
        passFailReporter,
        detectSlowpokes,
        slowpokeDetectionDelay,
        slowpokeDetectionPeriod
      ) { (loader, dispatchReporter) =>
        try {
          Runner.doRunRunRunDaDoRunRun(
            dispatchReporter,
            suitesList,
            agains,
            testSpecs,
            junitsList,
            stopper,
            tagsToIncludeSet,
            tagsToExcludeSet,
            propertiesMap,
            concurrent,
            memberOfList,
            beginsWithList,
            testNGList,
            runpathList,
            loader,
            RunnerJFrame.this,
            nextRunStamp,
            concurrentConfig,
            suffixes,
            chosenStyleSet
          )
        }
        finally {
          stopper.reset()
          nextRunStamp += 1
        }
      }
    }
  }

  private class RerunnerThread(rerun: Rerunner) extends Thread {

    if (rerun == null)
      throw new NullPointerException

    override def run() {
  
      val distributor: Option[Distributor] = None

      val tracker = new Tracker(new Ordinal(nextRunStamp))

      withClassLoaderAndDispatchReporter(
        runpathList,
        reporterConfigurations,
        Some(graphicRerunReporter),
        None,
        detectSlowpokes,
        slowpokeDetectionDelay,
        slowpokeDetectionPeriod
      ) { (loader, dispatchReporter) =>
        try {
          val filter = Filter(if (tagsToIncludeSet.isEmpty) None else Some(tagsToIncludeSet), tagsToExcludeSet)
          rerun(dispatchReporter, stopper, filter, propertiesMap,
              distributor, tracker, loader)
        }
        catch {
          case e: Throwable => {
            dispatchReporter.apply(RunAborted(tracker.nextOrdinal(), Resources.bigProblems(e), Some(e)))
          }
        }
        finally {
          stopper.reset()
          RunnerJFrame.this.done()
          nextRunStamp += 1
        }
      }
    }
  }
}

private[tools] object RunnerJFrame {

  def getUpperCaseName(event: Event): String = getUpperCaseName(eventToEventToPresent(event))

  def getUpperCaseName(eventToPresent: EventToPresent) =
    eventToPresent match {
      case PresentDiscoveryStarting => "DISCOVERY_STARTING"
      case PresentDiscoveryCompleted => "DISCOVERY_COMPLETED"
      case PresentRunStarting => "RUN_STARTING"
      case PresentTestStarting => "TEST_STARTING"
      case PresentTestFailed => "TEST_FAILED"
      case PresentTestSucceeded => "TEST_SUCCEEDED"
      case PresentTestIgnored => "TEST_IGNORED"
      case PresentTestPending => "TEST_PENDING"
      case PresentTestCanceled => "TEST_CANCELED"
      case PresentSuiteStarting => "SUITE_STARTING"
      case PresentSuiteAborted => "SUITE_ABORTED"
      case PresentSuiteCompleted => "SUITE_COMPLETED"
      case PresentInfoProvided => "INFO_PROVIDED"
      case PresentAlertProvided => "ALERT_PROVIDED"
      case PresentUpdateProvided => "UPDATE_PROVIDED"
      case PresentScopeOpened => "SCOPE_OPENED"
      case PresentScopeClosed => "SCOPE_CLOSED"
      case PresentScopePending => "SCOPE_PENDING"
      case PresentMarkupProvided => "MARKUP_PROVIDED"
      case PresentRunStopped => "RUN_STOPPED"
      case PresentRunAborted => "RUN_ABORTED"
      case PresentRunCompleted => "RUN_COMPLETED"
    }
}

