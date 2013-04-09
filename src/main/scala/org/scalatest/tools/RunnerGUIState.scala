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

/**
 * A set of state objects for the GUI. This is the Gang of Four state pattern.
 *
 * @author Bill Venners
 */
private[scalatest] trait RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState
  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState
  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState
  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState
}

private[scalatest] object ReStoppingState extends RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReStopping()
    this
  }

  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReStopping()
    this
  }

  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReStopping()
    this
  }

  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    ReadyState
  }
}

private[scalatest] object ReadyState extends RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    try {
      runnerGUI.prepUIForRunning()
      runnerGUI.runFromGUI()
      RunningState
    }
    catch {
      case e: IllegalArgumentException => {
        runnerGUI.prepUIForReady()
        runnerGUI.showErrorDialog(Resources("couldntRun"), e.getMessage())
        this
      }
    }
  }

  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {

    runnerGUI.getSelectedRerunner() match {
      case None => {
        runnerGUI.prepUIForReady()
        this
      }
      case Some(rerunnable) => {
        runnerGUI.prepUIForRerunning()
        try {
          runnerGUI.rerunFromGUI(rerunnable)
          RerunningState
        }
        catch {
          case e: IllegalArgumentException => {
            runnerGUI.prepUIForReady()
            runnerGUI.showErrorDialog(Resources("couldntRerun"), e.getMessage())
            this
          }
        }
      }
    }
  }

  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    this
  }

  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    this
  }
}

private[scalatest] object RerunningState extends RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForRerunning()
    this
  }

  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReStopping()
    runnerGUI.requestStop()
    ReStoppingState
  }

  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIWhileRerunning()
    this
  }

  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    ReadyState
  }
}

private[scalatest] object RunningState extends RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForStopping()
    runnerGUI.requestStop()
    StoppingState
  }

  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIWhileRunning()
    this
  }

  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIWhileRunning()
    this
  }

  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    ReadyState
  }
}

private[scalatest] object StoppingState extends RunnerGUIState {

  def runButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForStopping()
    this
  }

  def rerunButtonPressed(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForStopping()
    this
  }

  def listSelectionChanged(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForStopping()
    this
  }

  def runFinished(runnerGUI: RunnerGUI): RunnerGUIState = {
    runnerGUI.prepUIForReady()
    ReadyState
  }
}
