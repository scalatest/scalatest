/*
 * Copyright 2001-2024 Artima, Inc.
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
 * Trait implemented by RunnerJFrame, which can be passed to the RunnerGUI State objects,
 * allowing them to call back into RunnerJFrame.
 *
 * @author Bill Venners
 */
private[scalatest] trait RunnerGUI {

  def prepUIForReady(): Unit
  def prepUIForRunning(): Unit
  def prepUIForRerunning(): Unit
  def prepUIWhileRunning(): Unit
  def prepUIWhileRerunning(): Unit
  def prepUIForStopping(): Unit
  def prepUIForReStopping(): Unit
  def showErrorDialog(title: String, msg: String): Unit
  def getSelectedRerunner(): Option[Rerunner]
  def runFromGUI(): Unit
  def rerunFromGUI(rerunnable: Rerunner): Unit
  def requestStop(): Unit
}
