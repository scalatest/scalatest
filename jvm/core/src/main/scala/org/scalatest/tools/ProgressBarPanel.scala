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
package org.scalatest.tools

import org.scalatest.Resources

import java.awt.BorderLayout
import java.awt.Dimension
import java.net.URL
import javax.swing.border.BevelBorder
import javax.swing.ImageIcon
import javax.swing.JLabel
import javax.swing.JPanel

/**
 * A JPanel that shows the progress bar as tests are running, or a
 * 'doing discovery' message with an animated icon if discovery is in
 * process.
 */
private[scalatest] class ProgressBarPanel() extends JPanel {
  private val progressBar = new ColorBar()
  private val discoPanel = new JPanel
  private val discoJLabel = new JLabel
  private val discoImageURL: URL =
    getClass.getClassLoader.getResource("images/inProgress.gif")

  setLayout(new BorderLayout())
  setBorder(new BevelBorder(BevelBorder.LOWERED))
  add(progressBar, BorderLayout.CENTER)

  discoJLabel.setIcon(new ImageIcon(discoImageURL))
  discoPanel.add(discoJLabel)

  //
  // This prevents resizing of the panel when switching between
  // the color bar and the discovery message panel.  
  //
  setPreferredSize(
    new Dimension(
      getPreferredSize.getWidth.toInt,
      discoPanel.getPreferredSize.getHeight.toInt + 4))

  //
  // Replaces progress bar with discovery message.
  //
  def discoveryStarting(): Unit = {
    remove(progressBar)
    add(discoPanel, BorderLayout.WEST)

    // change text after adding panel in order to get panel to show up
    discoJLabel.setText("")
    discoJLabel.setText(Resources.doingDiscovery)
  }

  private def showProgressBar(): Unit = {
    remove(discoPanel)
    add(progressBar, BorderLayout.CENTER)
  }

  def runAborted(): Unit = {
    showProgressBar()
    progressBar.setRed()
  }

  def discoveryCompleted(): Unit = {
    showProgressBar()
  }

  def runStarting(testCount: Int): Unit = {
     progressBar.setMax(testCount)
     progressBar.setValue(0)
     progressBar.setGreen()
  }

  def setTestsRun(testsCompletedCount: Int): Unit = {
    progressBar.setValue(testsCompletedCount)
  }

  def suiteAborted(): Unit = {
    progressBar.setRed()
  }

  def testFailed(testsCompletedCount: Int): Unit = {
    progressBar.setRed()
    setTestsRun(testsCompletedCount)
  }

  def reset(): Unit = {
    showProgressBar()
    progressBar.setGray()
  }
}
