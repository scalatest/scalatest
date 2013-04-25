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
import java.awt.BorderLayout
import java.awt.FlowLayout
import java.awt.GridLayout
import javax.swing.JLabel
import javax.swing.JPanel

/**
 * A JPanel that shows the number of tests expected, completed, and failed.
 *
 * @author Bill Venners
 */
private[scalatest] class StatusJPanel extends JPanel {
  
  private var testsExpected: Int = 0
  private var testsRun: Int = 0
  private var testsFailed: Int = 0
  private val testsRunNumJLabel: JLabel = new JLabel("0")
  private val testsExpectedNumJLabel: JLabel = new JLabel("0")
  private val testsFailedNumJLabel: JLabel = new JLabel("0")

  // Interesting, I didn't realize until now I can 't put
  // local variables into primary constructors unless I make
  // an initialize method. Is this true?
  initialize()

  private def initialize() {

    val testsRunJLabel: JLabel = new JLabel(Resources("testsRun"))
    val testsExpectedJLabel: JLabel = new JLabel(Resources("testsExpected"))
    val testsFailedJLabel: JLabel = new JLabel(Resources("testsFailed"))
  
    val testsRunJPanel: JPanel = new JPanel()
  
    testsRunJPanel.setLayout(new BorderLayout(4, 4))
    testsRunJPanel.add(testsRunJLabel, BorderLayout.WEST)
    testsRunJPanel.add(testsRunNumJLabel, BorderLayout.CENTER)
    val testsExpectedJPanel: JPanel = new JPanel()
  
    testsExpectedJPanel.setLayout(new BorderLayout(4, 4))
    testsExpectedJPanel.add(testsExpectedJLabel, BorderLayout.WEST)
    testsExpectedJPanel.add(testsExpectedNumJLabel, BorderLayout.CENTER)
    val testsFailedJPanel: JPanel = new JPanel()
  
    testsFailedJPanel.setLayout(new BorderLayout(4, 4))
    testsFailedJPanel.add(testsFailedJLabel, BorderLayout.WEST)
    testsFailedJPanel.add(testsFailedNumJLabel, BorderLayout.CENTER)
    val centeredJPanel: JPanel = new JPanel()
  
    centeredJPanel.setLayout(new GridLayout(1, 3, 15, 15))
    centeredJPanel.add(testsRunJPanel)
    centeredJPanel.add(testsExpectedJPanel)
    centeredJPanel.add(testsFailedJPanel)
    setLayout(new FlowLayout(FlowLayout.CENTER))
    add(centeredJPanel)
  }

  def setTestsRun(testsRun: Int, succeeded: Boolean) {

    if (testsRun < 0) 
      throw new IllegalArgumentException()

    this.testsRun = testsRun

    if (testsRun > testsExpected)
      testsExpected = testsRun

    if (!succeeded)
      testsFailed = testsFailed + 1

    testsRunNumJLabel.setText(Integer.toString(testsRun))
    testsExpectedNumJLabel.setText(Integer.toString(testsExpected))
    testsFailedNumJLabel.setText(Integer.toString(testsFailed))
  }

  def setTestsExpected(testsExpected: Int) {

    if (testsExpected < 0)
      throw new IllegalArgumentException()

    this.testsExpected = testsExpected

    if (testsRun > testsExpected)
      testsRun = testsExpected

    testsRunNumJLabel.setText(Integer.toString(testsRun))
    testsExpectedNumJLabel.setText(Integer.toString(testsExpected))
    testsFailedNumJLabel.setText(Integer.toString(testsFailed))
  }

  def reset() {
    testsExpected = 0
    testsRun = 0
    testsFailed = 0
  }
}
