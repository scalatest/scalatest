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
import javax.swing.JDialog
import javax.swing.JFrame
import javax.swing.SwingConstants
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.border.EmptyBorder
import java.util.ResourceBundle
import java.awt.GridLayout
import java.awt.FlowLayout
import java.awt.BorderLayout
import java.awt.Font
import java.awt.Color

/**
 * The About dialog box.
 *
 * @author Bill Venners
 */
private[scalatest] class AboutJDialog(owner: JFrame, title: String) extends JDialog(owner, title) {

  val name = Resources("AppName")
  val copyright = Resources("AppCopyright")
  val moreInfo = Resources("MoreInfo")
  val url = Resources("AppURL")
  val version = Resources("AppVersion")
  val scalaVersion = Resources("ScalaVersion")
  val reason = Resources("Reason")
  val trademarks = Resources("Trademarks")
  val company = Resources("ArtimaInc")
  val nameLabel = new JLabel(name, SwingConstants.CENTER)
  val copyrightLabel = new JLabel(copyright, SwingConstants.CENTER)
  val urlLabel = new JLabel(url, SwingConstants.CENTER)
  val moreInfoLabel = new JLabel(moreInfo, SwingConstants.CENTER)
  val versionLabel = new JLabel(version + " (Built for Scala " + scalaVersion + ")", SwingConstants.CENTER)
  val reasonLabel = new JLabel(reason, SwingConstants.CENTER)
  val trademarksLabel = new JLabel(trademarks, SwingConstants.CENTER)
  val companyLabel = new JLabel(company, SwingConstants.CENTER)

  nameLabel.setFont(new Font("SansSerif", Font.BOLD, 20))
  urlLabel.setFont(new Font("SansSerif", Font.PLAIN, 13))
  reasonLabel.setFont(new Font("SansSerif", Font.ITALIC, 13))
  companyLabel.setFont(new Font("SansSerif", Font.BOLD, 14))
  trademarksLabel.setFont(new Font("SansSerif", Font.PLAIN, 11))
  copyrightLabel.setFont(new Font("SansSerif", Font.PLAIN, 11))
  nameLabel.setForeground(new Color(0x00, 0x00, 0x66))
  versionLabel.setForeground(new Color(0x00, 0x00, 0x66))
  companyLabel.setForeground(new Color(0x00, 0x00, 0x66))

  val northJPanel = new JPanel
  northJPanel.setLayout(new BorderLayout())
  northJPanel.add(nameLabel, BorderLayout.NORTH)
  northJPanel.add(versionLabel, BorderLayout.SOUTH)

  val centerBottomJPanel = new JPanel
  centerBottomJPanel.setLayout(new GridLayout(2, 1))
  centerBottomJPanel.add(moreInfoLabel)
  centerBottomJPanel.add(urlLabel)

  val centerJPanel = new JPanel
  centerJPanel.setLayout(new GridLayout(2, 1, 5, 5))
  centerJPanel.add(reasonLabel)
  centerJPanel.add(centerBottomJPanel)

  val copyrightTrademarkJPanel = new JPanel
  copyrightTrademarkJPanel.setLayout(new GridLayout(2, 1))
  copyrightTrademarkJPanel.add(copyrightLabel)
  copyrightTrademarkJPanel.add(trademarksLabel)

  val aboutJPanel = new JPanel
  aboutJPanel.setLayout(new BorderLayout(15, 15))
  aboutJPanel.add(northJPanel, BorderLayout.NORTH)
  aboutJPanel.add(centerJPanel, BorderLayout.CENTER)
  aboutJPanel.add(copyrightTrademarkJPanel, BorderLayout.SOUTH)
  aboutJPanel.setBorder(new EmptyBorder(5, 5, 5, 5))
  getContentPane().setLayout(new BorderLayout())
  getContentPane().add("Center", aboutJPanel)
  pack()
}
