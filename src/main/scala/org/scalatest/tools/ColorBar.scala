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
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Image
import javax.swing.JPanel

/**
 * ColorBar is used to show the red or green result of a test in the GUI.
 *
 * @author Bill Venners
 */
private[scalatest] class ColorBar extends JPanel {

  private val HANDSOME_GREEN: Color = new Color(0x55, 0xEE, 0x66)
  private val DEEP_RED: Color = new Color(0xEE, 0x55, 0x66)
  private val SENSIBLE_GRAY: Color = new Color(0xEE, 0xEE, 0xEE)
  // These vars should be set only by the event handler thread, so no need for synchronization
  private var max: Int = 0
  private var value: Int = 0
  private var barColor: Color = HANDSOME_GREEN

  // A cache, for performance, so I can reuse the Image object if the dimensions are the same
  private var offscreenImage: Image = _

  setBackground(SENSIBLE_GRAY)

  def setGreen() {
    barColor = HANDSOME_GREEN
    repaint()
  }

  def setRed() {
    barColor = DEEP_RED
    repaint()
  }

  def setGray() {
    barColor = SENSIBLE_GRAY
    repaint()
  }

  def setValue(value: Int) {

    if (value < 0) 
      throw new IllegalArgumentException()

    this.value = value

    if (value > max)
      max = value

    repaint()
  }

  def setMax(max: Int) {

    if (max < 0)
      throw new IllegalArgumentException()

    this.max = max

    if (value > max)
      value = max

    repaint()
  }

  override def update(g: Graphics) {
    paint(g)
  }

  override def paint(g: Graphics) {

    val dim: Dimension = getSize()

    if (offscreenImage == null) {
      offscreenImage = createImage(dim.width, dim.height)
    }
    else {
      val offWidth: Int = offscreenImage.getWidth(null)
      val offHeight: Int = offscreenImage.getHeight(null)
      if (offWidth != dim.width || offHeight != dim.height)
        offscreenImage = createImage(dim.width, dim.height)
    }

    val og: Graphics = offscreenImage.getGraphics()

    og.setColor(SENSIBLE_GRAY)
    og.fillRect(0, 0, dim.width, dim.height)

    val localVal: Int = value
    val localMax: Int = max

    val extent: Int =
      if (localVal >= localMax) {
        dim.width + 1
      }
      else if (localVal != 0) {

        val floatExtent: Float = (dim.width.toFloat * localVal) / localMax
        floatExtent.toInt 
      }
      else 0

    if (max != 0) {
      og.setColor(barColor)
      og.fillRect(0, 0, extent, dim.height + 1)
    }
    g.drawImage(offscreenImage, 0, 0, this)
  }
}

