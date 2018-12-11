import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.io.Source

object GenNumberCompatHelper {

  private def writeFile(targetFile: File, content: String, scalaVersion: String): File = {
    targetFile.getParentFile.mkdirs()
    val destWriter = new BufferedWriter(new FileWriter(targetFile))
    try {
      destWriter.write(content)
      targetFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Generated " + targetFile.getAbsolutePath)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val content =
      if (scalaVersion startsWith "2.13")
        """/*
          | * Copyright 2001-2018 Artima, Inc.
          | *
          | * Licensed under the Apache License, Version 2.0 (the "License");
          | * you may not use this file except in compliance with the License.
          | * You may obtain a copy of the License at
          | *
          | *     http://www.apache.org/licenses/LICENSE-2.0
          | *
          | * Unless required by applicable law or agreed to in writing, software
          | * distributed under the License is distributed on an "AS IS" BASIS,
          | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          | * See the License for the specific language governing permissions and
          | * limitations under the License.
          | */
          |package org.scalactic
          |
          |import scala.collection.immutable.NumericRange
          |
          |private[scalactic] object NumberCompatHelper {
          |
          |  def doubleUntil(value: Double, end: Double) = BigDecimal(value).until(end)
          |
          |  def doubleUntil(value: Double, end: Double, step: Double) = BigDecimal(value).until(end, step)
          |
          |  def doubleTo(value: Double, end: Double) = BigDecimal(value).to(end)
          |
          |  def doubleTo(value: Double, end: Double, step: Double) = BigDecimal(value).to(end, step)
          |
          |  def floatUntil(value: Float, end: Float) = BigDecimal(value).until(end)
          |
          |  def floatUntil(value: Float, end: Float, step: Float) = BigDecimal(value).until(end, step)
          |
          |  def floatTo(value: Float, end: Float) = BigDecimal(value).to(end)
          |
          |  def floatTo(value: Float, end: Float, step: Float) = BigDecimal(value).to(end, step)
          |
          |}
        """.stripMargin
      else
        """/*
          | * Copyright 2001-2018 Artima, Inc.
          | *
          | * Licensed under the Apache License, Version 2.0 (the "License");
          | * you may not use this file except in compliance with the License.
          | * You may obtain a copy of the License at
          | *
          | *     http://www.apache.org/licenses/LICENSE-2.0
          | *
          | * Unless required by applicable law or agreed to in writing, software
          | * distributed under the License is distributed on an "AS IS" BASIS,
          | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          | * See the License for the specific language governing permissions and
          | * limitations under the License.
          | */
          |package org.scalactic
          |
          |import scala.collection.immutable.NumericRange
          |
          |private[scalactic] object NumberCompatHelper {
          |
          |  def doubleUntil(value: Double, end: Double): Range.Partial[Double, NumericRange[Double]] = value.until(end)
          |
          |  def doubleUntil(value: Double, end: Double, step: Double): NumericRange.Exclusive[Double] = value.until(end, step)
          |
          |  def doubleTo(value: Double, end: Double): Range.Partial[Double, NumericRange[Double]] = value.to(end)
          |
          |  def doubleTo(value: Double, end: Double, step: Double): NumericRange.Inclusive[Double] = value.to(end, step)
          |
          |  def floatUntil(value: Float, end: Float): Range.Partial[Float, NumericRange[Float]] = value.until(end)
          |
          |  def floatUntil(value: Float, end: Float, step: Float): NumericRange.Exclusive[Float] = value.until(end, step)
          |
          |  def floatTo(value: Float, end: Float): Range.Partial[Float, NumericRange[Float]] = value.to(end)
          |
          |  def floatTo(value: Float, end: Float, step: Float): NumericRange.Inclusive[Float] = value.to(end, step)
          |
          |}
        """.stripMargin
    Seq(
      writeFile(new File(targetDir,"NumberCompatHelper.scala"), content, scalaVersion)
    )
  }

}