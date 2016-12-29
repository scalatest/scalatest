/*
 * Copyright 2001-2011 Artima, Inc.
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

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object GenAnyVals {

  def genMacro(targetDir: File, primitiveTypeName: String, typeName: String, typeBooleanExpr: String): File = {
    val content =
      s"""/*
         | * Copyright 2001-2016 Artima, Inc.
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
         |package org.scalactic.anyvals
         |
        |import org.scalactic.Resources
         |import reflect.macros.Context
         |
        |private[anyvals] object ${typeName}Macro extends CompileTimeAssertions {
         |
        |  def isValid(i: $primitiveTypeName): Boolean = $typeBooleanExpr
         |
        |  def apply(c: Context)(value: c.Expr[$primitiveTypeName]): c.Expr[$typeName] = {
         |    val notValidMsg = Resources.notValid$typeName
         |    val notLiteralMsg = Resources.notLiteral$typeName
         |
        |    import c.universe._
         |
        |    ensureValid${primitiveTypeName}Literal(c)(value, notValidMsg, notLiteralMsg)(isValid)
         |    reify { $typeName.ensuringValid(value.splice) }
         |  }
         |}
      """.stripMargin

    val targetFile = new File(targetDir, typeName + "Macro.scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(content)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    targetFile
  }

  def genIntAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                   typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                   widensToTypes: Seq[String]): List[File] = {
    val templateSource = scala.io.Source.fromFile("project/templates/IntAnyVal.template")
    val templateText = try templateSource.mkString finally templateSource.close()
    val st = new org.antlr.stringtemplate.StringTemplate(templateText)

    st.setAttribute("typeName", typeName)
    st.setAttribute("typeDesc", typeDesc)
    st.setAttribute("typeNote", typeNote)
    st.setAttribute("typeBooleanExpr", typeBooleanExpr)
    st.setAttribute("typeValidExample", typeValidExample)
    st.setAttribute("typeInvalidExample", typeInvalidExample)
    st.setAttribute("typeValidValue", typeValidValue)
    st.setAttribute("typeInvalidValue", typeInvalidValue)
    st.setAttribute("typeMinValue", typeMinValue)
    st.setAttribute("typeMinValueNumber", typeMinValueNumber)
    st.setAttribute("typeMaxValue", typeMaxValue)
    st.setAttribute("typeMaxValueNumber", typeMaxValueNumber)

    val widensToOtherAnyVals =
      widensToTypes.map { targetType =>
        s"""/**
           |   * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
           |   *
           |   * @param pos the <code>$typeName</code> to widen
           |   * @return the <code>$targetType</code> widen from <code>$typeName</code>.
           |   */
           |  implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
           |
        """.stripMargin
      }.mkString

    st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

    val targetFile = new File(targetDir, typeName + ".scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(st.toString)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    List(targetFile, genMacro(targetDir, "Int", typeName, typeBooleanExpr))
  }

  def genLongAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                    typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                    widensToTypes: Seq[String]): List[File] = {
    val templateSource = scala.io.Source.fromFile("project/templates/LongAnyVal.template")
    val templateText = try templateSource.mkString finally templateSource.close()
    val st = new org.antlr.stringtemplate.StringTemplate(templateText)

    st.setAttribute("typeName", typeName)
    st.setAttribute("typeDesc", typeDesc)
    st.setAttribute("typeNote", typeNote)
    st.setAttribute("typeBooleanExpr", typeBooleanExpr)
    st.setAttribute("typeValidExample", typeValidExample)
    st.setAttribute("typeInvalidExample", typeInvalidExample)
    st.setAttribute("typeValidValue", typeValidValue)
    st.setAttribute("typeInvalidValue", typeInvalidValue)
    st.setAttribute("typeMinValue", typeMinValue)
    st.setAttribute("typeMinValueNumber", typeMinValueNumber)
    st.setAttribute("typeMaxValue", typeMaxValue)
    st.setAttribute("typeMaxValueNumber", typeMaxValueNumber)

    val widensToOtherAnyVals =
      widensToTypes.map { targetType =>
        s"""/**
           |   * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
           |   *
           |   * @param pos the <code>$typeName</code> to widen
           |   * @return the <code>$targetType</code> widen from <code>$typeName</code>.
           |   */
           |  implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
           |
        """.stripMargin
      }.mkString

    st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

    val targetFile = new File(targetDir, typeName + ".scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(st.toString)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    List(targetFile, genMacro(targetDir, "Long", typeName, typeBooleanExpr))
  }

  def genFloatAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                     typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                     classExtraMethods: String, objectExtraMethods: String, widensToTypes: Seq[String]): List[File] = {
    val templateSource = scala.io.Source.fromFile("project/templates/FloatAnyVal.template")
    val templateText = try templateSource.mkString finally templateSource.close()
    val st = new org.antlr.stringtemplate.StringTemplate(templateText)

    st.setAttribute("typeName", typeName)
    st.setAttribute("typeDesc", typeDesc)
    st.setAttribute("typeNote", typeNote)
    st.setAttribute("typeBooleanExpr", typeBooleanExpr)
    st.setAttribute("typeValidExample", typeValidExample)
    st.setAttribute("typeInvalidExample", typeInvalidExample)
    st.setAttribute("typeValidValue", typeValidValue)
    st.setAttribute("typeInvalidValue", typeInvalidValue)
    st.setAttribute("typeMinValue", typeMinValue)
    st.setAttribute("typeMinValueNumber", typeMinValueNumber)
    st.setAttribute("typeMaxValue", typeMaxValue)
    st.setAttribute("typeMaxValueNumber", typeMaxValueNumber)
    st.setAttribute("classExtraMethods", classExtraMethods)
    st.setAttribute("objectExtraMethods", objectExtraMethods)

    val widensToOtherAnyVals =
      widensToTypes.map { targetType =>
        s"""/**
           |   * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
           |   *
           |   * @param pos the <code>$typeName</code> to widen
           |   * @return the <code>$targetType</code> widen from <code>$typeName</code>.
           |   */
           |  implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
           |
        """.stripMargin
      }.mkString

    st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

    val targetFile = new File(targetDir, typeName + ".scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(st.toString)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    List(targetFile, genMacro(targetDir, "Float", typeName, typeBooleanExpr))
  }

  def genDoubleAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                      typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                      classExtraMethods: String, objectExtraMethods: String, widensToTypes: Seq[String]): List[File] = {
    val templateSource = scala.io.Source.fromFile("project/templates/DoubleAnyVal.template")
    val templateText = try templateSource.mkString finally templateSource.close()
    val st = new org.antlr.stringtemplate.StringTemplate(templateText)

    st.setAttribute("typeName", typeName)
    st.setAttribute("typeDesc", typeDesc)
    st.setAttribute("typeNote", typeNote)
    st.setAttribute("typeBooleanExpr", typeBooleanExpr)
    st.setAttribute("typeValidExample", typeValidExample)
    st.setAttribute("typeInvalidExample", typeInvalidExample)
    st.setAttribute("typeValidValue", typeValidValue)
    st.setAttribute("typeInvalidValue", typeInvalidValue)
    st.setAttribute("typeMinValue", typeMinValue)
    st.setAttribute("typeMinValueNumber", typeMinValueNumber)
    st.setAttribute("typeMaxValue", typeMaxValue)
    st.setAttribute("typeMaxValueNumber", typeMaxValueNumber)
    st.setAttribute("classExtraMethods", classExtraMethods)
    st.setAttribute("objectExtraMethods", objectExtraMethods)

    val widensToOtherAnyVals =
      widensToTypes.map { targetType =>
        s"""/**
           |   * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
           |   *
           |   * @param pos the <code>$typeName</code> to widen
           |   * @return the <code>$targetType</code> widen from <code>$typeName</code>.
           |   */
           |  implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
           |
        """.stripMargin
      }.mkString

    st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

    val targetFile = new File(targetDir, typeName + ".scala")
    val bw = new BufferedWriter(new FileWriter(targetFile))

    bw.write(st.toString)
    bw.flush()
    bw.close()
    println("Generated: " + targetFile.getAbsolutePath)
    List(targetFile, genMacro(targetDir, "Double", typeName, typeBooleanExpr))
  }

  /*private val wideOrderList =
    List(
      ("NonZero", (n: String) => n),
      ("Pos", ""),

    )*/

  val primitiveTypes =
    List(
      "Int",
      "Long",
      "Float",
      "Double"
    )

  val anyValTypes =
    List(
      "NonZero"
    )

  def nonZeroWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NonZero" + p)
  }

  def posZWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "PosZ" + p)
  }

  def genMain(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()

    genIntAnyVal(dir, "NonZeroInt", "non-zero", "Note: a <code>NonZeroInt</code> may not equal 0.", "i != 0", "NonZeroInt(42)", "NonZeroInt(0)", "42", "0", "Int.MinValue", "-2147483648",
      "Int.MaxValue", "2147483647", nonZeroWidens("Int")) :::
    genLongAnyVal(dir, "NonZeroLong", "non-zero", "Note: a <code>NonZeroLong</code> may not equal 0.", "i != 0L", "NonZeroLong(42)", "NonZeroLong(0)", "42", "0", "Long.MinValue", "-9223372036854775808",
      "Long.MaxValue", "9223372036854775807", nonZeroWidens("Long")) :::
    genFloatAnyVal(dir, "NonZeroFloat", "non-zero", "Note: a <code>NonZeroFloat</code> may not equal 0.0.", "i != 0.0f && !i.isNaN", "NonZeroFloat(1.1f)", "NonZeroFloat(0.0f)", "1.1", "0.0", "Float.MinValue", "-3.4028235E38",
      "Float.MaxValue", "3.4028235E38",
      "",
      """/**
        |  * The positive infinity value, which is <code>NonZeroFloat.ensuringValid(Float.PositiveInfinity)</code>.
        |  */
        |final val PositiveInfinity: NonZeroFloat = NonZeroFloat.ensuringValid(Float.PositiveInfinity) // Can't use the macro here
        |
        |/**
        |  * The negative infinity value, which is <code>NonZeroFloat.ensuringValid(Float.NegativeInfinity)</code>.
        |  */
        |final val NegativeInfinity: NonZeroFloat = NonZeroFloat.ensuringValid(Float.NegativeInfinity) // Can't use the macro here
        |
        |final val MinPositiveValue: NonZeroFloat = NonZeroFloat.ensuringValid(Float.MinPositiveValue)
      """.stripMargin,
      nonZeroWidens("Float")) :::
    genDoubleAnyVal(dir, "NonZeroDouble", "non-zero", "Note: a <code>NonZeroDouble</code> may not equal 0.0.", "i != 0.0 && !i.isNaN", "NonZeroDouble(1.1)", "NonZeroDouble(0.0)", "1.1", "0.0", "Double.MinValue", "-1.7976931348623157E308",
      "Double.MaxValue", "1.7976931348623157E308",
      "",
      """/**
        |  * The positive infinity value, which is <code>NonZeroDouble.ensuringValid(Double.PositiveInfinity)</code>.
        |  */
        |final val PositiveInfinity: NonZeroDouble = NonZeroDouble.ensuringValid(Double.PositiveInfinity) // Can't use the macro here
        |
        |/**
        |  * The negative infinity value, which is <code>NonZeroFloat.ensuringValid(Double.NegativeInfinity)</code>.
        |  */
        |final val NegativeInfinity: NonZeroDouble = NonZeroDouble.ensuringValid(Double.NegativeInfinity) // Can't use the macro here
        |
        |final val MinPositiveValue: NonZeroDouble = NonZeroDouble.ensuringValid(Double.MinPositiveValue)
      """.stripMargin,
      nonZeroWidens("Double")) :::
    genIntAnyVal(dir, "PosZInt", "non-negative", "", "i >= 0", "PosZInt(42)", "PosZInt(-1)", "42", "-1", "0", "0",
      "Int.MaxValue", "2147483647", posZWidens("Int"))
  }

}
