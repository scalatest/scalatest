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

  def posWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Pos" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "PosZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p)
  }

  def positiveInfinity(typePrefix: String, primitiveName: String): String =
    s"""/**
      |  * The positive infinity value, which is <code>$typePrefix$primitiveName.ensuringValid($primitiveName.PositiveInfinity)</code>.
      |  */
      |final val PositiveInfinity: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.PositiveInfinity) // Can't use the macro here
      |
    """.stripMargin

  def negativeInfinity(typePrefix: String, primitiveName: String): String =
    s"""/**
        |  * The negative infinity value, which is <code>$typePrefix$primitiveName.ensuringValid($primitiveName.NegativeInfinity)</code>.
        |  */
        |final val NegativeInfinity: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.NegativeInfinity) // Can't use the macro here
        |
     """.stripMargin

  def minPositiveValue(typePrefix: String, primitiveName: String): String =
    s"final val MinPositiveValue: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.MinPositiveValue)".stripMargin

  def round(typePrefix: String, primitiveName: String): String =
    s"""/**
      |  * Rounds this `$typePrefix$primitiveName` value to the nearest whole number value that can be expressed as an `$primitiveName`, returning the result as a `$typePrefix$primitiveName`.
      |  */
      |def round: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(math.round(value))""".stripMargin

  def ceil(typePrefix: String, primitiveName: String): String =
    s"""/**
        |  * Returns the smallest (closest to 0) `$typePrefix$primitiveName` that is greater than or equal to this `$typePrefix$primitiveName`
        |  * and represents a mathematical integer.
        |  */
        |def ceil: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(math.ceil(value).to$primitiveName)
        |
     """.stripMargin

  def sumOf(typePrefix: String, primitiveName: String, typeDesc: String): String =
    s"""/**
        |  * Returns the <code>$typePrefix$primitiveName</code> sum of the passed <code>$typePrefix$primitiveName</code> values `x` and `y`.
        |  *
        |  * <p>
        |  * This method will always succeed (not throw an exception) because
        |  * adding a $typeDesc $primitiveName to another $typeDesc $primitiveName
        |  * will always result in another $typePrefix$primitiveName $primitiveName
        |  * value (though the result may be infinity).
        |  * </p>
        |  *
        |  * <p>
        |  * This overloaded form of the method is used when there are just two arguments so that
        |  * boxing is avoided. The overloaded <code>sumOf</code> that takes a varargs of
        |  * <code>$typePrefix$primitiveName</code> starting at the third parameter can sum more than two
        |  * values, but will entail boxing and may therefore be less efficient.
        |  * </p>
        |  */
        |def sumOf(x: $typePrefix$primitiveName, y: $typePrefix$primitiveName): $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(x.value + y.value)
        |
        |/**
        |  * Returns the <code>$typePrefix$primitiveName</code> sum of the passed <code>$typePrefix$primitiveName</code> values `first` and
        |  * value `second`, and the <code>$typePrefix$primitiveName</code> values passed as varargs `rest`.
        |  *
        |  * <p>
        |  * This method will always succeed (not throw an exception) because
        |  * adding a $typeDesc $primitiveName to another $typeDesc $primitiveName
        |  * will always result in another $typeDesc $primitiveName
        |  * value (though the result may be infinity).
        |  * </p>
        |  *
        |  * <p>
        |  * This overloaded form of the <code>sumOf</code> method can sum more than two
        |  * values, but unlike its two-arg sibling, will entail boxing.
        |  * </p>
        |  */
        |def sumOf(first: $typePrefix$primitiveName, second: $typePrefix$primitiveName, rest: $typePrefix$primitiveName*): $typePrefix$primitiveName =
        |  $typePrefix$primitiveName.ensuringValid(first.value + second.value + rest.map(_.value).sum)
        |
     """.stripMargin

  def sumOf(typePrefix: String, primitiveName: String, typeDesc: String, typePrefix2: String, typeDesc2: String): String =
    s"""/**
        | * Returns the <code>$typePrefix$primitiveName</code> sum of the passed <code>$typePrefix$primitiveName</code> value `x` and <code>$typePrefix2$primitiveName</code> value `y`.
        | *
        | * <p>
        | * This method will always succeed (not throw an exception) because
        | * adding a $typeDesc $primitiveName and $typeDesc2 $primitiveName will
        | * always result in another $typeDesc $primitiveName
        | * value (though the result may be infinity).
        | * </p>
        | *
        | * <p>
        | * This overloaded form of the method is used when there are just two arguments so that
        | * boxing is avoided. The overloaded <code>sumOf</code> that takes a varargs of
        | * <code>$typePrefix2$primitiveName</code> starting at the third parameter can sum more than two
        | * values, but will entail boxing and may therefore be less efficient.
        | * </p>
        | */
        |def sumOf(x: $typePrefix$primitiveName, y: $typePrefix2$primitiveName): $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(x.value + y.value)
        |
        |/**
        |  * Returns the <code>$typePrefix$primitiveName</code> sum of the passed <code>$typePrefix$primitiveName</code> value `first`, the <code>$typePrefix2$primitiveName</code>
        |  * value `second`, and the <code>$typePrefix2$primitiveName</code> values passed as varargs `rest`.
        |  *
        |  * <p>
        |  * This method will always succeed (not throw an exception) because
        |  * adding a $typeDesc $primitiveName and one or more $typeDesc2 ${primitiveName}s
        |  * will always result in another $typeDesc $primitiveName
        |  * value (though the result may be infinity).
        |  * </p>
        |  *
        |  * <p>
        |  * This overloaded form of the <code>sumOf</code> method can sum more than two
        |  * values, but unlike its two-arg sibling, will entail boxing.
        |  * </p>
        |  */
        |def sumOf(first: $typePrefix$primitiveName, second: $typePrefix2$primitiveName, rest: $typePrefix2$primitiveName*): $typePrefix$primitiveName =
        |  $typePrefix$primitiveName.ensuringValid(first.value + second.value + rest.map(_.value).sum)
        |
     """.stripMargin

  def floor(typePrefix: String, primitiveName: String): String =
    s"""/**
       |  * Returns the greatest (closest to infinity) `$typePrefix$primitiveName` that is less than or equal to
       |  * this `$typePrefix$primitiveName` and represents a mathematical integer.
       |  */
       |def floor: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(math.floor(value).to$primitiveName)
       |
     """.stripMargin

  def plus(typePrefix: String, primitiveName: String, typeDesc: String): String =
    s"""/**
        |  * Returns the <code>$typePrefix$primitiveName</code> sum of this value and `x`.
        |  *
        |  * <p>
        |  * This method will always succeed (not throw an exception) because
        |  * adding a $typeDesc $primitiveName to another $typeDesc $primitiveName
        |  * will always result in another $typeDesc $primitiveName
        |  * value (though the result may be infinity).
        |  * </p>
        |  */
        |def plus(x: $typePrefix$primitiveName): $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(value + x)
        |
     """.stripMargin

  def plus(typePrefix: String, primitiveName: String, typeDesc: String, typePrefix2: String, typeDesc2: String): String =
    s"""/**
        | * Returns the <code>$typePrefix$primitiveName</code> sum of this <code>$typePrefix$primitiveName</code>'s value and the given <code>$typePrefix2$primitiveName</code> value.
        | *
        | * <p>
        | * This method will always succeed (not throw an exception) because
        | * adding a $typeDesc $primitiveName and $typeDesc2 $primitiveName and another
        | * $typeDesc $primitiveName will always result in another $typeDesc $primitiveName
        | * value (though the result may be infinity).
        | * </p>
        | */
        |def plus(x: $typePrefix2$primitiveName): $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(value + x.value)
        |
     """.stripMargin

  def genMain(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()

    genIntAnyVal(dir, "NonZeroInt", "non-zero", "Note: a <code>NonZeroInt</code> may not equal 0.", "i != 0", "NonZeroInt(42)", "NonZeroInt(0)", "42", "0", "Int.MinValue", "-2147483648",
      "Int.MaxValue", "2147483647", nonZeroWidens("Int")) :::
    genLongAnyVal(dir, "NonZeroLong", "non-zero", "Note: a <code>NonZeroLong</code> may not equal 0.", "i != 0L", "NonZeroLong(42)", "NonZeroLong(0)", "42", "0", "Long.MinValue", "-9223372036854775808",
      "Long.MaxValue", "9223372036854775807", nonZeroWidens("Long")) :::
    genFloatAnyVal(dir, "NonZeroFloat", "non-zero", "Note: a <code>NonZeroFloat</code> may not equal 0.0.", "i != 0.0f && !i.isNaN", "NonZeroFloat(1.1f)", "NonZeroFloat(0.0f)", "1.1", "0.0", "Float.MinValue", "-3.4028235E38",
      "Float.MaxValue", "3.4028235E38",
      "",
      positiveInfinity("NonZero", "Float") +
      negativeInfinity("NonZero", "Float") +
      minPositiveValue("NonZero", "Float"),
      nonZeroWidens("Float")) :::
    genDoubleAnyVal(dir, "NonZeroDouble", "non-zero", "Note: a <code>NonZeroDouble</code> may not equal 0.0.", "i != 0.0 && !i.isNaN", "NonZeroDouble(1.1)", "NonZeroDouble(0.0)", "1.1", "0.0", "Double.MinValue", "-1.7976931348623157E308",
      "Double.MaxValue", "1.7976931348623157E308",
      "",
      positiveInfinity("NonZero", "Double") +
      negativeInfinity("NonZero", "Double") +
      minPositiveValue("NonZero", "Double"),
      nonZeroWidens("Double")) :::
    genIntAnyVal(dir, "PosZInt", "non-negative", "", "i >= 0", "PosZInt(42)", "PosZInt(-1)", "42", "-1", "0", "0",
      "Int.MaxValue", "2147483647", posZWidens("Int")) :::
    genLongAnyVal(dir, "PosZLong", "non-negative", "", "i >= 0L", "PosZLong(42)", "PosZLong(-1)", "42", "-1", "0L", "0L",
      "Long.MaxValue", "9223372036854775807", posZWidens("Long")) :::
    genFloatAnyVal(dir, "PosZFloat", "non-negative", "", "i >= 0.0f", "PosZFloat(1.1f)", "PosZFloat(-1.0f)", "1.1f", "-1.1f", "0.0f", "0.0f",
      "Float.MaxValue", "3.4028235E38",
      round("PosZ", "Float") +
      ceil("PosZ", "Float") +
      floor("PosZ", "Float") +
      plus("PosZ", "Float", "non-negative"),
      positiveInfinity("PosZ", "Float") +
      minPositiveValue("PosZ", "Float") +
      sumOf("PosZ", "Float", "non-negative"),
      posZWidens("Float")) :::
    genDoubleAnyVal(dir, "PosZDouble", "non-negative", "", "i >= 0.0", "PosZDouble(1.1)", "PosZDouble(-1.1)", "1.1", "-1.1", "0.0", "0.0",
      "Double.MaxValue", "1.7976931348623157E308",
      round("PosZ", "Double") +
      ceil("PosZ", "Double") +
      floor("PosZ", "Double") +
      plus("PosZ", "Double", "non-negative"),
      positiveInfinity("PosZ", "Double") +
      minPositiveValue("PosZ", "Double") +
      sumOf("PosZ", "Double", "non-negative"),
      posZWidens("Double")) :::
    genIntAnyVal(dir, "PosInt", "positive", "Note: a <code>PosInt</code> may not equal 0. If you want positive number or 0, use [[PosZInt]].", "i > 0", "PosInt(42)", "PosInt(0)", "42", "0", "1", "1",
      "Int.MaxValue", "2147483647", posWidens("Int")) :::
    genLongAnyVal(dir, "PosLong", "positive", "Note: a <code>PosLong</code> may not equal 0. If you want positive number or 0, use [[PosZLong]].", "i > 0L", "PosLong(42L)", "PosLong(0L)", "42L", "0L", "1L", "1L",
      "Long.MaxValue", "9223372036854775807", posWidens("Long")) :::
    genFloatAnyVal(dir, "PosFloat", "positive", "Note: a <code>PostFloat</code> may not equal 0.0. If you want positive number or 0, use [[PosZFloat]].", "i > 0.0f", "PosFloat(42.1f)", "PosFloat(0.0f)", "42.1f", "0.0f", "Float.MinPositiveValue", "1.4E-45",
      "Float.MaxValue", "3.4028235E38",
      round("Pos", "Float") +
      ceil("Pos", "Float") +
      floor("Pos", "Float") +
      plus("Pos", "Float", "positive", "PosZ", "non-negative"),
      positiveInfinity("Pos", "Float") +
      minPositiveValue("Pos", "Float") +
      sumOf("Pos", "Float", "positive", "PosZ", "non-negative"),
      posWidens("Float")) :::
    genDoubleAnyVal(dir, "PosDouble", "positive", "", "i > 0.0", "PosDouble(1.1)", "PosDouble(-1.1)", "1.1", "-1.1", "Double.MinPositiveValue", "4.9E-324",
      "Double.MaxValue", "1.7976931348623157E308",
      round("Pos", "Double") +
      ceil("Pos", "Double") +
      floor("Pos", "Double") +
      plus("Pos", "Double", "positive", "PosZ", "non-negative"),
      positiveInfinity("Pos", "Double") +
      minPositiveValue("Pos", "Double") +
      sumOf("Pos", "Double", "positive", "PosZ", "non-negative"),
      posWidens("Double"))
  }

}
