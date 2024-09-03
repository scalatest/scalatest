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

  val generatorSource = new File("GenAnyVals.scala")

  def genMacro(targetDir: File, primitiveTypeName: String, typeName: String, typeBooleanExpr: String, dotty: Boolean): File = {
    val targetFile = new File(targetDir, typeName + "Macro.scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {

      val macroCode =
        if (dotty)
          s"""
             |  def apply(value: Expr[$primitiveTypeName])(using Quotes): Expr[$typeName] = {
             |    val notValidMsg = Resources.notValid$typeName
             |    val notLiteralMsg = Resources.notLiteral$typeName
             |    import quotes.reflect._
             |    ensureValid${primitiveTypeName}Literal(value, notValidMsg, notLiteralMsg)(isValid)
             |    '{ $typeName.ensuringValid($$value) }
             |  }
             |""".stripMargin
        else
          s"""
             |  def apply(c: Context)(value: c.Expr[$primitiveTypeName]): c.Expr[$typeName] = {
             |    val notValidMsg = Resources.notValid$typeName
             |    val notLiteralMsg = Resources.notLiteral$typeName
             |    import c.universe._
             |    ensureValid${primitiveTypeName}Literal(c)(value, notValidMsg, notLiteralMsg)(isValid)
             |    reify { $typeName.ensuringValid(value.splice) }
             |  }
             |""".stripMargin

      val macroObjectModifier = if (dotty) "" else "private[anyvals]"  // TODO: Hopefully Dotty can allow this in later version.

      val content =
        s"""
           |/*
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
           |${importsForMacro(dotty)}
           |
           |$macroObjectModifier object ${typeName}Macro extends CompileTimeAssertions {
           |
           |  def isValid(i: $primitiveTypeName): Boolean = $typeBooleanExpr
           |
           |  $macroCode
           |}
           |""".stripMargin

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(content)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    targetFile
  }

  def getPrimitiveType(t: String): String =
    if (t.endsWith("Double"))
      "Double"
    else if (t.endsWith("Float"))
      "Float"
    else if (t.endsWith("Long"))
      "Long"
    else
      "Int"

  def genIntAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                   typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                   objectExtraMethods: String, widensToTypes: Seq[String], dotty: Boolean): List[File] = {
    val targetFile = new File(targetDir, typeName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
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
      st.setAttribute("objectExtraMethods", objectExtraMethods)
      st.setAttribute("negation", negation(typeName))

      val widensToOtherAnyVals =
        widensToTypes.map { targetType =>
          val targetPrimitiveType = getPrimitiveType(targetType)
          s"""
             |/**
             | * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
             | *
             | * @param pos the <code>$typeName</code> to widen
             | * @return the <code>Int</code> value underlying the specified <code>$typeName</code>,
             | *     widened to <code>$targetPrimitiveType</code> and wrapped in a <code>$targetType</code>.
             | */
             |implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
             |""".stripMargin
        }.mkString

      st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

      val macroApplyMethod =
        if (dotty)
          s"inline implicit def apply(value: => Int): $typeName = $${ ${typeName}Macro('{value}) }"
        else
          s"implicit def apply(value: Int): $typeName = macro ${typeName}Macro.apply"
      st.setAttribute("macroApplyMethod", macroApplyMethod)

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(st.toString)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile, genMacro(targetDir, "Int", typeName, typeBooleanExpr, dotty))
  }

  def genLongAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                    typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                    objectExtraMethods: String, widensToTypes: Seq[String], dotty: Boolean): List[File] = {
    val targetFile = new File(targetDir, typeName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
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
      st.setAttribute("objectExtraMethods", objectExtraMethods)
      st.setAttribute("negation", negation(typeName))

      val widensToOtherAnyVals =
        widensToTypes.map { targetType =>
          val targetPrimitiveType = getPrimitiveType(targetType)
          s"""
             |/**
             | * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
             | *
             | * @param pos the <code>$typeName</code> to widen
             | * @return the <code>Long</code> value underlying the specified <code>$typeName</code>,
             | *     widened to <code>$targetPrimitiveType</code> and wrapped in a <code>$targetType</code>.
             | */
             |implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
             |""".stripMargin
        }.mkString
      st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

      val macroApplyMethod =
        if (dotty)
          s"inline implicit def apply(value: => Long): $typeName = $${ ${typeName}Macro('{value}) }"
        else
          s"implicit def apply(value: Long): $typeName = macro ${typeName}Macro.apply"
      st.setAttribute("macroApplyMethod", macroApplyMethod)

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(st.toString)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile, genMacro(targetDir, "Long", typeName, typeBooleanExpr, dotty))
  }

  def genFloatAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                     typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                     classExtraMethods: String, objectExtraMethods: String, widensToTypes: Seq[String], dotty: Boolean): List[File] = {
    val targetFile = new File(targetDir, typeName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
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
      st.setAttribute("negation", negation(typeName))

      val widensToOtherAnyVals =
        widensToTypes.map { targetType =>
          val targetPrimitiveType = getPrimitiveType(targetType)
          s"""
             |/**
             | * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
             | *
             | * @param pos the <code>$typeName</code> to widen
             | * @return the <code>Float</code> value underlying the specified <code>$typeName</code>,
             | *     widened to <code>$targetPrimitiveType</code> and wrapped in a <code>$targetType</code>.
             | */
             |implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
             |""".stripMargin
        }.mkString

      st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

      val macroApplyMethod =
        if (dotty)
          s"inline implicit def apply(value: => Float): $typeName = $${ ${typeName}Macro('{value}) }"
        else
          s"implicit def apply(value: Float): $typeName = macro ${typeName}Macro.apply"
      st.setAttribute("macroApplyMethod", macroApplyMethod)

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(st.toString)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile, genMacro(targetDir, "Float", typeName, typeBooleanExpr, dotty))
  }

  def genDoubleAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                      typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                      classExtraMethods: String, objectExtraMethods: String, widensToTypes: Seq[String], dotty: Boolean): List[File] = {
    val targetFile = new File(targetDir, typeName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
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
      st.setAttribute("negation", negation(typeName))

      val widensToOtherAnyVals =
        widensToTypes.map { targetType =>
          val targetPrimitiveType = getPrimitiveType(targetType)
          s"""
             |/**
             | * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
             | *
             | * @param pos the <code>$typeName</code> to widen
             | * @return the <code>Double</code> value underlying the specified <code>$typeName</code>,
             | *     widened to <code>$targetPrimitiveType</code> and wrapped in a <code>$targetType</code>.
             | */
             |implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
             |""".stripMargin
        }.mkString
      st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

      val macroApplyMethod =
        if (dotty)
          s"inline implicit def apply(value: => Double): $typeName = $${ ${typeName}Macro('{value}) }"
        else
          s"implicit def apply(value: Double): $typeName = macro ${typeName}Macro.apply"
      st.setAttribute("macroApplyMethod", macroApplyMethod)

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(st.toString)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile, genMacro(targetDir, "Double", typeName, typeBooleanExpr, dotty))
  }

  def genCharAnyVal(targetDir: File, typeName: String, typeDesc: String, typeNote: String, typeBooleanExpr: String, typeValidExample: String, typeInvalidExample: String,
                   typeValidValue: String, typeInvalidValue: String, typeMinValue: String, typeMinValueNumber: String, typeMaxValue: String, typeMaxValueNumber: String,
                   widensToTypes: Seq[String], dotty: Boolean): List[File] = {
    val targetFile = new File(targetDir, typeName + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val templateSource = scala.io.Source.fromFile("project/templates/CharAnyVal.template")
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
      st.setAttribute("negation", negation(typeName))

      val widensToOtherAnyVals =
        widensToTypes.map { targetType =>
          val targetPrimitiveType = getPrimitiveType(targetType)
          s"""
             |/**
             | * Implicit widening conversion from <code>$typeName</code> to <code>$targetType</code>.
             | *
             | * @param pos the <code>$typeName</code> to widen
             | * @return the <code>Int</code> value underlying the specified <code>$typeName</code>,
             | *     widened to <code>$targetPrimitiveType</code> and wrapped in a <code>$targetType</code>.
             | */
             |implicit def widenTo$targetType(pos: $typeName): $targetType = $targetType.ensuringValid(pos.value)
             |""".stripMargin
        }.mkString
      st.setAttribute("widensToOtherAnyVals", widensToOtherAnyVals)

      val macroApplyMethod =
        if (dotty)
          s"inline implicit def apply(value: => Char): $typeName = $${ ${typeName}Macro('{value}) }"
        else
          s"implicit def apply(value: Char): $typeName = macro ${typeName}Macro.apply"
      st.setAttribute("macroApplyMethod", macroApplyMethod)

      val bw = new BufferedWriter(new FileWriter(targetFile))

      bw.write(st.toString)
      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile, genMacro(targetDir, "Char", typeName, typeBooleanExpr, dotty))
  }

  val primitiveTypes =
    List(
      "Int",
      "Long",
      "Float",
      "Double"
    )

  val anyValTypes =
    List(
      "Pos",
      "PosZ",
      "NonZeroFinite",
      "NonZero",
      "PosFinite",
      "PosZFinite",
      "NegFinite",
      "NegZFinite",
      "Finite"
    )

  def negation(typeName: String): String = {
    val negationType =
      if (typeName.startsWith("Pos"))
        "Neg" + typeName.drop(3)
      else if (typeName.startsWith("Neg")) {
        typeName match {
          case "NegInt" | "NegZInt" => "Int"
          case "NegLong" | "NegZLong" => "Long"
          case _ => "Pos" + typeName.drop(3)
        }
      }
      else
        typeName

    if (primitiveTypes.contains(negationType))
      s"def unary_- : $negationType = -value"
    else
      s"def unary_- : $negationType = $negationType.ensuringValid(-value)"
  }

  val allAnyValTypes =
    anyValTypes.flatMap(t => primitiveTypes.map(p => t + p)).filter( typeName =>
      typeName != "PosFiniteInt" &&
      typeName != "PosFiniteLong" &&
      typeName != "PosZFiniteInt" &&
      typeName != "PosZFiniteLong" &&
      typeName != "NegFiniteInt" &&
      typeName != "NegFiniteLong" &&
      typeName != "NegZFiniteInt" &&
      typeName != "NegZFiniteLong" &&
      typeName != "NonZeroFiniteInt" &&
      typeName != "NonZeroFiniteLong" &&
      typeName != "FiniteInt" &&
      typeName != "FiniteLong"
    )

  def nonZeroWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NonZero" + p)
  }

  def nonZeroFiniteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NonZeroFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Finite" + p)
  }

  def posZWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "PosZ" + p)
  }

  def negZWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NegZ" + p)
  }

  def posWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Pos" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "PosZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p)
  }

  def negWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Neg" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NegZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p)
  }

  def posFiniteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "Pos" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "PosZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "PosFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "PosZFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "Finite" + p)
  }

  def posZFiniteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "PosZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "PosZFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Finite" + p)
  }

  def negFiniteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "Neg" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NegZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NonZero" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NegFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NegZFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "Finite" + p)
  }

  def negZFiniteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).map(p => "NegZ" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "NegZFinite" + p) :::
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Finite" + p)
  }

  def finiteWidens(primitiveType: String): List[String] = {
    primitiveTypes.dropWhile(_ != primitiveType).tail.map(p => "Finite" + p)
  }

  def numericCharWidens: List[String] =
    primitiveTypes.filter(e => e != "Int" && e != "Long").map(p => "Finite" + p) ++
    primitiveTypes.map(p => "Pos" + p) ++
    primitiveTypes.map(p => "PosZ" + p) ++
    primitiveTypes.filter(e => e != "Int" && e != "Long").map(p => "PosFinite" + p) ++
    primitiveTypes.filter(e => e != "Int" && e != "Long").map(p => "PosZFinite" + p)

  def positiveInfinity(typePrefix: String, primitiveName: String): String =
    s"""
       |/**
       | * The positive infinity value, which is <code>$typePrefix$primitiveName.ensuringValid($primitiveName.PositiveInfinity)</code>.
       | */
       |final val PositiveInfinity: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.PositiveInfinity) // Can't use the macro here
       |""".stripMargin

  def negativeInfinity(typePrefix: String, primitiveName: String): String =
    s"""
       |/**
       | * The negative infinity value, which is <code>$typePrefix$primitiveName.ensuringValid($primitiveName.NegativeInfinity)</code>.
       | */
       |final val NegativeInfinity: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.NegativeInfinity) // Can't use the macro here
       |""".stripMargin

  def isPosInfinity(typePrefix: String, primitiveType: String): String =
    s"""
       |/**
       | * True if this <code>$typePrefix$primitiveType</code> value represents positive infinity, else false.
       | */
       |def isPosInfinity: Boolean = $primitiveType.PositiveInfinity == value
       |""".stripMargin

  def isNegInfinity(typePrefix: String, primitiveType: String): String =
    s"""
       |/**
       | * True if this <code>$typePrefix$primitiveType</code> value represents negative infinity, else false.
       | */
       |def isNegInfinity: Boolean = $primitiveType.NegativeInfinity == value
       |""".stripMargin

  def isInfinite(typePrefix: String, primitiveType: String): String =
    s"""
       |/**
       | * True if this <code>$typePrefix$primitiveType</code> value represents positive or negative infinity, else false.
       | */
       |def isInfinite: Boolean = value.isInfinite
       |""".stripMargin

  def isFinite(typePrefix: String, primitiveType: String): String =
    s"""
       |/**
       | * True if this <code>$typePrefix$primitiveType</code> value is any finite value (i.e., it is neither positive nor negative infinity), else false.
       | */
       |def isFinite: Boolean = !value.isInfinite
       |""".stripMargin

  def minPositiveValue(typePrefix: String, primitiveName: String): String = {
    val theValue = if (primitiveName == "Float") "1.4E-45" else "4.9E-324"
    s"""
       |/**
       | * The smallest positive value greater than 0.0d representable as a <code>$typePrefix$primitiveName</code>, which is $typePrefix$primitiveName($theValue).
       | */
       |final val MinPositiveValue: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid($primitiveName.MinPositiveValue)
       |""".stripMargin
  }

  def round(typePrefix: String, primitiveName: String, integralName: String): String = {
    val impl =
      if (integralName == "Int" || integralName == "Long")
        "math.round(value)"
      else
        s"$integralName.ensuringValid(math.round(value))"
    s"""
       |/**
       | * Rounds this `$typePrefix$primitiveName` value to the nearest whole number value that can be expressed as an `$integralName`, returning the result as a `$integralName`.
       | */
       |def round: $integralName = $impl
       |""".stripMargin
  }

  def ceil(typePrefix: String, primitiveName: String): String =
    s"""
       |/**
       | * Returns the smallest (closest to 0) `$typePrefix$primitiveName` that is greater than or equal to this `$typePrefix$primitiveName`
       | * and represents a mathematical integer.
       | */
       |def ceil: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(math.ceil(value).to$primitiveName)
       |""".stripMargin

  def floor(typePrefix: String, primitiveName: String): String =
    s"""
       |/**
       | * Returns the greatest (closest to infinity) `$typePrefix$primitiveName` that is less than or equal to
       | * this `$typePrefix$primitiveName` and represents a mathematical integer.
       | */
       |def floor: $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(math.floor(value).to$primitiveName)
       |""".stripMargin

  def plus(typePrefix: String, primitiveName: String, typeDesc: String): String =
    s"""
       |/**
       | * Returns the <code>$typePrefix$primitiveName</code> sum of this value and `x`.
       | *
       | * <p>
       | * This method will always succeed (not throw an exception) because
       | * adding a $typeDesc $primitiveName to another $typeDesc $primitiveName
       | * will always result in another $typeDesc $primitiveName
       | * value (though the result may be infinity).
       | * </p>
       | */
       |def plus(x: $typePrefix$primitiveName): $typePrefix$primitiveName = $typePrefix$primitiveName.ensuringValid(value + x)
       |""".stripMargin

  def plus(typePrefix: String, primitiveName: String, typeDesc: String, typePrefix2: String, typeDesc2: String): String =
    s"""
       |/**
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
       |""".stripMargin

  def deprecatedOrdering(name: String, typePrefix: String, primitiveName: String): String =
    s"""
       |/**
       | * <strong>The formerly implicit <code>$name</code> field has been deprecated and will be removed in a future version of ScalaTest. Please use the <code>ordering</code> field instead.</strong>
       | */
       |@deprecated("The formerly implicit $name field has been deprecated and will be removed in a future version of ScalaTest. Please use the ordering field instead.")
       |val $name: Ordering[$typePrefix$primitiveName] =
       |  new Ordering[$typePrefix$primitiveName] {
       |    def compare(x: $typePrefix$primitiveName, y: $typePrefix$primitiveName): Int = ordering.compare(x, y)
       |  }
    """.stripMargin

  def importsForMacro(dotty: Boolean): String =
    if (dotty)
      "import scala.quoted._"
    else
      "import scala.reflect.macros.whitebox.Context"


  def genMain(dir: File, version: String, scalaVersion: String, dotty: Boolean): Seq[File] = {
    dir.mkdirs()

    genIntAnyVal(dir, "NonZeroInt", "non-zero", "Note: a <code>NonZeroInt</code> may not equal 0.", "i != 0", "NonZeroInt(42)", "NonZeroInt(0)", "42", "0", "Int.MinValue", "-2147483648",
      "Int.MaxValue", "2147483647", "", nonZeroWidens("Int"), dotty) :::
    genLongAnyVal(dir, "NonZeroLong", "non-zero", "Note: a <code>NonZeroLong</code> may not equal 0.", "i != 0L", "NonZeroLong(42)", "NonZeroLong(0)", "42", "0", "Long.MinValue", "-9223372036854775808",
      "Long.MaxValue", "9223372036854775807", "", nonZeroWidens("Long"), dotty) :::
    genFloatAnyVal(dir, "NonZeroFloat", "non-zero", "Note: a <code>NonZeroFloat</code> may not equal 0.0.", "i != 0.0f && !i.isNaN", "NonZeroFloat(1.1f)", "NonZeroFloat(0.0f)", "1.1", "0.0", "Float.MinValue", "-3.4028235E38",
      "Float.MaxValue", "3.4028235E38",
      isPosInfinity("NonZero", "Float") +
      isNegInfinity("NonZero", "Float") +
      isInfinite("NonZero", "Float") +
      isFinite("NonZero", "Float"),
      positiveInfinity("NonZero", "Float") +
      negativeInfinity("NonZero", "Float") +
      minPositiveValue("NonZero", "Float"),
      nonZeroWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NonZeroDouble", "non-zero", "Note: a <code>NonZeroDouble</code> may not equal 0.0.", "i != 0.0 && !i.isNaN", "NonZeroDouble(1.1)", "NonZeroDouble(0.0)", "1.1", "0.0", "Double.MinValue", "-1.7976931348623157E308",
      "Double.MaxValue", "1.7976931348623157E308",
      isPosInfinity("NonZero", "Double") +
      isNegInfinity("NonZero", "Double") +
      isInfinite("NonZero", "Double") +
      isFinite("NonZero", "Double"),
      positiveInfinity("NonZero", "Double") +
      negativeInfinity("NonZero", "Double") +
      minPositiveValue("NonZero", "Double"),
      nonZeroWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "NonZeroFiniteFloat", "finite non-zero", "Note: a <code>NonZeroFiniteFloat</code> may not equal 0.0.", "i != 0.0f && !i.isNaN && i != Float.PositiveInfinity && i != Float.NegativeInfinity", "NonZeroFiniteFloat(1.1f)", "NonZeroFiniteFloat(0.0f)", "1.1", "0.0", "Float.MinValue", "-3.4028235E38",
      "Float.MaxValue", "3.4028235E38",
      "",
      minPositiveValue("NonZeroFinite", "Float"),
      nonZeroFiniteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NonZeroFiniteDouble", "finite non-zero", "Note: a <code>NonZeroFiniteDouble</code> may not equal 0.0.", "i != 0.0 && !i.isNaN && i != Double.PositiveInfinity && i != Double.NegativeInfinity", "NonZeroDouble(1.1)", "NonZeroDouble(0.0)", "1.1", "0.0", "Double.MinValue", "-1.7976931348623157E308",
      "Double.MaxValue", "1.7976931348623157E308",
      "",
      minPositiveValue("NonZeroFinite", "Double"),
      nonZeroFiniteWidens("Double"), dotty) :::
    genIntAnyVal(dir, "PosZInt", "non-negative", "", "i >= 0", "PosZInt(42)", "PosZInt(-1)", "42", "-1", "0", "0",
      "Int.MaxValue", "2147483647", deprecatedOrdering("posZIntOrd", "PosZ", "Int"), posZWidens("Int"), dotty) :::
    genLongAnyVal(dir, "PosZLong", "non-negative", "", "i >= 0L", "PosZLong(42)", "PosZLong(-1)", "42", "-1", "0L", "0L",
      "Long.MaxValue", "9223372036854775807", deprecatedOrdering("posZLongOrd", "PosZ", "Long"), posZWidens("Long"), dotty) :::
    genFloatAnyVal(dir, "PosZFloat", "non-negative", "", "i >= 0.0f", "PosZFloat(1.1f)", "PosZFloat(-1.0f)", "1.1f", "-1.1f", "0.0f", "0.0f",
      "Float.MaxValue", "3.4028235E38",
      round("PosZ", "Float", "PosZInt") +
      ceil("PosZ", "Float") +
      floor("PosZ", "Float") +
      plus("PosZ", "Float", "non-negative") +
      isPosInfinity("PosZ", "Float") +
      isFinite("PosZ", "Float"),
      positiveInfinity("PosZ", "Float") +
      minPositiveValue("PosZ", "Float") +
      deprecatedOrdering("posZFloatOrd", "PosZ", "Float"),
      posZWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "PosZDouble", "non-negative", "", "i >= 0.0", "PosZDouble(1.1)", "PosZDouble(-1.1)", "1.1", "-1.1", "0.0", "0.0",
      "Double.MaxValue", "1.7976931348623157E308",
      round("PosZ", "Double", "PosZLong") +
      ceil("PosZ", "Double") +
      floor("PosZ", "Double") +
      plus("PosZ", "Double", "non-negative") +
      isPosInfinity("PosZ", "Double") +
      isFinite("PosZ", "Double"),
      positiveInfinity("PosZ", "Double") +
      minPositiveValue("PosZ", "Double") +
      deprecatedOrdering("posZDoubleOrd", "PosZ", "Double"),
      posZWidens("Double"), dotty) :::
    genIntAnyVal(dir, "PosInt", "positive", "Note: a <code>PosInt</code> may not equal 0. If you want positive number or 0, use [[PosZInt]].", "i > 0", "PosInt(42)", "PosInt(0)", "42", "0", "1", "1",
      "Int.MaxValue", "2147483647", deprecatedOrdering("posIntOrd", "Pos", "Int"), posWidens("Int"), dotty) :::
    genLongAnyVal(dir, "PosLong", "positive", "Note: a <code>PosLong</code> may not equal 0. If you want positive number or 0, use [[PosZLong]].", "i > 0L", "PosLong(42L)", "PosLong(0L)", "42L", "0L", "1L", "1L",
      "Long.MaxValue", "9223372036854775807", deprecatedOrdering("posLongOrd", "Pos", "Long"), posWidens("Long"), dotty) :::
    genFloatAnyVal(dir, "PosFloat", "positive", "Note: a <code>PosFloat</code> may not equal 0.0. If you want positive number or 0, use [[PosZFloat]].", "i > 0.0f", "PosFloat(42.1f)", "PosFloat(0.0f)", "42.1f", "0.0f", "Float.MinPositiveValue", "1.4E-45",
      "Float.MaxValue", "3.4028235E38",
      round("Pos", "Float", "PosZInt") +
      ceil("Pos", "Float") +
      floor("PosZ", "Float") +
      plus("Pos", "Float", "positive", "PosZ", "non-negative") +
      isPosInfinity("Pos", "Float") +
      isFinite("Pos", "Float"),
      positiveInfinity("Pos", "Float") +
      minPositiveValue("Pos", "Float") +
      deprecatedOrdering("posFloatOrd", "Pos", "Float"),
      posWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "PosDouble", "positive", "", "i > 0.0", "PosDouble(1.1)", "PosDouble(-1.1)", "1.1", "-1.1", "Double.MinPositiveValue", "4.9E-324",
      "Double.MaxValue", "1.7976931348623157E308",
      round("Pos", "Double", "PosZLong") +
      ceil("Pos", "Double") +
      floor("PosZ", "Double") +
      plus("Pos", "Double", "positive", "PosZ", "non-negative") +
      isPosInfinity("Pos", "Double") +
      isFinite("Pos", "Double"),
      positiveInfinity("Pos", "Double") +
      minPositiveValue("Pos", "Double") +
      deprecatedOrdering("posDoubleOrd", "Pos", "Double"),
      posWidens("Double"), dotty) :::
    genIntAnyVal(dir, "NegInt", "negative", "Note: a <code>NegInt</code> may not equal 0. If you want negative number or 0, use [[NegZInt]].", "i < 0", "NegInt(-42)", "NegInt(0)", "-42", "0", "Int.MinValue", "-2147483648", "-1", "-1",
      "", negWidens("Int"), dotty) :::
    genLongAnyVal(dir, "NegLong", "negative", "Note: a <code>NegLong</code> may not equal 0. If you want negative number or 0, use [[NegZLong]].", "i < 0L", "NegLong(-42L)", "NegLong(0L)", "-42L", "0L", "Long.MinValue", "-9223372036854775808", "-1L", "-1L",
      "", negWidens("Long"), dotty) :::
    genFloatAnyVal(dir, "NegFloat", "megative", "Note: a <code>NegFloat</code> may not equal 0.0. If you want negative number or 0, use [[NegZFloat]].", "i < 0.0f", "NegFloat(-42.1f)", "NegFloat(0.0f)", "-42.1f", "0.0f", "Float.MinValue", "-3.4028235E38",
      "-Float.MinPositiveValue", "-1.4E-45",
      round("Neg", "Float", "NegZInt") +
      ceil("NegZ", "Float") +
      floor("Neg", "Float") +
      plus("Neg", "Float", "negative", "NegZ", "non-positive") +
      isNegInfinity("Neg", "Float") +
      isFinite("Neg", "Float"),
      negativeInfinity("Neg", "Float"),
      negWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NegDouble", "negative", "", "i < 0.0", "NegDouble(-1.1)", "NegDouble(1.1)", "-1.1", "1.1", "Double.MinValue", "-1.7976931348623157E308",
      "-Double.MinPositiveValue", "-4.9E-324",
      round("Neg", "Double", "NegZLong") +
      ceil("NegZ", "Double") +
      floor("Neg", "Double") +
      plus("Neg", "Double", "negative", "NegZ", "non-positive") +
      isNegInfinity("Neg", "Double") +
      isFinite("Neg", "Double"),
      negativeInfinity("Neg", "Double"),
      negWidens("Double"), dotty) :::
    genIntAnyVal(dir, "NegZInt", "non-positive", "", "i <= 0", "NegZInt(-42)", "NegZInt(1)", "-42", "1", "Int.MinValue", "-2147483648",
      "0", "0", "", negZWidens("Int"), dotty) :::
    genLongAnyVal(dir, "NegZLong", "non-positive", "", "i <= 0L", "NegZLong(-42L)", "NegZLong(-1L)", "-42", "1", "Long.MinValue", "-9223372036854775808",
      "0L", "0L", "", negZWidens("Long"), dotty) :::
    genFloatAnyVal(dir, "NegZFloat", "non-positive", "", "i <= 0.0f", "NegZFloat(-1.1f)", "NegZFloat(1.0f)", "-1.1f", "1.1f", "Float.MinValue", "-3.4028235E38", "0.0f", "0.0f",
      round("NegZ", "Float", "NegZInt") +
      ceil("NegZ", "Float") +
      floor("NegZ", "Float") +
      plus("NegZ", "Float", "non-positive") +
      isNegInfinity("NegZ", "Float") +
      isFinite("NegZ", "Float"),
      negativeInfinity("NegZ", "Float"),
      negZWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NegZDouble", "non-positive", "", "i <= 0.0", "NegZDouble(-1.1)", "NegZDouble(1.1)", "-1.1", "1.1", "Double.MinValue", "-1.7976931348623157E308", "0.0", "0.0",
      round("NegZ", "Double", "NegZLong") +
      ceil("NegZ", "Double") +
      floor("NegZ", "Double") +
      plus("NegZ", "Double", "non-positive") +
      isNegInfinity("NegZ", "Double") +
      isFinite("NegZ", "Double"),
      negativeInfinity("NegZ", "Double"),
      negZWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "PosFiniteFloat", "finite positive", "Note: a <code>PosFiniteFloat</code> may not equal 0.0. If you want positive number or 0, use [[PosZFiniteFloat]].", "i > 0.0f && i != Float.PositiveInfinity", "PosFiniteFloat(42.1f)", "PosFiniteFloat(0.0f)", "42.1f", "0.0f", "Float.MinPositiveValue", "1.4E-45",
      "Float.MaxValue", "3.4028235E38",
      round("PosFinite", "Float", "PosZInt") +
      ceil("PosFinite", "Float") +
      floor("PosZFinite", "Float"),
      minPositiveValue("Pos", "Float"),
      posFiniteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "PosFiniteDouble", "finite positive", "", "i > 0.0  && i != Double.PositiveInfinity", "PosFiniteDouble(1.1)", "PosFiniteDouble(-1.1)", "1.1", "-1.1", "Double.MinPositiveValue", "4.9E-324",
      "Double.MaxValue", "1.7976931348623157E308",
      round("PosFinite", "Double", "PosZLong") +
      ceil("PosFinite", "Double") +
      floor("PosZFinite", "Double"),
      minPositiveValue("PosFinite", "Double"),
      posFiniteWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "PosZFiniteFloat", "finite non-negative", "", "i >= 0.0f && i != Float.PositiveInfinity", "PosZFiniteFloat(1.1f)", "PosZFiniteFloat(-1.0f)", "1.1f", "-1.1f", "0.0f", "0.0f",
      "Float.MaxValue", "3.4028235E38",
      round("PosZFinite", "Float", "PosZInt") +
      ceil("PosZFinite", "Float") +
      floor("PosZFinite", "Float"),
      minPositiveValue("PosZFinite", "Float"),
      posZFiniteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "PosZFiniteDouble", "finite non-negative", "", "i >= 0.0 && i != Double.PositiveInfinity", "PosZFiniteDouble(1.1)", "PosZFiniteDouble(-1.1)", "1.1", "-1.1", "0.0", "0.0",
      "Double.MaxValue", "1.7976931348623157E308",
      round("PosZFinite", "Double", "PosZLong") +
      ceil("PosZFinite", "Double") +
      floor("PosZFinite", "Double"),
      minPositiveValue("PosZFinite", "Double"),
      posZFiniteWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "NegFiniteFloat", "finite negative", "Note: a <code>NegFiniteFloat</code> may not equal 0.0. If you want negative number or 0, use [[NegZFiniteFloat]].", "i < 0.0f && i != Float.NegativeInfinity", "NegFiniteFloat(-42.1f)", "NegFiniteFloat(0.0f)", "-42.1f", "0.0f",
      "Float.MinValue", "-3.4028235E38", "-Float.MinPositiveValue", "-1.4E-45",
      round("NegFinite", "Float", "NegZInt") +
      ceil("NegZFinite", "Float") +
      floor("NegFinite", "Float"),
      "",
      negFiniteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NegFiniteDouble", "finite negative", "", "i < 0.0  && i != Double.NegativeInfinity", "NegFiniteDouble(-1.1)", "NegFiniteDouble(1.1)", "-1.1", "1.1", "Double.MinValue", "-1.7976931348623157E308", "-Double.MinPositiveValue", "-4.9E-324",
      round("NegFinite", "Double", "NegZLong") +
      ceil("NegZFinite", "Double") +
      floor("NegFinite", "Double"),
      "",
      negFiniteWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "NegZFiniteFloat", "finite non-positive", "", "i <= 0.0f && i != Float.NegativeInfinity", "NegZFiniteFloat(-1.1f)", "NegZFiniteFloat(1.0f)", "-1.1f", "1.1f", "Float.MinValue", "-3.4028235E38", "0.0f", "0.0f",
      round("NegZFinite", "Float", "NegZInt") +
      ceil("NegZFinite", "Float") +
      floor("NegZFinite", "Float"),
      "",
      negZFiniteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "NegZFiniteDouble", "finite non-positive", "", "i <= 0.0 && i != Double.NegativeInfinity", "PosZFiniteDouble(-1.1)", "NegZFiniteDouble(1.1)", "-1.1", "1.1", "Double.MinValue", "-1.7976931348623157E308", "0.0", "0.0",
      round("NegZFinite", "Double", "NegZLong") +
      ceil("NegZFinite", "Double") +
      floor("NegZFinite", "Double"),
      "",
      negZFiniteWidens("Double"), dotty) :::
    genFloatAnyVal(dir, "FiniteFloat", "finite", "", "i != Float.NegativeInfinity && i != Float.PositiveInfinity && !i.isNaN", "FiniteFloat(42.1f)", "FiniteFloat(Float.PositiveInfinity)", "42.1f", "Float.PositiveInfinity", "Float.MinValue", "-3.4028235E38",
      "Float.MaxValue", "3.4028235E38",
      round("Finite", "Float", "Int") +
      ceil("Finite", "Float") +
      floor("Finite", "Float"),
      minPositiveValue("Finite", "Float"),
      finiteWidens("Float"), dotty) :::
    genDoubleAnyVal(dir, "FiniteDouble", "finite", "", "i != Double.NegativeInfinity && i != Double.PositiveInfinity && !i.isNaN", "FiniteDouble(1.1)", "FiniteDouble(FiniteDouble.PositiveInfinity)", "1.1", "Finite.PositiveInfinity", "Double.MinValue", "-1.7976931348623157E308", "Double.MaxValue", "1.7976931348623157E308",
      round("Finite", "Double", "Long") +
      ceil("Finite", "Double") +
      floor("Finite", "Double"),
      minPositiveValue("Finite", "Double"),
      finiteWidens("Double"), dotty) :::
    genCharAnyVal(dir, "NumericChar", "numeric", "Note: a <code>NumericChar</code> has a value between '0' and '9'.", "i >= '0' && i <= '9'", "NumericChar('4')", "NumericChar('a')", "'4'", "'a'", "'0'", "'0'",
      "'9'", "'9'", numericCharWidens, dotty)

  }

  def valueFormat(value: String, typeName: String): String =
    if (typeName.endsWith("Long")) {
      if (value.indexOf('.') >= 0)
        value.substring(0, value.indexOf('.')) + "L"
      else
        s"${value}L"
    }
    else if (typeName.endsWith("Float")) {
      if (value.indexOf('.') >= 0)
        s"${value}f"
      else
        s"${value}.0f"
    }
    else if (typeName.endsWith("Double")) {
      if (value.indexOf('.') >= 0)
        s"${value}"
      else
        s"${value}.0"
    }
    else if (typeName.endsWith("Char")) {
      s"'${value}'"
    }
    else {
      if (value.indexOf('.') >= 0)
        value.substring(0, value.indexOf('.'))
      else
        s"${value}"
    }

  def primitivesShouldEqualTests(typeName: String, types: Seq[String], lhsFun: String => String, resultValue: String): String =
    types.map { pType =>
      val widerValue = pickWiderType(typeName, pType)
      val expectedValue =
        if (typeName.endsWith("Char") && !pType.endsWith("Char"))
          valueFormat(resultValue.charAt(0).toInt.toString, widerValue)
        else
          valueFormat(resultValue, widerValue)
      lhsFun(pType) + " shouldEqual " + expectedValue
    }.mkString("\n")

  def primitivesWidenPropertyTests(typeName: String, primitiveType: String, targetTypes: Seq[String]): String =
    targetTypes.map { targetType =>
      s"""forAll { (p: $typeName) =>
         |  def widen(value: $targetType): $targetType = value
         |  widen(p) shouldEqual widen(p.to$primitiveType)
         |}
       """.stripMargin
    }.mkString("\n")

  def anyvalsWidenPropertyTests(typeName: String, primitiveType: String, targetTypes: Seq[String]): String =
    targetTypes.map { targetType =>
      s"""forAll { (p: $typeName) =>
         |  def widen(value: $targetType): $targetType = value
         |  widen(p) shouldEqual widen($targetType.from(p.to$primitiveType).get)
         |}
       """.stripMargin
    }.mkString("\n")

  def shouldNotCompileTests(types: Seq[String], lhsFun: String => String): String =
    types.map { t =>
      "\"" + lhsFun(t) + "\" shouldNot compile"
    }.mkString("\n")

  def anyValsWidenShouldEqualTests(typeName: String, widensToTypes: Seq[String], validValue: String): String =
    (widensToTypes map { widenType =>
      //val expectedValue = valueFormat(validValue, widenType)
      val expectedValue =
        if (typeName.endsWith("Char") && !widenType.endsWith("Char"))
          valueFormat(validValue.charAt(0).toInt.toString, widenType)
        else
          valueFormat(validValue, widenType)
      val lhsValue = valueFormat(validValue, typeName)
      s"($typeName($lhsValue): $widenType) shouldEqual $widenType($expectedValue)"
    }).mkString("\n")

  def typeWidth(t: String): Int =
    if (t.endsWith("Double"))
      4
    else if (t.endsWith("Float"))
      3
    else if (t.endsWith("Long"))
      2
    else
      1

  def pickWiderType(t1: String, t2: String): String = {
    val t1Width = typeWidth(t1)
    val t2Width = typeWidth(t2)
    if (t2Width > t1Width)
      t2
    else
      t1
  }

  val typeOperatorValues =
    List(
      ("PosZ", (3, 2, 2, 3, 3)),
      ("Pos", (3, 2, 2, 3, 3)),
      ("NonZero", (3, 2, 2, 3, 3)),
      ("Neg", (-3, -2, -2, -3, -3)),
      ("Finite", (3, 2, 2, 3, 3)),
      ("NumericChar", (3, 2, 2, 3, 3))
    )

  def getModifyValue(typeName: String, operator: String): Int =
    typeOperatorValues.find(row => typeName.startsWith(row._1)) match {
      case Some((_, (add, minus, multiply, divide, modulus))) =>
        operator match {
          case "+" => add
          case "-" => minus
          case "*" => multiply
          case "/" => divide
          case "%" => modulus
          case _ => throw new IllegalArgumentException("Unsupported operator: " + operator)
        }

      case None => throw new IllegalArgumentException("Cannot find modifyValue for " + typeName + ", operator: " + operator)
    }

  def getResultValue(primitiveType: String, lhsValue: Int, operator: String, rhsValue: Int): Int = {
    val leftValue =
      if (primitiveType == "Char")
        lhsValue.toString.charAt(0).toInt
      else
        lhsValue
    operator match {
      case "+" => leftValue + rhsValue
      case "-" => leftValue - rhsValue
      case "*" => leftValue * rhsValue
      case "/" => leftValue / rhsValue
      case "%" => leftValue % rhsValue
    }
  }

  def operatorShouldEqualTests(typeName: String, primitiveType: String, lhsValue: Int, operator: String): String = {
    val primitiveModifyValue = getModifyValue(typeName, operator)
    primitiveTypes.map { pType =>
      val widerType = pickWiderType(typeName, pType)
      typeName + "(" + valueFormat(lhsValue.toString, typeName) + ") " + operator + " " + valueFormat(primitiveModifyValue.toString, pType) + " shouldEqual " + valueFormat(getResultValue(primitiveType, lhsValue, operator, primitiveModifyValue).toString, pType)
    }.mkString("\n") + "\n" +
    allAnyValTypes.map { rhsType =>
      val widerType = pickWiderType(typeName, rhsType)
      val modifyValue = getModifyValue(rhsType, operator)
      typeName + "(" + valueFormat(lhsValue.toString, typeName) + ") " + operator + " " + rhsType + "(" + valueFormat(modifyValue.toString, rhsType) + ") shouldEqual " + valueFormat(getResultValue(primitiveType, lhsValue, operator, modifyValue).toString, rhsType)
    }.mkString("\n")
  }

  def genAnyValTests(targetDir: File, typeName: String, primitiveType: String, validValue: Int, widensToTypes: Seq[String]): List[File] = {
    val targetFile = new File(targetDir, typeName + "GeneratedSpec.scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val bw = new BufferedWriter(new FileWriter(targetFile))

      val autoWidenTests =
        primitivesShouldEqualTests(typeName, primitiveTypes.dropWhile(_ != primitiveType), pType => "(" + typeName + "(" + validValue + "): " + pType + ")", validValue.toString) + "\n" +
          anyValsWidenShouldEqualTests(typeName, widensToTypes, validValue.toString) + "\n" +
          shouldNotCompileTests(primitiveTypes.takeWhile(_ != primitiveType) ++ allAnyValTypes.filter(t => !widensToTypes.contains(t) && t != typeName), pType => "(" + typeName + "(" + validValue + "): " + pType + ")")

      val autoWidenPropertyTests =
        primitivesWidenPropertyTests(typeName, primitiveType, primitiveTypes.dropWhile(_ != primitiveType)) ++
          anyvalsWidenPropertyTests(typeName, primitiveType, widensToTypes)

      val additionTests = operatorShouldEqualTests(typeName, primitiveType, validValue, "+")
      val minusTests = operatorShouldEqualTests(typeName, primitiveType, validValue, "-")
      val multiplyTests = operatorShouldEqualTests(typeName, primitiveType, validValue, "*")
      val divideTests = operatorShouldEqualTests(typeName, primitiveType, validValue, "/")
      val modulusTests = operatorShouldEqualTests(typeName, primitiveType, validValue, "%")

      val templateSource = scala.io.Source.fromFile("project/templates/GeneratedSpec.template")
      val templateText = try templateSource.mkString finally templateSource.close()
      val st = new org.antlr.stringtemplate.StringTemplate(templateText)

      st.setAttribute("typeName", typeName)
      st.setAttribute("autoWidenTests", autoWidenTests)
      st.setAttribute("autoWidenPropertyTests", autoWidenPropertyTests)
      st.setAttribute("additionTests", additionTests)
      st.setAttribute("minusTests", minusTests)
      st.setAttribute("multiplyTests", multiplyTests)
      st.setAttribute("divideTests", divideTests)
      st.setAttribute("modulusTests", modulusTests)
      st.setAttribute("formattedValidValue", valueFormat(validValue.toString, primitiveType))
      st.setAttribute("validValue", validValue.toString)
      st.setAttribute("primitiveType", primitiveType)

      bw.write(
        st.toString
      )

      bw.flush()
      bw.close()
      println("Generated: " + targetFile.getAbsolutePath)
    }
    List(targetFile)
  }

  def genTest(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()
    genAnyValTests(dir, "PosInt", "Int", 3, posWidens("Int")) ++
    genAnyValTests(dir, "PosLong", "Long", 3, posWidens("Long")) ++
    genAnyValTests(dir, "PosFloat", "Float", 3, posWidens("Float")) ++
    genAnyValTests(dir, "PosDouble", "Double", 3, posWidens("Double")) ++
    genAnyValTests(dir, "PosZInt", "Int", 3, posZWidens("Int")) ++
    genAnyValTests(dir, "PosZLong", "Long", 3, posZWidens("Long")) ++
    genAnyValTests(dir, "PosZFloat", "Float", 3, posZWidens("Float")) ++
    genAnyValTests(dir, "PosZDouble", "Double", 3, posZWidens("Double")) ++
    genAnyValTests(dir, "NonZeroInt", "Int", 3, nonZeroWidens("Int")) ++
    genAnyValTests(dir, "NonZeroLong", "Long", 3, nonZeroWidens("Long")) ++
    genAnyValTests(dir, "NonZeroFloat", "Float", 3, nonZeroWidens("Float")) ++
    genAnyValTests(dir, "NonZeroDouble", "Double", 3, nonZeroWidens("Double")) ++
    genAnyValTests(dir, "NonZeroFiniteFloat", "Float", 3, nonZeroFiniteWidens("Float")) ++"NegZ"
    genAnyValTests(dir, "NonZeroFiniteDouble", "Double", 3, nonZeroFiniteWidens("Double")) ++
    genAnyValTests(dir, "NegInt", "Int", -3, negWidens("Int")) ++
    genAnyValTests(dir, "NegLong", "Long", -3, negWidens("Long")) ++
    genAnyValTests(dir, "NegFloat", "Float", -3, negWidens("Float")) ++
    genAnyValTests(dir, "NegDouble", "Double", -3, negWidens("Double")) ++
    genAnyValTests(dir, "NegZInt", "Int", -3, negZWidens("Int")) ++
    genAnyValTests(dir, "NegZLong", "Long", -3, negZWidens("Long")) ++
    genAnyValTests(dir, "NegZFloat", "Float", -3, negZWidens("Float")) ++
    genAnyValTests(dir, "NegZDouble", "Double", -3, negZWidens("Double")) ++
    genAnyValTests(dir, "PosFiniteFloat", "Float", 3, posFiniteWidens("Float")) ++
    genAnyValTests(dir, "PosFiniteDouble", "Double", 3, posFiniteWidens("Double")) ++
    genAnyValTests(dir, "PosZFiniteFloat", "Float", 3, posZFiniteWidens("Float")) ++
    genAnyValTests(dir, "PosZFiniteDouble", "Double", 3, posZFiniteWidens("Double")) ++
    genAnyValTests(dir, "NegFiniteFloat", "Float", -3, negFiniteWidens("Float")) ++
    genAnyValTests(dir, "NegFiniteDouble", "Double", -3, negFiniteWidens("Double")) ++
    genAnyValTests(dir, "NegZFiniteFloat", "Float", -3, negZFiniteWidens("Float")) ++
    genAnyValTests(dir, "NegZFiniteDouble", "Double", -3, negZFiniteWidens("Double")) ++
    genAnyValTests(dir, "FiniteFloat", "Float", 3, finiteWidens("Float")) ++
    genAnyValTests(dir, "FiniteDouble", "Double", 3, finiteWidens("Double")) ++
    genAnyValTests(dir, "NumericChar", "Char", 3, numericCharWidens)
  }
}
