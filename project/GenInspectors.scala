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

import java.io.{File, FileWriter, BufferedWriter}
import collection.Iterable
import scala.annotation.tailrec

object GenInspectors {

  val generatorSource = new File("GenInspectors.scala")
  
  import Generator._

  trait ErrorMessageTemplate extends Template {
    val header: String
    val xsName: String = "xs"
    override protected def childrenContent =
      children.map(_.toString.split("\n").map("  " + _).mkString("\n")).mkString(", \\n\" + \n") + " \\n\" + \n"
    override def toString =
      header +
        childrenContent +
        "in \" + decorateToStringValue(prettifier, " + xsName + ")"
  }

  class ErrorDetailTemplate(indexOrKey: String, fileName: String, lineNumber: String, messageTemplate: Template) extends Template {
    val at: String = "index"
    override def toString =
      "at " + at + " " + indexOrKey + ", " + messageTemplate + " (" + fileName + ":\" + " + lineNumber + " + \")"
  }

  // Templates
  class IndexesTemplate(indexes: List[Int]) extends Template {
    override def toString =
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")
  }

  class ForAllErrMsgTemplate(headerFailedPrefix: String, detail: Template) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because: \\n\" + " + "\n"
    override val children = List(detail)
  }

  class ForAtLeastErrMsgTemplate(headerFailedPrefix: String, elementText: String, details: List[Template]) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block: \\n\" + " + "\n"
    override val children = details
  }

  class ForAtMostErrMsgTemplate(headerFailedPrefix: String, max: Int, elementText: String, okFun: String, errorFun: String, errorValue: String, colType: String) extends Template {
    val xsName: String = "xs"
    val maxSucceed = max + 1
    def extractXsName =
      if (xsName.startsWith("\"Array(")) {
        val elements = xsName.substring(1, xsName.length - 2).substring(6)
        if (colType == "String")
          "Array(" + elements.split(", ").map(e => if (e != "null") "\"" + e + "\"" else e).mkString(", ") + ")"
        else
          "Array(" + elements + ")"
      }
      else if (xsName.startsWith("arrayToString(")) {
        val elements = xsName.substring(14, xsName.length - 1)
        if (colType == "String")
          elements.split(", ").map(e => if (e != "null") "\"" + e + "\"" else e).mkString(", ")
        else
          elements
      }
      else
        xsName
    override def toString =
      headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block at \" + failEarlySucceededIndexes" + getErrorMessageValuesFunName(colType, okFun) + "(" + extractXsName + ", " + errorValue + ", " + maxSucceed + ") + \" in \" + decorateToStringValue(prettifier, " + xsName + ")"
  }

  class ForExactlyErrMsgTemplate(headerFailedPrefix: String, elementText: String, okFun: String, errorFun: String, errorValue: String, colType: String, details: List[Template]) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block" + (if (elementText == "no element") "" else " at \" + succeededIndexes" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + ") + \"") + ": \\n\" + " + "\n"
    override val children = details
  }

  class ForNoErrMsgTemplate(headerFailedPrefix: String, indexOrKey: String, useIndex: Boolean) extends Template {
    val xsName: String = "xs"
    override def toString =
      headerFailedPrefix + " failed, because 1 element satisfied the assertion block at " + (if (useIndex) "index" else "key") + " " + indexOrKey + " in \" + decorateToStringValue(prettifier, " + xsName + ")"
  }

  class ForBetweenLessErrMsgTemplate(headerFailedPrefix: String, elementText: String, okFun: String, errorFun: String, errorValue: String, colType: String, details: List[Template]) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block" + (if (elementText == "no element") "" else " at \" + succeededIndexes" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + ") + \"") + ": \\n\" + " + "\n"
    override val children = details
  }

  class ForBetweenMoreErrMsgTemplate(headerFailedPrefix: String, elementText: String, indexesTemplate: IndexesTemplate) extends Template {
    val xsName: String = "xs"
    override def toString =
      headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block at " + indexesTemplate + " in \" + " + xsName
  }

  class ForEveryErrMsgTemplate(headerFailedPrefix: String, details: List[Template]) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because: \\n\" + \n"
    override val children = details
  }
  
  class NestedSucceedTemplate(forType: String, forText: String, name: String, text: String, assertText: String) extends Template {
    override def toString = 
      "def `" + forType + " and " + name + " should nest without problem` {\n" + 
      "  " + forText + " { l =>\n" + 
      "    " + text + 
      "      " + assertText + "\n" + 
      "    }\n" +
      "  }\n" +
      "}\n"
  }
  
  class NestedFailedTemplate(colText: Iterable[_], forType: String, forText: String, name: String, text: String, assertText: String, messageTemplate: Template) extends Template {
    override def toString = 
      "def `" + forType + " and " + name + " when nest should throw TestFailedException with correct stack depth and error message when failed` {\n" + 
      "  val xs = " + colText + "\n" +
      "  val e = intercept[exceptions.TestFailedException] {\n" + 
      "    " + forText + " { l =>\n" + 
      "      " + text + 
      "        " + assertText + "\n" +
      "      }\n" + 
      "    }\n" + 
      "  }\n" + 
      "  assert(e.failedCodeFileName == Some(\"NestedInspectorsSpec.scala\"), e.failedCodeFileName + \" did not equal \" + Some(\"NestedInspectorsSpec.scala\"))\n" + 
      "  assert(e.failedCodeLineNumber == Some(thisLineNumber - 7), e.failedCodeLineNumber + \" did not equal \" + Some(thisLineNumber - 7))\n" + 
      "  val outerForLineNumber = thisLineNumber - 8\n" + 
      "  val innerForLineNumber = thisLineNumber - 8\n" + 
      "  val assertLineNumber = thisLineNumber - 8\n" + 
      "  assert(e.message == Some(\n" + messageTemplate.toString.split("\n").map("\"" + _).mkString("\n") + "), e.message + \" did not equal \" + Some(\n" + messageTemplate.toString.split("\n").map("\"" + _).mkString("\n") + "))\n" + 
      "}\n"
  }

  def getMessageTemplate(name: String, index: Int, xs: List[_], fileName: String, lineNumber: String, detailTemplate: Template, forNested: Boolean = false): Template = {
    name match {
      case "forAll" =>
        new ForAllErrMsgTemplate("forAll", new ErrorDetailTemplate("0", fileName, lineNumber, detailTemplate)) {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forAtLeast" =>
        val details =
          for (x <- 0 until xs.length) yield {
            new ErrorDetailTemplate(x + "", fileName, lineNumber, detailTemplate)
          }
        new ForAtLeastErrMsgTemplate("forAtLeast(3)", "no element", details.toList) {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forAtMost" =>
        new ForAtMostErrMsgTemplate("forAtMost(3)", 3, xs.length + " elements", "NotEqualBoolean", "EqualBoolean", "false", if (forNested) "Int" else "List[Int]") {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forExactly" =>
        val details =
          for (x <- 0 until xs.length) yield {
            new ErrorDetailTemplate(x + "", fileName, lineNumber, detailTemplate)
          }
        new ForExactlyErrMsgTemplate("forExactly(4)", "no element", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]", details.toList) {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forNo" =>
        new ForNoErrMsgTemplate("forNo", "0", true) {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forBetween" =>
        val details =
          for (x <- 0 until xs.length) yield {
            new ErrorDetailTemplate(x + "", fileName, lineNumber, detailTemplate)
          }
        new ForBetweenLessErrMsgTemplate("forBetween(2, 4)", "no element", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]", details.toList) {
          override val xsName: String = "xs(" + index + ")"
        }

      case "forEvery" =>
        val details =
          for (x <- 0 until xs.length) yield {
            new ErrorDetailTemplate(x + "", fileName, lineNumber, detailTemplate)
          }
        new ForEveryErrMsgTemplate("forEvery", details.toList) {
          override val xsName: String = "xs(" + index + ")"
        }
    }
  }
  
  def getNestedMessageTemplate(outerName: String, innerName: String, full: Boolean, xs: List[List[_]], fileName: String) = {
    val errorMessage = new SimpleMessageTemplate("0 did not equal 1")
    val assertLineNumber = "assertLineNumber"
    val innerLineNumber = "innerForLineNumber"
    val innerTemplates =
    if (full)
      xs.zipWithIndex map { case (l, i) =>
        getMessageTemplate(innerName, i, l, fileName, assertLineNumber, errorMessage, true)
      }
    else
      List(getMessageTemplate(innerName, 0, xs(0), fileName, assertLineNumber, errorMessage, true))
    val innerDetails = innerTemplates.zipWithIndex.map { case (template, index) => 
      new ErrorDetailTemplate(index + "", fileName, innerLineNumber, new SimpleMessageTemplate(template.toString + " + \""))
    }
    outerName match {
      case "forAll" => new ForAllErrMsgTemplate("forAll", innerDetails(0)) // should have at least one element
      case "forAtLeast" => new ForAtLeastErrMsgTemplate("forAtLeast(3)", "no element", innerDetails.toList)
      case "forAtMost" => new ForAtMostErrMsgTemplate("forAtMost(1)", 1, "2 elements", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]")
      case "forExactly" => new ForExactlyErrMsgTemplate("forExactly(1)", "no element", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]", innerDetails.toList)
      case "forNo" => new ForNoErrMsgTemplate("forNo", "0", true)
      case "forBetween" => new ForBetweenLessErrMsgTemplate("forBetween(2, 4)", "no element", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]", innerDetails.toList)
      case "forEvery" => new ForEveryErrMsgTemplate("forEvery", innerDetails.toList)
    }
  }

  val collectionTypes =
    List(
      ("List", "List(1, 2, 3)", "List(1, 2, 3, 4, 5)", "List.empty[Int]", "e"),
      ("Set", "Set(1, 2, 3)", "Set(1, 2, 3, 4, 5)", "Set.empty[Int]", "e"),
      ("String", "\"123\"", "\"12345\"", "\"\"", "e.toString.toInt"),
      ("Map", "Map(1 -> \"one\", 2 -> \"two\", 3 -> \"three\")", "Map(1 -> \"one\", 2 -> \"two\", 3 -> \"three\", 4 -> \"four\", 5 -> \"five\")", "Map.empty[Int, String]", "e._1"),
      ("Java List", "javaList(1, 2, 3)", "javaList(1, 2, 3, 4, 5)", "javaList[Int]()", "e"),
      ("Java Set", "javaSet(1, 2, 3)", "javaSet(1, 2, 3, 4, 5)", "javaSet[Int]()", "e"),
      ("Java Map", "javaMap(Entry(1, \"one\"), Entry(2, \"two\"), Entry(3, \"three\"))", "javaMap(Entry(1, \"one\"), Entry(2, \"two\"), Entry(3, \"three\"), Entry(4, \"four\"), Entry(5, \"five\"))", "javaMap[Int, String]()", "e.getKey")
    )

  class DefTemplate(name: String, body: Template) extends Template {
    override def toString: String =
      "def `" + name + "` {\n" +
      body.toString.split("\n").map("  " + _).mkString("\n") + "\n" +
      "}"
  }

  class ItTemplate(name: String, body: Template) extends Template {
    override def toString: String =
      "it(\"" + name + "\") {\n" +
        body.toString.split("\n").map("  " + _).mkString("\n") + "\n" +
      "}"
  }

  def isMap(colName: String): Boolean = colName.contains("Map")

  def getIndexForType(colName: String, e: Any): String =
    colName match {
      case "Map" => "\"key " + e.toString + "\""
      case "Java Map" => "\"key " + e.toString + "\""
      case "String" => "\"index \" + getIndex(col, '" + e.toString + "')"
      case _ => "\"index \" + getIndex(col, " + e.toString + ")"
    }

  def getVariableIndexForType(colName: String, variable: String): String =
    colName match {
      case "Map" => "\"key \" + " + variable + "._1"
      case "Java Map" => "\"key \" + " + variable + ".getKey"
      case "String" => "\"index \" + getIndex(col, " + variable + ")"
      case _ => "\"index \" + getIndex(col, " + variable + ")"
    }

  def getIndexOrKeyWord(colName: String): String =
    colName match {
      case "Map" => "key"
      case "Java Map" => "key"
      case "String" => "index"
      case _ => "index"
    }

  def getIndexOrKey(colName: String, variable: String): String =
    colName match {
      case "Map" => variable + "._1"
      case "Java Map" => variable + ".getKey"
      case "String" => "getIndex(col, " + variable + ")"
      case _ => "getIndex(col, " + variable + ")"
    }

  def getLhs(colName: String, variableName: String): String =
    colName match {
      case "Map" => variableName + "._1"
      case "Java Map" => variableName + ".getKey"
      case "String" => variableName + ".toString.toInt"
      case _ => variableName
    }

  def getElementType(colName: String): String =
    colName match {
      case "Map" => "[(Int, String)]"
      case "Java Map" => "[Int, String]"
      case "String" => ""
      case _ => "[Int]"
    }

  def getFirst(colName: String): String =
    colName match {
      case "Java List" => "getFirstInJavaCol"
      case "Java Set" => "getFirstInJavaCol"
      case "Java Map" => "getFirstInJavaMap"
      case "String" => "getFirstInString"
      case _ => "getFirst"
    }

  def getNext(colName: String): String =
    colName match {
      case "String" => "getNextInString"
      case "Java List" => "getNextInJavaIterator"
      case "Java Set" => "getNextInJavaIterator"
      case "Java Map" => "getNextInJavaMap"
      case _ => "getNext"
    }

  def iterator(colName: String): String =
    colName match {
      case "Java List" => "iterator"
      case "Java Set" => "iterator"
      case "Java Map" => "entrySet.iterator"
      case _ => "toIterator"
    }

  class ForAllTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should pass when all elements passed for " + colName, new SimpleTemplate("forAll(" + col + ") { e => assert(" + lhs + " < 4) }")),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName,
                        new InterceptWithCauseTemplate(
                          "val col = " + col,
                          "forAll(col) { e => \n" +
                          "  assert(" + lhs + " != 2) \n" +
                          "}",
                          "ForAllInspectorsSpec.scala",
                          "\"forAll failed, because: \\n\" + \n" +
                          "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (ForAllInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
                          "\"in \" + decorateToStringValue(prettifier, col)",
                          5,
                          "ForAllInspectorsSpec.scala",
                          "\"2 equaled 2\"",
                          11)
                        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName,
          new InterceptWithCauseTemplate(
            "val col = " + col + "\n" +
            "val firstViolation = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 2)",
            "forAll(col) { e => \n" +
              "  assert(" + lhs + " < 2) \n" +
              "}",
            "ForAllInspectorsSpec.scala",
            "\"forAll failed, because: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "firstViolation") + " + \", \" + " + getLhs(colName, "firstViolation") + " + \" was not less than 2 (ForAllInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5,
            "ForAllInspectorsSpec.scala",
            getLhs(colName, "firstViolation") + " + \" was not less than 2\"",
            11)
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forAll(col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forAll(col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forAll(col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forAll(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forAll(col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forAll(col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forAll(col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forAll(col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forAll(col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForAllTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should pass when all elements passed for " + colName, new SimpleTemplate("forAll(" + col + ") { e => Future { assert(" + lhs + " < 4) } }")),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName + " inside future block",
          new RecoverToExceptionIfWithCauseTemplate(
            "val col = " + col,
            "forAll(col) { e => \n" +
            "  Future { assert(" + lhs + " != 2) } \n" +
            "}",
            "AsyncForAllInspectorsSpec.scala",
            "\"forAll failed, because: \\n\" + \n" +
              "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (AsyncForAllInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5,
            "AsyncForAllInspectorsSpec.scala",
            "\"2 equaled 2\"",
            11)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName + " outside future block",
          new RecoverToExceptionIfWithCauseTemplate(
            "val col = " + col,
            "forAll(col) { e => \n" +
            "  assert(" + lhs + " != 2)\n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAllInspectorsSpec.scala",
            "\"forAll failed, because: \\n\" + \n" +
              "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (AsyncForAllInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6,
            "AsyncForAllInspectorsSpec.scala",
            "\"2 equaled 2\"",
            12)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName + " inside future block",
          new RecoverToExceptionIfWithCauseTemplate(
            "val col = " + col + "\n" +
              "val firstViolation = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 2)",
            "forAll(col) { e => \n" +
            "  Future { assert(" + lhs + " < 2) } \n" +
            "}",
            "AsyncForAllInspectorsSpec.scala",
            "\"forAll failed, because: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "firstViolation") + " + \", \" + " + getLhs(colName, "firstViolation") + " + \" was not less than 2 (AsyncForAllInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5,
            "AsyncForAllInspectorsSpec.scala",
            getLhs(colName, "firstViolation") + " + \" was not less than 2\"",
            11)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName + " outside future block",
          new RecoverToExceptionIfWithCauseTemplate(
            "val col = " + col + "\n" +
            "val firstViolation = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 2)",
            "forAll(col) { e => \n" +
            "  assert(" + lhs + " < 2) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAllInspectorsSpec.scala",
            "\"forAll failed, because: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "firstViolation") + " + \", \" + " + getLhs(colName, "firstViolation") + " + \" was not less than 2 (AsyncForAllInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6,
            "AsyncForAllInspectorsSpec.scala",
            getLhs(colName, "firstViolation") + " + \" was not less than 2\"",
            12)
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forAll(col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
              "  forAll(col) { e => pending; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forAll(col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
              "  forAll(col) { e => cancel; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forAll(col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[AnnotationFormatError] {\n" +
              "  forAll(col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forAll(col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[CoderMalfunctionError] {\n" +
              "  forAll(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[FactoryConfigurationError] {\n" +
              "  forAll(col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[FactoryConfigurationError] {\n" +
              "  forAll(col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
              "  forAll(col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
              "  forAll(col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForAtLeastTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should throw IllegalArgumentException when 0 is passed in as min for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtLeast(0, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'min' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when -1 is passed in as min for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtLeast(-1, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'min' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should pass when minimum count of elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => assert(" + lhs + " == 2) }"
          )
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val firstIndex = getIndex(col, first)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val secondIndex = getIndex(col, second)\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 2) \n" +
            "}",
            "ForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "}",
            "ForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "}",
            "ForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val failed = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " == 3)\n",
            "forAtLeast(3, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "}",
            "ForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(3) failed, because only 2 elements satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should pass when more than minimum count of elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => assert(" + lhs + " < 3) }"
          )
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when none of the elements passed for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(1, col) { e => \n" +
            "  assert(" + lhs + " > 5) \n" +
            "}",
            "ForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(1) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 5 (ForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should pass when all of the elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => assert(" + lhs + " < 5) }"
          )
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forAtLeast(1, col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forAtLeast(1, col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forAtLeast(1, col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forAtLeast(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forAtLeast(1, col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forAtLeast(1, col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forAtLeast(1, col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForAtLeastTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should throw IllegalArgumentException when 0 is passed in as min for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtLeast(0, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'min' argument must be more than 0\")\n"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when -1 is passed in as min for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtLeast(-1, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'min' argument must be more than 0\")\n"
          )
        ),
        new ItTemplate("should pass when minimum count of elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => Future { assert(" + lhs + " == 2) } }"
          )
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val firstIndex = getIndex(col, first)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val secondIndex = getIndex(col, second)\n",
            "forAtLeast(2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 2) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val firstIndex = getIndex(col, first)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val secondIndex = getIndex(col, second)\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 2) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(2, col) { e => \n" +
              "  Future { assert(" + lhs + " == 5) } \n" +
              "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 10) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n",
            "forAtLeast(2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 2) }\n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n",
            "forAtLeast(2, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "  Future { assert(true) }\n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(2) failed, because only 1 element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val failed = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " == 3)\n",
            "forAtLeast(3, col) { e => \n" +
            "  Future { assert(" + lhs + " < 3) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(3) failed, because only 2 elements satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val failed = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " == 3)\n",
            "forAtLeast(3, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(3) failed, because only 2 elements satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should pass when more than minimum count of elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => Future { assert(" + lhs + " < 3) } }"
          )
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when none of the elements passed for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(1, col) { e => \n" +
            "  Future { assert(" + lhs + " > 5) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(1) failed, because no element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when none of the elements passed for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forAtLeast(1, col) { e => \n" +
            "  assert(" + lhs + " > 5) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAtLeastInspectorsSpec.scala",
            "\"forAtLeast(1) failed, because no element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 5 (AsyncForAtLeastInspectorsSpec.scala:\" + (thisLineNumber - 10) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should pass when all of the elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtLeast(1, col) { e => Future { assert(" + lhs + " < 5) } }"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forAtLeast(1, col) { e => Future { pending ; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forAtLeast(1, col) { e => pending ; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forAtLeast(1, col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forAtLeast(1, col) { e => cancel; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forAtLeast(1, col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forAtLeast(1, col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forAtLeast(1, col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forAtLeast(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forAtLeast(1, col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForAtMostTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should throw IllegalArgumentException when 0 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtMost(0, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'max' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when -1 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtMost(-1, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'max' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should pass when number of elements passed is less than maximum allowed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtMost(2, col) { e => assert(" + lhs + " == 2) }"
          )
        ),
        new DefTemplate("should pass when number of elements passed equal to maximum allowed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtMost(2, col) { e => assert(" + lhs + " < 3) }"
          )
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n",
            "forAtMost(2, col) { e => \n" +
            "  assert(" + lhs + " < 4) \n" +
            "}",
            "ForAtMostInspectorsSpec.scala",
            "\"forAtMost(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should pass when none of the elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtMost(2, col) { e => assert(" + lhs + " > 5) }"
          )
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forAtMost(1, col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forAtMost(1, col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forAtMost(1, col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forAtMost(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forAtMost(1, col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forAtMost(1, col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forAtMost(1, col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forAtMost(1, col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForAtMostTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should throw IllegalArgumentException when 0 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtMost(0, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'max' argument must be more than 0\")\n"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when -1 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forAtMost(-1, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'max' argument must be more than 0\")"
          )
        ),
        new ItTemplate("should pass when number of elements passed is less than maximum allowed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtMost(2, col) { e => Future { assert(" + lhs + " == 2) } }"
          )
        ),
        new ItTemplate("should pass when number of elements passed equal to maximum allowed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forAtMost(2, col) { e => Future { assert(" + lhs + " < 3) } }"
          )
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n",
            "forAtMost(2, col) { e => \n" +
            "  Future { assert(" + lhs + " < 4) } \n" +
            "}",
            "AsyncForAtMostInspectorsSpec.scala",
            "\"forAtMost(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 4)\n",
            "forAtMost(2, col) { e => \n" +
            "  assert(" + lhs + " < 4) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForAtMostInspectorsSpec.scala",
            "\"forAtMost(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should pass when none of the elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "forAtMost(2, col) { e => Future { assert(" + lhs + " > 5) } }"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forAtMost(1, col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forAtMost(1, col) { e => pending; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forAtMost(1, col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forAtMost(1, col) { e => cancel; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[AnnotationFormatError] {\n" +
              "  forAtMost(1, col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forAtMost(1, col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forAtMost(1, col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[CoderMalfunctionError] {\n" +
              "  forAtMost(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forAtMost(1, col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forAtMost(1, col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForExactlyTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should throw IllegalArgumentException when 0 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forExactly(0, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'succeededCount' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when -1 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forExactly(-1, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'succeededCount' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should pass when number of element passes is equal to specified succeeded count for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forExactly(2, col) { e => assert(" + lhs + " < 3) }"
          )
        ),
        new DefTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
              "val itr = col." + iterator(colName) + "\n" +
              "val first = itr.next\n" +
              "val second = itr.next\n" +
              "val third = itr.next\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " < 5)\n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forExactly(3, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(3) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n",
            "forExactly(1, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(1) failed, because 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \" in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 2) \n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (ForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " < 5)\n" +
            "}",
            "ForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forExactly(1, col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forExactly(1, col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forExactly(1, col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forExactly(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forExactly(1, col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forExactly(1, col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forExactly(1, col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forExactly(1, col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forExactly(1, col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForExactlyTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should throw IllegalArgumentException when 0 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forExactly(0, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'succeededCount' argument must be more than 0\")"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when -1 is passed in as max for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forExactly(-1, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'succeededCount' argument must be more than 0\")"
          )
        ),
        new ItTemplate("should pass when number of element passes is equal to specified succeeded count for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forExactly(2, col) { e => Future { assert(" + lhs + " < 3) } }"
          )
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forExactly(2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 5) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 10) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 2) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "  Future { assert(true) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  Future { assert(" + lhs + " < 5) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " < 5)\n" +
            "  Future { assert(true) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forExactly(3, col) { e => \n" +
              "  Future { assert(" + lhs + " < 3) } \n" +
              "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(3) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forExactly(3, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(3) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n",
            "forExactly(1, col) { e => \n" +
            "  Future { assert(" + lhs + " < 3) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(1) failed, because 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \" in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n",
            "forExactly(1, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(1) failed, because 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \" in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 2) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " == 2) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForExactlyInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  Future { assert(" + lhs + " < 5) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 5)\n",
            "forExactly(2, col) { e => \n" +
            "  assert(" + lhs + " < 5)\n" +
            "  Future { assert(true) }\n" +
            "}",
            "AsyncForExactlyInspectorsSpec.scala",
            "\"forExactly(2) failed, because 3 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \" and \" + " + getIndexOrKey(colName, "third") + " + \" in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forExactly(1, col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
              "  forExactly(1, col) { e => pending; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forExactly(1, col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
              "  forExactly(1, col) { e => cancel; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forExactly(1, col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[AnnotationFormatError] {\n" +
              "  forExactly(1, col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forExactly(1, col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forExactly(1, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forExactly(1, col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[FactoryConfigurationError] {\n" +
              "  forExactly(1, col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forExactly(1, col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
              "  forExactly(1, col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForNoTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should pass when none of the element pass for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forNo(col) { e => assert(" + lhs + " > 5) }"
          )
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when 1 element passed for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forNo(col) { e => assert(" + lhs + " == 2) }\n",
            "ForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            3)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when 2 element passed for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " < 5)",
            "forNo(col) { e => assert(" + lhs + " < 5) }\n",
            "ForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            3)
        ),
        new DefTemplate("should pass when empty list of element is passed in for " + colName,
          new SimpleTemplate(
            "val col = " + emptyCol + "\n" +
            "forNo(col) { e => assert(" + lhs + " > 5) }"
          )
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forNo(col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forNo(col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forNo(col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forNo(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forNo(col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forNo(col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forNo(col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forNo(col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forNo(col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForNoTemplate(colName: String, col: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should pass when none of the element pass for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forNo(col) { e => Future { assert(" + lhs + " > 5) } }"
          )
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when 1 element passed for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forNo(col) { e => Future { assert(" + lhs + " == 2) } }\n",
            "AsyncForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when 1 element passed for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forNo(col) { e => assert(" + lhs + " == 2); Future { assert(true) } }\n",
            "AsyncForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when 2 element passed for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " < 5)",
            "forNo(col) { e => Future { assert(" + lhs + " < 5) } }\n",
            "AsyncForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when 2 element passed for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val first = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " < 5)",
            "forNo(col) { e => assert(" + lhs + " < 5); Future { assert(true) } }\n",
            "AsyncForNoInspectorsSpec.scala",
            "\"forNo failed, because 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should pass when empty list of element is passed in for " + colName,
          new SimpleTemplate(
            "val col = " + emptyCol + "\n" +
            "forNo(col) { e => Future { assert(" + lhs + " > 5) } }"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forNo(col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forNo(col) { e => pending; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forNo(col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forNo(col) { e => cancel; Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forNo(col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forNo(col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forNo(col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forNo(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forNo(col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forNo(col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forNo(col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forNo(col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForBetweenTemplate(colName: String, col: String, bigCol: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should throw IllegalArgumentException when -1 is passed in as from for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(-1, 2, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'from' argument must be more than or equal 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when 0 is passed in as upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(0, 0, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when -1 is passed in as upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(0, -1, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 0\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when from and upTo is the same for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(1, 1, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 'from' argument\")"
          )
        ),
        new DefTemplate("should throw IllegalArgumentException when from is greater than upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(3, 2, col) { e => assert(" + lhs + " == 2) }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 'from' argument\")"
          )
        ),
        new DefTemplate("should pass when number of element passed is within the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => assert(" + lhs + " > 2) }"
          )
        ),
        new DefTemplate("should pass when number of element passed is same as lower bound of the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => assert(" + lhs + " > 3) }"
          )
        ),
        new DefTemplate("should pass when number of element passed is same as upper bound of the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => assert(" + lhs + " > 1) }"
          )
        ),
        new DefTemplate("should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forBetween(1, 2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "}",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(1, 2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forBetween(2, 3, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "}",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forBetween(3, 4, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "}",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(3, 4) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo' for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n",
            "forBetween(2, 3, col) { e => \n" +
            "  assert(" + lhs + " > 1) \n" +
            "}",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because 4 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \" and \" + " + getIndexOrKey(colName, "forth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range for " + colName,
          new InterceptWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " > 4)",
            "forBetween(2, 4, col) { e => \n" +
            "  assert(" + lhs + " > 4) \n" +
            "}",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 4 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 4 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 4 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "forth") + " + \", \" + " + getLhs(colName, "forth") + " + \" was not greater than 4 (ForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            5)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range for " + colName,
          new InterceptTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n" +
            "val forth = itr.next\n" +
            "val fifth = itr.next\n",
            "forBetween(2, 4, col) { e => assert(" + lhs + " > 0) }",
            "ForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because 5 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \", \" + " + getIndexOrKey(colName, "forth") + " + \" and \" + " + getIndexOrKey(colName, "fifth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            3)
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forBetween(2, 4, col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forBetween(2, 4, col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forBetween(2, 4, col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forBetween(2, 4, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forBetween(2, 4, col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[LinkageError] {\n" +
            "  forBetween(2, 4, col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forBetween(2, 4, col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forBetween(2, 4, col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forBetween(2, 4, col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForBetweenTemplate(colName: String, col: String, bigCol: String, emptyCol: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should throw IllegalArgumentException when -1 is passed in as from for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(-1, 2, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'from' argument must be more than or equal 0\")"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when 0 is passed in as upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(0, 0, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 0\")"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when -1 is passed in as upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(0, -1, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 0\")"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when from and upTo is the same for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(1, 1, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 'from' argument\")"
          )
        ),
        new ItTemplate("should throw IllegalArgumentException when from is greater than upTo for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "val e = intercept[IllegalArgumentException] {\n" +
            "  forBetween(3, 2, col) { e => Future { assert(" + lhs + " == 2) } }\n" +
            "}\n" +
            "assert(e.getMessage == \"'upTo' argument must be more than 'from' argument\")"
          )
        ),
        new ItTemplate("should pass when number of element passed is within the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => Future { assert(" + lhs + " > 2) } }"
          )
        ),
        new ItTemplate("should pass when number of element passed is same as lower bound of the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => Future { assert(" + lhs + " > 3) } }"
          )
        ),
        new ItTemplate("should pass when number of element passed is same as upper bound of the specified range for " + colName,
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "forBetween(2, 4, col) { e => Future { assert(" + lhs + " > 1) } }"
          )
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forBetween(1, 2, col) { e => \n" +
            "  Future { assert(" + lhs + " == 5) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(1, 2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n",
            "forBetween(1, 2, col) { e => \n" +
            "  assert(" + lhs + " == 5) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(1, 2) failed, because no element satisfied the assertion block: \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" did not equal 5 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 10) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forBetween(2, 3, col) { e => \n" +
            "  Future { assert(" + lhs + " == 2) }\n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " != 2)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " == 2)",
            "forBetween(2, 3, col) { e => \n" +
            "  assert(" + lhs + " == 2)\n" +
            "  Future { assert(true) }\n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" did not equal 2 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" did not equal 2 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forBetween(3, 4, col) { e => \n" +
            "  Future { assert(" + lhs + " < 3) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(3, 4) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from' for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " < 3)\n" +
            "val failed = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " >= 3)",
            "forBetween(3, 4, col) { e => \n" +
            "  assert(" + lhs + " < 3) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(3, 4) failed, because only 2 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \" and \" + " + getIndexOrKey(colName, "second") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "failed") + " + \", \" + " + getLhs(colName, "failed") + " + \" was not less than 3 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo' for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n",
            "forBetween(2, 3, col) { e => \n" +
            "  Future { assert(" + lhs + " > 1) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because 4 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \" and \" + " + getIndexOrKey(colName, "forth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo' for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " > 1)\n",
            "forBetween(2, 3, col) { e => \n" +
            "  assert(" + lhs + " > 1) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 3) failed, because 4 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \" and \" + " + getIndexOrKey(colName, "forth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range for " + colName + " inside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " > 4)",
            "forBetween(2, 4, col) { e => \n" +
            "  Future { assert(" + lhs + " > 4) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 7) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
              "\"  at \" + " + getVariableIndexForType(colName, "forth") + " + \", \" + " + getLhs(colName, "forth") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 10) + \") \\n\" + \n" +
              "\"in \" + decorateToStringValue(prettifier, col)",
            6)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range for " + colName + " outside future block",
          new RecoverToExceptionIfWithNullCauseTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val third = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val forth = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " <= 4)\n" +
            "val succeeded = " + getFirst(colName) + getElementType(colName) + "(col, " + getLhs(colName, "_") + " > 4)",
            "forBetween(2, 4, col) { e => \n" +
            "  assert(" + lhs + " > 4) \n" +
            "  Future { assert(true) } \n" +
            "}",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because only 1 element satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "succeeded") + " + \": \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 8) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 9) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "third") + " + \", \" + " + getLhs(colName, "third") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 10) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "forth") + " + \", \" + " + getLhs(colName, "forth") + " + \" was not greater than 4 (AsyncForBetweenInspectorsSpec.scala:\" + (thisLineNumber - 11) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            7)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n" +
            "val forth = itr.next\n" +
            "val fifth = itr.next\n",
            "forBetween(2, 4, col) { e => Future { assert(" + lhs + " > 0) } }",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because 5 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \", \" + " + getIndexOrKey(colName, "forth") + " + \" and \" + " + getIndexOrKey(colName, "fifth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + bigCol + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = itr.next\n" +
            "val second = itr.next\n" +
            "val third = itr.next\n" +
            "val forth = itr.next\n" +
            "val fifth = itr.next\n",
            "forBetween(2, 4, col) { e => assert(" + lhs + " > 0); Future { assert(true) } }",
            "AsyncForBetweenInspectorsSpec.scala",
            "\"forBetween(2, 4) failed, because 5 elements satisfied the assertion block at " + getIndexOrKeyWord(colName) + " \" + " + getIndexOrKey(colName, "first") + " + \", \" + " + getIndexOrKey(colName, "second") + " + \", \" + " + getIndexOrKey(colName, "third") + " + \", \" + " + getIndexOrKey(colName, "forth") + " + \" and \" + " + getIndexOrKey(colName, "fifth") + " + \" in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forBetween(2, 4, col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
              "  forBetween(2, 4, col) { e => pending; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forBetween(2, 4, col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
              "  forBetween(2, 4, col) { e => cancel; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forBetween(2, 4, col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[AnnotationFormatError] {\n" +
              "  forBetween(2, 4, col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forBetween(2, 4, col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[CoderMalfunctionError] {\n" +
              "  forBetween(2, 4, col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forBetween(2, 4, col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[FactoryConfigurationError] {\n" +
              "  forBetween(2, 4, col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forBetween(2, 4, col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + bigCol + "\n" +
              "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
              "  forBetween(2, 4, col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
              "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class ForEveryTemplate(colName: String, col: String, lhs: String) extends Template {
    override val children =
      List(
        new DefTemplate("should pass when all elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forEvery(col) { e => assert(" + lhs + " < 4) }"
          )
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName,
          new InterceptTemplate(
            "val col = " + col,
            "forEvery(col) { e => assert(" + lhs + " != 2) }",
            "ForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (ForEveryInspectorsSpec.scala:\" + (thisLineNumber - 5) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            3)
        ),
        new DefTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName,
          new InterceptTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n",
            "forEvery(col) { e => assert(" + lhs + " < 2) }",
            "ForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not less than 2 (ForEveryInspectorsSpec.scala:\" + (thisLineNumber - 5) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not less than 2 (ForEveryInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            3)
        ),
        new DefTemplate("should propagate TestPendingException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestPendingException] {\n" +
            "  forEvery(col) { e => pending }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate TestCanceledException thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[exceptions.TestCanceledException] {\n" +
            "  forEvery(col) { e => cancel }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[AnnotationFormatError] {\n" +
            "  forEvery(col) { e => throw new AnnotationFormatError(\"test\") }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[CoderMalfunctionError] {\n" +
            "  forEvery(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")) }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[FactoryConfigurationError] {\n" +
            "  forEvery(col) { e => throw new FactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.LinkageError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[LinkageError] {\n" +
            "  forEvery(col) { e => throw new LinkageError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.ThreadDeath thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[ThreadDeath] {\n" +
            "  forEvery(col) { e => throw new ThreadDeath() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[TransformerFactoryConfigurationError] {\n" +
            "  forEvery(col) { e => throw new TransformerFactoryConfigurationError() }\n" +
            "}"
          )
        ),
        new DefTemplate("should propagate java.lang.VirtualMachineError thrown from assertion for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "intercept[VirtualMachineError] {\n" +
            "  forEvery(col) { e => throw new VirtualMachineError() {} }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  class AsyncForEveryTemplate(colName: String, col: String, lhs: String) extends Template {
    override val children =
      List(
        new ItTemplate("should pass when all elements passed for " + colName,
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "forEvery(col) { e => Future { assert(" + lhs + " < 4) } }"
          )
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col,
            "forEvery(col) { e => Future { assert(" + lhs + " != 2) } }",
            "AsyncForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when at least one element failed for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col,
            "forEvery(col) { e => assert(" + lhs + " != 2); Future { assert(true) } }",
            "AsyncForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getIndexForType(colName, 2) + " + \", 2 equaled 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 6) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName + " inside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n",
            "forEvery(col) { e => Future { assert(" + lhs + " < 2) } }",
            "AsyncForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not less than 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not less than 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should throw TestFailedException with correct stack depth and message when more than one element failed for " + colName + " outside future block",
          new RecoverToExceptionIfTemplate(
            "val col = " + col + "\n" +
            "val itr = col." + iterator(colName) + "\n" +
            "val first = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n" +
            "val second = " + getNext(colName) + getElementType(colName) + "(itr, " + getLhs(colName, "_") + " >= 2)\n",
            "forEvery(col) { e => assert(" + lhs + " < 2); Future { assert(true) } }",
            "AsyncForEveryInspectorsSpec.scala",
            "\"forEvery failed, because: \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "first") + " + \", \" + " + getLhs(colName, "first") + " + \" was not less than 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 6) + \"), \\n\" + \n" +
            "\"  at \" + " + getVariableIndexForType(colName, "second") + " + \", \" + " + getLhs(colName, "second") + " + \" was not less than 2 (AsyncForEveryInspectorsSpec.scala:\" + (thisLineNumber - 7) + \") \\n\" + \n" +
            "\"in \" + decorateToStringValue(prettifier, col)",
            4)
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
            "  forEvery(col) { e => Future { pending; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestPendingException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestPendingException] {\n" +
              "  forEvery(col) { e => pending; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
            "  forEvery(col) { e => Future { cancel; succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate TestCanceledException thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
              "recoverToSucceededIf[exceptions.TestCanceledException] {\n" +
              "  forEvery(col) { e => cancel; Future { succeed } }\n" +
              "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forEvery(col) { e => Future { throw new AnnotationFormatError(\"test\"); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[AnnotationFormatError] {\n" +
            "  forEvery(col) { e => throw new AnnotationFormatError(\"test\"); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forEvery(col) { e => Future { throw new CoderMalfunctionError(new RuntimeException(\"test\")); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[CoderMalfunctionError] {\n" +
            "  forEvery(col) { e => throw new CoderMalfunctionError(new RuntimeException(\"test\")); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forEvery(col) { e => Future { throw new FactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[FactoryConfigurationError] {\n" +
            "  forEvery(col) { e => throw new FactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " inside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forEvery(col) { e => Future { throw new TransformerFactoryConfigurationError(); succeed } }\n" +
            "}"
          )
        ),
        new ItTemplate("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion for " + colName + " outside future block",
          new SimpleTemplate(
            "val col = " + col + "\n" +
            "recoverToSucceededIf[TransformerFactoryConfigurationError] {\n" +
            "  forEvery(col) { e => throw new TransformerFactoryConfigurationError(); Future { succeed } }\n" +
            "}"
          )
        )
      )

    override protected def childrenContent =
      children.map(_.toString).mkString("\n") + "\n"

    override def toString = childrenContent
  }

  def genForAllSpecFile(targetDir: File): File = {
    val forAllSpecFile = new File(targetDir, "ForAllInspectorsSpec.scala")
    if (!forAllSpecFile.exists || generatorSource.lastModified > forAllSpecFile.lastModified) {
      genFile(
        forAllSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.forall"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForAllInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForAllTemplate(name, col, emptyCol, lhs)
            }
          }
        )
      )
    }
    forAllSpecFile
  }

  def genAsyncForAllSpecFile(targetDir: File): File = {
    val forAllSpecFile = new File(targetDir, "AsyncForAllInspectorsSpec.scala")
    genFile(
      forAllSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.forall"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForAllInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForAllTemplate(name, col, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForAtLeastSpecFile(targetDir: File): File =  {
    val forAtLeastSpecFile = new File(targetDir, "ForAtLeastInspectorsSpec.scala")
    if (!forAtLeastSpecFile.exists || generatorSource.lastModified > forAtLeastSpecFile.lastModified) {
      genFile(
        forAtLeastSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.foratleast"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForAtLeastInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForAtLeastTemplate(name, col, emptyCol, lhs)
            }
          }
        )
      )
    }
    forAtLeastSpecFile
  }

  def genAsyncForAtLeastSpecFile(targetDir: File): File = {
    val forAtLeastSpecFile = new File(targetDir, "AsyncForAtLeastInspectorsSpec.scala")
    genFile(
      forAtLeastSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.foratleast"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForAtLeastInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForAtLeastTemplate(name, col, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForAtMostSpecFile(targetDir: File): File = {
    val forAtMostSpecFile = new File(targetDir, "ForAtMostInspectorsSpec.scala")
    if (!forAtMostSpecFile.exists || generatorSource.lastModified > forAtMostSpecFile.lastModified) {
      genFile(
        forAtMostSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.foratmost"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForAtMostInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForAtMostTemplate(name, col, emptyCol, lhs)
            }
          }
        )
      )
    }
    forAtMostSpecFile
  }

  def genAsyncForAtMostSpecFile(targetDir: File): File = {
    val forAtMostSpecFile = new File(targetDir, "AsyncForAtMostInspectorsSpec.scala")
    genFile(
      forAtMostSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.foratmost"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForAtMostInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForAtMostTemplate(name, col, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForExactlySpecFile(targetDir: File): File = {
    val forExactlySpecFile = new File(targetDir, "ForExactlyInspectorsSpec.scala")
    if (!forExactlySpecFile.exists || generatorSource.lastModified > forExactlySpecFile.lastModified) {
      genFile(
        forExactlySpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.forexactly"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForExactlyInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForExactlyTemplate(name, col, emptyCol, lhs)
            }
          }
        )
      )
    }
    forExactlySpecFile
  }

  def genAsyncForExactlySpecFile(targetDir: File): File = {
    val forExactlySpecFile = new File(targetDir, "AsyncForExactlyInspectorsSpec.scala")
    genFile(
      forExactlySpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.forexactly"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForExactlyInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForExactlyTemplate(name, col, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForNoSpecFile(targetDir: File): File = {
    val forNoSpecFile = new File(targetDir, "ForNoInspectorsSpec.scala")
    if (!forNoSpecFile.exists || generatorSource.lastModified > forNoSpecFile.lastModified) {
      genFile(
        forNoSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.forno"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForNoInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForNoTemplate(name, col, emptyCol, lhs)
            }
          }
        )
      )
    }
    forNoSpecFile
  }

  def genAsyncForNoSpecFile(targetDir: File): File = {
    val forNoSpecFile = new File(targetDir, "AsyncForNoInspectorsSpec.scala")
    genFile(
      forNoSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.forno"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForNoInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForNoTemplate(name, col, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForBetweenSpecFile(targetDir: File): File = {
    val forBetweenSpecFile = new File(targetDir, "ForBetweenInspectorsSpec.scala")
    if (!forBetweenSpecFile.exists || generatorSource.lastModified > forBetweenSpecFile.lastModified) {
      genFile(
        forBetweenSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.forbetween"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForBetweenInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForBetweenTemplate(name, col, bigCol, emptyCol, lhs)
            }
          }
        )
      )
    }
    forBetweenSpecFile
  }

  def genAsyncForBetweenSpecFile(targetDir: File): File = {
    val forBetweenSpecFile = new File(targetDir, "AsyncForBetweenInspectorsSpec.scala")
    genFile(
      forBetweenSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.forbetween"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForBetweenInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForBetweenTemplate(name, col, bigCol, emptyCol, lhs)
          }
        }
      )
    )
  }

  def genForEverySpecFile(targetDir: File): File = {
    val forEverySpecFile = new File(targetDir, "ForEveryInspectorsSpec.scala")
    if (!forEverySpecFile.exists || generatorSource.lastModified > forEverySpecFile.lastModified) {
      genFile(
        forEverySpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.forevery"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "Inspectors._",
            "java.lang.annotation.AnnotationFormatError",
            "java.nio.charset.CoderMalfunctionError",
            "javax.xml.parsers.FactoryConfigurationError",
            "javax.xml.transform.TransformerFactoryConfigurationError",
            "org.scalatest.refspec.RefSpec"
          ),
          classTemplate = new ClassTemplate {
            val name = "ForEveryInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List.empty
            override val children = collectionTypes.map {
              case (name, col, bigCol, emptyCol, lhs) => new ForEveryTemplate(name, col, lhs)
            }
          }
        )
      )
    }
    forEverySpecFile
  }

  def genAsyncForEverySpecFile(targetDir: File): File = {
    val forEverySpecFile = new File(targetDir, "AsyncForEveryInspectorsSpec.scala")
    genFile(
      forEverySpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.forevery"),
        importList = List("org.scalatest._",
          "SharedHelpers._",
          "FailureMessages.decorateToStringValue",
          "org.scalactic.ColCompatHelper.Iterable",
          "Inspectors._",
          "java.lang.annotation.AnnotationFormatError",
          "java.nio.charset.CoderMalfunctionError",
          "javax.xml.parsers.FactoryConfigurationError",
          "javax.xml.transform.TransformerFactoryConfigurationError",
          "org.scalatest.funspec.AsyncFunSpec",
          "scala.concurrent.Future"
        ),
        classTemplate = new ClassTemplate {
          val name = "AsyncForEveryInspectorsSpec"
          override val extendName = Some("AsyncFunSpec")
          override val withList = List.empty
          override val children = collectionTypes.map {
            case (name, col, bigCol, emptyCol, lhs) => new AsyncForEveryTemplate(name, col, lhs)
          }
        }
      )
    )
  }
  
  def genNestedInspectorsSpecFile(targetDir: File): File = {
    val nestedInspectorsSpecFile = new File(targetDir, "NestedInspectorsSpec.scala")
    if (!nestedInspectorsSpecFile.exists || generatorSource.lastModified > nestedInspectorsSpecFile.lastModified) {
      genFile(
        nestedInspectorsSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.nested"),
          importList = List("org.scalatest._",
            "SharedHelpers._",
            "FailureMessages.decorateToStringValue",
            "org.scalactic.ColCompatHelper.Iterable",
            "org.scalatest.refspec.RefSpec"),
          classTemplate = new ClassTemplate {
            val name = "NestedInspectorsSpec"
            override val extendName = Some("RefSpec")
            override val withList = List("Inspectors")
            override val children = {

              val succeededAssertion = "assert(n % 2 == 0)"
              val failedAssertion = "assert(n % 2 == 1)"

              val succeededNestedList = List(("forAll", "forAll(l) { n =>\n"),
                ("forAtLeast", "forAtLeast(3, l) { n =>\n"),
                ("forAtMost", "forAtMost(3, l) { n =>\n"),
                ("forExactly", "forExactly(3, l) { n =>\n"),
                ("forNo", "forNo(l) { n =>\n"),
                ("forBetween", "forBetween(2, 4, l) { n =>\n"),
                ("forEvery", "forEvery(l) { n =>\n"))

              val failedNestedList = List(("forAll", "forAll(l) { n =>\n"),
                ("forAtLeast", "forAtLeast(3, l) { n =>\n"),
                ("forAtMost", "forAtMost(3, l) { n =>\n"),
                ("forExactly", "forExactly(4, l) { n =>\n"),
                ("forNo", "forNo(l) { n =>\n"),
                ("forBetween", "forBetween(2, 4, l) { n =>\n"),
                ("forEvery", "forEvery(l) { n =>\n"))

              val theList = List(List(2, 4, 6, 8), List(8, 10, 12, 16))

              // Generate code by templates
              (List(
                ("forAll", "forAll(List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion),
                ("forAtLeast", "forAtLeast(2, List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion),
                ("forAtMost", "forAtMost(2, List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion),
                ("forExactly", "forExactly(2, List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion),
                ("forNo", "forNo(List(List(0, 2, 4, 6), List(8, 10, 12, 14)))",
                  (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion),
                ("forBetween", "forBetween(2, 4, List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion),
                ("forEvery", "forEvery(List(List(2, 4, 6), List(8, 10, 12)))",
                  (name: String, text: String) => if (name != "forNo") succeededAssertion else failedAssertion)
              ) flatMap { case (forType, forText, assertFun) =>
                succeededNestedList map { case (name, text) =>
                  new NestedSucceedTemplate(forType, forText, name, text, assertFun(name, text))
                }
              }) ++
                (List(
                  ("forAll", "forAll(xs)", false,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion),
                  ("forAtLeast", "forAtLeast(3, xs)", true,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion),
                  ("forAtMost", "forAtMost(1, xs)", true,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") succeededAssertion else failedAssertion),
                  ("forExactly", "forExactly(1, xs)", true,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion),
                  ("forNo", "forNo(xs)", false,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") succeededAssertion else failedAssertion),
                  ("forBetween", "forBetween(2, 4, xs)", true,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion),
                  ("forEvery", "forEvery(xs)", true,
                    (name: String, text: String) => if (name != "forNo" && name != "forAtMost") failedAssertion else succeededAssertion)
                ) flatMap { case (forType, forText, full, assertFun) =>
                  failedNestedList map { case (name, text) =>
                    new NestedFailedTemplate(theList.toString, forType, forText, name, text, assertFun(name, text), getNestedMessageTemplate(forType, name, full, theList, nestedInspectorsSpecFile.getName))
                  }
                }
                  )
            }
          }
        )
      )
    }
    nestedInspectorsSpecFile
  }

  def getErrorMessageValuesFunName(colType: String, errorFun: String): String = {
    val typeParamOpenIdx = errorFun.indexOf("[")
    val funName =
      if (typeParamOpenIdx >= 0)
        errorFun.substring(0, typeParamOpenIdx)
      else
        errorFun
    val typeParam =
      if (typeParamOpenIdx >= 0)
        errorFun.substring(typeParamOpenIdx)
      else
        ""

    funName +
      (colType match {
        case "Array[String]" => "Array"
        case _ => ""
      }) + typeParam
  }

  def targetDir(targetBaseDir: File, packageName: String): File = {
    val targetDir = new File(targetBaseDir, "org/scalatest/inspectors/" + packageName)
    if (!targetDir.exists)
      targetDir.mkdirs()
    targetDir
  }

  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File] =  {
    Seq(
      genForAllSpecFile(targetDir(targetBaseDir, "forall")),
      genAsyncForAllSpecFile(targetDir(targetBaseDir, "forall")),
      genForAtLeastSpecFile(targetDir(targetBaseDir, "foratleast")),
      genAsyncForAtLeastSpecFile(targetDir(targetBaseDir, "foratleast")),
      genForAtMostSpecFile(targetDir(targetBaseDir, "foratmost")),
      genAsyncForAtMostSpecFile(targetDir(targetBaseDir, "foratmost")),
      genForExactlySpecFile(targetDir(targetBaseDir, "forexactly")),
      genAsyncForExactlySpecFile(targetDir(targetBaseDir, "forexactly")),
      genForNoSpecFile(targetDir(targetBaseDir, "forno")),
      genAsyncForNoSpecFile(targetDir(targetBaseDir, "forno")),
      genForBetweenSpecFile(targetDir(targetBaseDir, "forbetween")),
      genAsyncForBetweenSpecFile(targetDir(targetBaseDir, "forbetween")),
      genForEverySpecFile(targetDir(targetBaseDir, "forevery")),
      genAsyncForEverySpecFile(targetDir(targetBaseDir, "forevery")),
      genNestedInspectorsSpecFile(targetDir(targetBaseDir, "nested"))
    )
  }
  
  def main(args: Array[String]) {
    val targetBaseDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetBaseDir), version, scalaVersion)
  }
  
}
