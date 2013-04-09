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
import collection.GenTraversable
import scala.annotation.tailrec

object GenInspectors {
  
  import Generator._
  
  // Templates
  class IndexesTemplate(indexes: List[Int]) extends Template {
    override def toString = 
      if (indexes.length > 1)
        indexes.dropRight(1).mkString(", ") + " and " + indexes.last
      else
        indexes.mkString(", ")
  }
  
  class ErrorDetailTemplate(index: String, fileName: String, lineNumber: String, messageTemplate: Template) extends Template {
    override def toString = 
      "at index " + index + ", " + messageTemplate + " (" + fileName + ":\" + " + lineNumber + " + \")"
  }
  
  class DynamicErrorDetailTemplate(fileName: String, lineNumber: String, messageTemplate: Template, formatParams: String) extends Template {
    override def toString = 
      "\" + new java.text.MessageFormat(\"at index {0}, " + messageTemplate + " (" + fileName + ":\" + " + lineNumber + " + \")\"" + ").format(" + formatParams + ") + \""
  }
  
  class DynamicFirstIndexErrorDetailTemplate(colType: String, errorFun: String, errorValue: String, fileName: String, lineNumber: String, messageTemplate: Template) extends 
    ErrorDetailTemplate("\" + getIndex(xs, " + errorFun + "(xs, " + errorValue + ")) + \"", fileName, lineNumber, messageTemplate)
  
  class DynamicFirstElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      if (colType == "String")
        "\\\"\" + " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ") + \"\\\""
      else
        "\" + " + errorFun + "(xs, " + errorValue + ") + \""
  }
  
  class DynamicFirstArrayElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      "\" + " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ").deep + \""
  }
  
  class DynamicFirstElementLengthTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      "\" + " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ").length + \""
  }
  
  class DynamicNextIndexErrorDetailTemplate(errorValue: String, fileName: String, lineNumber: String, messageTemplate: Template, messageValuesFunName: String) extends 
    DynamicErrorDetailTemplate(fileName, lineNumber, messageTemplate, messageValuesFunName + "(itr, xs, " + errorValue + ")")  
  
  class DynamicNextElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      if (colType == "String")
        "\\\"\" + " + errorFun + "(itr, " + errorValue + ") + \"\\\""
      else
        "\" + " + errorFun + "(itr, " + errorValue + ") + \""
  }
  
  class DynamicNextArrayElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      "\" + " + errorFun + "[" + colType + "](itr, " + errorValue + ").deep + \""
  }
  
  class DynamicNextElementLengthTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString = 
      "\" + " + errorFun + "[" + colType + "](itr, " + errorValue + ").length + \""
  }
  
  trait ErrorMessageTemplate extends Template {
    val header: String
    val xsName: String = "xs"
    override protected def childrenContent = 
      children.map(_.toString.split("\n").map("  " + _).mkString("\n")).mkString(", \\n\" + \n") + " \\n\" + \n"
    override def toString = 
      header + 
      childrenContent + 
      "in \" + " + xsName + ""
  }
  
  class ForAllErrMsgTemplate(headerFailedPrefix: String, detail: ErrorDetailTemplate) extends ErrorMessageTemplate {
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
      if (xsName.startsWith("\"WrappedArray(")) {
        val elements = xsName.substring(1, xsName.length - 2).substring(13)
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
      headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block at \" + failEarlySucceededIndexes" + getErrorMessageValuesFunName(colType, okFun) + "(" + extractXsName + ", " + errorValue + ", " + maxSucceed + ") + \" in \" + " + xsName
  }
  
  class ForExactlyErrMsgTemplate(headerFailedPrefix: String, elementText: String, okFun: String, errorFun: String, errorValue: String, colType: String, details: List[Template]) extends ErrorMessageTemplate {
    val header = headerFailedPrefix + " failed, because " + elementText + " satisfied the assertion block" + (if (elementText == "no element") "" else " at \" + succeededIndexes" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + ") + \"") + ": \\n\" + " + "\n"
    override val children = details
  }
  
  class ForNoErrMsgTemplate(headerFailedPrefix: String, index: String) extends Template {
    val xsName: String = "xs"
    override def toString = 
      headerFailedPrefix + " failed, because 1 element satisfied the assertion block at index " + index + " in \" + " + xsName
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
  
  class NestedFailedTemplate(colText: GenTraversable[_], forType: String, forText: String, name: String, text: String, assertText: String, messageTemplate: Template) extends Template {
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
  
  class InspectorShorthandsSucceedTemplate(name: String, condition: String, assertText: String) extends Template {
    override def toString = 
      "def `" + name + " should succeed when " + condition + "` {\n" + 
      "  " + assertText + "\n" + 
      "}\n"
  }
  
  class InspectorShorthandsForAllErrorTemplateWithCause(
          colText: String, condition: String, assertText: String, 
          fileName: String, colType: String, errorFun: String, 
          errorValue: String, causeErrMsg: String, xsText: String) extends Template {
    
    val causeErrorMessage = new SimpleMessageTemplate(causeErrMsg)
    val errorMessage = new ForAllErrMsgTemplate("'all' inspection", new DynamicFirstIndexErrorDetailTemplate(colType, getErrorMessageValuesFunName(colType, errorFun), errorValue, fileName, "assertLineNumber", causeErrorMessage)) {
      override val xsName: String = xsText
    }
    val testName = colText + " should throw TestFailedException with correct stack depth and message when " + condition
    
    override def toString = 
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" + 
      "  val xs = " + colText + "\n" +
      "  val e = intercept[exceptions.TestFailedException] {\n" + 
      "    " + assertText + "\n" + 
      "  }\n" + 
      "  val assertLineNumber = thisLineNumber - 2\n" + 
      "  checkErrorAndCause(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ", \"" + causeErrorMessage + "\")\n" +
      "}\n"
  }
  
  class ElementTemplate(count: Int) extends Template {
    override def toString = 
      if (count == 0)
        "no element"
      else if (count == 1)
        "1 element"
      else
        count + " elements"
  }
  
  class InspectorShorthandsForAtLeastErrorTemplate(
          colText: String, condition: String, assertText: String, 
          fileName: String, colType: String, errorFun: String, errorValue: String, 
          min: Int, totalCount: Int, passedCount: Int, detailErrorMessage: String, 
          xsText: String) extends Template {
    
    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun)) }
    val errorMessage = new ForAtLeastErrMsgTemplate("'atLeast(" + min + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, details)  {
      override val xsName: String = xsText
    }
    
    override def toString = 
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
      "  val xs = " + colText + "\n" +
      "  val itr = xs.toIterator\n" + 
      "  val e = intercept[exceptions.TestFailedException] {\n" + 
      "    " + assertText + "\n" + 
      "  }\n" + 
      "  val assertLineNumber = thisLineNumber - 2\n" + 
      "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" + 
      "}\n"
    
  }
  
  class InspectorShorthandsForEveryErrorTemplate(
          colText: String, condition: String, assertText: String, 
          fileName: String, colType: String, errorFun: String, errorValue: String, 
          totalCount: Int, passedCount: Int, detailErrorMessage: String,
          xsText: String) extends Template {
    
    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun)) }
    val errorMessage = new ForEveryErrMsgTemplate("'every' inspection", details)  {
      override val xsName: String = xsText
    }
    
    override def toString = 
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
      "  val xs = " + colText + "\n" +
      "  val itr = xs.toIterator\n" + 
      "  val e = intercept[exceptions.TestFailedException] {\n" + 
      "    " + assertText + "\n" + 
      "  }\n" + 
      "  val assertLineNumber = thisLineNumber - 2\n" + 
      "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" + 
      "}\n"
    
  }
  
  class InspectorShorthandsForExactlyErrorTemplate(
          colText: String, condition: String, assertText: String, 
          fileName: String, colType: String, okFun: String, errorFun: String, errorValue: String, 
          count: Int, totalCount: Int, passedCount: Int, detailErrorMessage: String, 
          xsText: String) extends Template {
    
    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun)) }
    val errorMessage = new ForExactlyErrMsgTemplate("'exactly(" + count + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType, details)  {
      override val xsName: String = xsText
    }
    
    override def toString = 
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
      "  val xs = " + colText + "\n" +
      "  val itr = xs.toIterator\n" + 
      "  val e = intercept[exceptions.TestFailedException] {\n" + 
      "    " + assertText + "\n" + 
      "  }\n" + 
      "  val assertLineNumber = thisLineNumber - 2\n" + 
      "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" + 
      "}\n"
    
  }

  class InspectorShorthandsForNoErrorTemplate(colText: String, condition: String, assertText: String,
                                             fileName: String, colType: String, okFun: String, errorFun: String, errorValue: String, 
                                             xsText: String) extends Template {

    val errorMessage = new ForNoErrMsgTemplate("'no' inspection", "\" + getIndex(xs, getFirst" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + ")) + \"")  {
      override val xsName: String = xsText
    }

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs.toIterator\n" +
        "  val e = intercept[exceptions.TestFailedException] {\n" +
        "    " + assertText + "\n" +
        "  }\n" +
        "  val assertLineNumber = thisLineNumber - 2\n" +
        "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" +
        "}\n"

  }

  class InspectorShorthandsForBetweenErrorTemplate(colText: String, condition: String, assertText: String,
                                             fileName: String, colType: String, okFun: String, errorFun: String, errorValue: String, 
                                             from: Int, upTo: Int, totalCount: Int, passedCount: Int, detailErrorMessage: String,
                                             xsText: String) extends Template {

    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun)) }
    val errorMessage = new ForBetweenLessErrMsgTemplate("'between(" + from + ", " + upTo + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType, details)  {
      override val xsName: String = xsText
    }

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs.toIterator\n" +
        "  val e = intercept[exceptions.TestFailedException] {\n" +
        "    " + assertText + "\n" +
        "  }\n" +
        "  val assertLineNumber = thisLineNumber - 2\n" +
        "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" +
        "}\n"

  }

  class InspectorShorthandsForAtMostErrorTemplate(colText: String, condition: String, assertText: String,
                                                 fileName: String, colType: String, okFun: String, errorFun: String, errorValue: String, 
                                                 max: Int, passedCount: Int, detailErrorMessage: String,
                                                 xsText: String) extends Template {

    val errorMessage = new ForAtMostErrMsgTemplate("'atMost(" + max + ")' inspection", max, new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType)  {
      override val xsName: String = xsText
    }

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs.toIterator\n" +
        "  val e = intercept[exceptions.TestFailedException] {\n" +
        "    " + assertText + "\n" +
        "  }\n" +
        "  val assertLineNumber = thisLineNumber - 2\n" +
        "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" +
        "}\n"

  }
  
  class JavaColHelperMethodTemplate(methodName: String, javaColType: String) extends Template {
    override def toString = 
      "def " + methodName + "[T](valueList: List[T]): " + javaColType + "[T] = {\n" + 
      "  val col = new " + javaColType + "[T]()\n" + 
      "  for (value <- valueList)\n" + 
      "    col.add(value)\n" +
      "  col\n" +
      "}\n"
  }
  
  class JavaColHelperMethodWithSizeTemplate(methodName: String, javaColType: String) extends Template {
    override def toString = 
      "def " + methodName + "[T](valueList: List[T], size: Int): " + javaColType + "[T] = {\n" + 
      "  val col = new " + javaColType + "[T](size)\n" + 
      "  for (value <- valueList)\n" + 
      "    col.add(value)\n" +
      "  col\n" +
      "}\n"
  }
  
  class JavaMapHelperMethodTemplate(methodName: String, javaMapType: String) extends Template {
    override def toString = 
      "def " + methodName + "[K, V](valueMap: Map[K, V]): " + javaMapType + "[K, V] = {\n" + 
      "  val map = new " + javaMapType + "[K, V]()\n" + 
      "  for ((key, value) <- valueMap)\n" + 
      "    map.put(key, value)\n" +
      "  map\n" +
      "}\n"
  }
  
  class TypeParameterlessJavaMapHelperMethodTemplate(methodName: String, javaMapType: String) extends Template {
    override def toString = 
      "def " + methodName + "(valueMap: Map[String, String]): " + javaMapType + " = {\n" + 
      "  val map = new " + javaMapType + "()\n" + 
      "  for ((key, value) <- valueMap)\n" + 
      "    map.put(key, value)\n" +
      "  map\n" +
      "}\n"
  }
  
  class InspectorShorthandsHelpersTemplate extends Template {
    override def toString = 
      "class EmptyBePropertyMatcher extends BePropertyMatcher[String] {\n" + 
      "  def apply(left: String) = BePropertyMatchResult(left.isEmpty, \"empty\")\n" + 
      "}\n" + 
      "class StringLengthMatcher(expectedValue: Int) extends HavePropertyMatcher[String, Int] {\n" + 
      "  def apply(value: String) = {\n" + 
      "    new HavePropertyMatchResult(value.length == expectedValue, \"length\", expectedValue, value.length)\n" + 
      "  }\n" + 
      "}\n" + 
      "val empty = new EmptyBePropertyMatcher()\n" + 
      "def plength(expectedValue: Int) = new StringLengthMatcher(expectedValue)\n" + 
      "val theInstance = \"2\"\n" + 
      "def arrayToString(xs: GenTraversable[_]): String = xs.toString\n" +  // pass in array so that it is implicit converted
      "def checkErrorAndCause(e: exceptions.TestFailedException, assertLineNumber: Int, fileName: String, errorMessage: String, causeErrorMessage: String) {\n" +
      "  assert(e.failedCodeFileName == Some(fileName), e.failedCodeFileName + \" did not equal \" + Some(fileName))\n" + 
      "  assert(e.failedCodeLineNumber == Some(assertLineNumber), e.failedCodeLineNumber + \" did not equal \" + Some(assertLineNumber))\n" +
      "  assert(e.message == Some(errorMessage), e.message + \" did not equal \" + Some(\nerrorMessage))\n" + 
      "  e.getCause match {\n" + 
      "    case tfe: exceptions.TestFailedException =>\n" + 
      "      assert(tfe.failedCodeFileName == Some(fileName), tfe.failedCodeFileName + \" did not equal \" + Some(fileName))\n" + 
      "      assert(tfe.failedCodeLineNumber == Some(assertLineNumber), tfe.failedCodeLineNumber + \" did not equal \" + Some(assertLineNumber))\n" +
      "      assert(tfe.message == Some(causeErrorMessage), tfe.message + \" did not equal \" + Some(causeErrorMessage))\n" + 
      "      assert(tfe.getCause == null, tfe.getCause + \" was not null\")\n" + 
      "    case other => fail(\"Expected cause to be TestFailedException, but got: \" + other)\n" + 
      "  }\n" + 
      "}\n" +
      "def checkError(e: exceptions.TestFailedException, assertLineNumber: Int, fileName: String, errorMessage: String) {\n" +
      "  assert(e.failedCodeFileName == Some(fileName), e.failedCodeFileName + \" did not equal \" + Some(fileName))\n" + 
      "  assert(e.failedCodeLineNumber == Some(assertLineNumber), e.failedCodeLineNumber + \" did not equal \" + Some(assertLineNumber))\n" +
      "  assert(e.message == Some(errorMessage), e.message + \" did not equal \" + Some(\nerrorMessage))\n" + 
      "}\n" + 
      "def equal[T](left: T, right: T): Boolean = left == right\n" + 
      /*new SucceededIndexesHelperMethodTemplate +
      new FailEarlySucceededIndexesHelperMethodTemplate +*/
      new JavaColHelperMethodTemplate("javaArrayList", "java.util.ArrayList") + 
      new JavaColHelperMethodTemplate("javaHashSet", "java.util.HashSet") + 
      new JavaColHelperMethodTemplate("javaLinkedList", "java.util.LinkedList") + 
      new JavaColHelperMethodTemplate("javaStack", "java.util.Stack") + 
      new JavaColHelperMethodTemplate("javaTreeSet", "java.util.TreeSet") + 
      new JavaColHelperMethodTemplate("javaVector", "java.util.Vector") + 
      new JavaColHelperMethodWithSizeTemplate("javaArrayBlockingQueue", "java.util.concurrent.ArrayBlockingQueue") + 
      new JavaColHelperMethodTemplate("javaArrayDeque", "java.util.ArrayDeque") + 
      new JavaColHelperMethodTemplate("javaConcurrentLinkedQueue", "java.util.concurrent.ConcurrentLinkedQueue") + 
      new JavaColHelperMethodTemplate("javaConcurrentSkipListSet", "java.util.concurrent.ConcurrentSkipListSet") + 
      new JavaColHelperMethodTemplate("javaCopyOnWriteArrayList", "java.util.concurrent.CopyOnWriteArrayList") + 
      new JavaColHelperMethodTemplate("javaCopyOnWriteArraySet", "java.util.concurrent.CopyOnWriteArraySet") + 
      new JavaColHelperMethodTemplate("javaLinkedBlockingDeque", "java.util.concurrent.LinkedBlockingDeque") + 
      new JavaColHelperMethodTemplate("javaLinkedBlockingQueue", "java.util.concurrent.LinkedBlockingQueue") + 
      new JavaColHelperMethodTemplate("javaLinkedHashSet", "java.util.LinkedHashSet") + 
      new JavaColHelperMethodTemplate("javaPriorityBlockingQueue", "java.util.concurrent.PriorityBlockingQueue") + 
      new JavaColHelperMethodTemplate("javaPriorityQueue", "java.util.PriorityQueue") + 
      new JavaMapHelperMethodTemplate("javaHashMap", "java.util.HashMap") + 
      new JavaMapHelperMethodTemplate("javaTreeMap", "java.util.TreeMap") + 
      new JavaMapHelperMethodTemplate("javaHashtable", "java.util.Hashtable") + 
      new JavaMapHelperMethodTemplate("javaConcurrentHashMap", "java.util.concurrent.ConcurrentHashMap") + 
      new JavaMapHelperMethodTemplate("javaConcurrentSkipListMap", "java.util.concurrent.ConcurrentSkipListMap") + 
      new JavaMapHelperMethodTemplate("javaLinkedHashMap", "java.util.LinkedHashMap") + 
      new JavaMapHelperMethodTemplate("javaWeakHashMap", "java.util.WeakHashMap") + 
      new TypeParameterlessJavaMapHelperMethodTemplate("javaProperties", "java.util.Properties")
  }
  
  class EmptyTextMessageTemplate extends Template {
    override def toString = "empty"
  }
  
  val empty = new EmptyTextMessageTemplate
  
  // Helper methods
  
  @tailrec
  def buildList[T](size: Int, element: T, list: List[T] = List.empty[T]): List[T] = 
    if (list.size < size) {
      buildList(size, element, element :: list)
    }
    else
      list
  
  def splitMultilineErrorMessage(errorMessage: String) = 
    errorMessage.toString.split("\n").map("\"" + _).mkString("\n")
  
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
        new ForNoErrMsgTemplate("forNo", "0") {
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
    val errorMessage = new SimpleMessageTemplate("0 was not equal to 1")
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
      case "forNo" => new ForNoErrMsgTemplate("forNo", "0")
      case "forBetween" => new ForBetweenLessErrMsgTemplate("forBetween(2, 4)", "no element", "NotEqualBoolean", "EqualBoolean", "false", "List[Int]", innerDetails.toList)
      case "forEvery" => new ForEveryErrMsgTemplate("forEvery", innerDetails.toList)
    }
  }
  
  def genNestedInspectorsSpecFile(targetDir: File) {
    val nestedInspectorsSpecFile = new File(targetDir, "NestedInspectorsSpec.scala")
    genFile(
      nestedInspectorsSpecFile, 
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.nested"), 
        importList = List("org.scalatest._", 
                          "collection.GenTraversable"), 
        classTemplate = new ClassTemplate {
          val name = "NestedInspectorsSpec"
          override val extendName = Some("Spec")
          override val withList = List("Inspectors", "SharedHelpers")
          override val children = {
            
            val succeededAssertion = "assert(n % 2 == 0, n % 2 + \" was not equal to 0\")"
            val failedAssertion = "assert(n % 2 == 1, n % 2 + \" was not equal to 1\")"
            
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
  
  def genCol[T](colText: String, arrayXsText: String) = 
    List[(String, String)](
      ("Set(" + colText + ")", "xs"), 
      ("List(" + colText + ")", "xs"), 
      ("Seq(" + colText + ")", "xs"), 
      ("Array(" + colText + ")", arrayXsText), 
      ("IndexedSeq(" + colText + ")", "xs"), 
      ("Vector(" + colText + ")", "xs"), 
      ("Set(" + colText + ").par", "xs"), 
      ("List(" + colText + ").par", "xs"), 
      ("Seq(" + colText + ").par", "xs"), 
      ("IndexedSeq(" + colText + ").par", "xs"), 
      ("collection.mutable.Set(" + colText + ")", "xs"), 
      ("new collection.mutable.ListBuffer() ++ List(" + colText + ")", "xs"), 
      ("collection.mutable.Seq(" + colText + ")", "xs"), 
      ("collection.mutable.IndexedSeq(" + colText + ")", "xs"), 
      ("collection.mutable.Set(" + colText + ").par", "xs"), 
      ("(new collection.mutable.ListBuffer() ++ List(" + colText + ")).par", "xs"),  
      ("collection.mutable.Seq(" + colText + ").par", "xs"), 
      ("collection.mutable.IndexedSeq(" + colText + ").par", "xs")
    )
    
  def genNullableCol[T](colText: String, arrayXsText: String) = 
    List[(String, String)](
      ("Set(" + colText + ")", "xs"), 
      ("List(" + colText + ")", "xs"), 
      ("Seq(" + colText + ")", "xs"), 
      ("Array(" + colText + ")", arrayXsText), 
      ("IndexedSeq(" + colText + ")", "xs"), 
      ("Vector(" + colText + ")", "xs"), 
      ("Set(" + colText + ").par", "xs"), 
      ("List(" + colText + ").par", "xs"), 
      ("Seq(" + colText + ").par", "xs"), 
      ("IndexedSeq(" + colText + ").par", "xs"), 
      ("new collection.mutable.ListBuffer() ++ List(" + colText + ")", "xs"), 
      ("collection.mutable.Seq(" + colText + ")", "xs"), 
      ("collection.mutable.IndexedSeq(" + colText + ")", "xs"), 
      ("(new collection.mutable.ListBuffer() ++ List(" + colText + ")).par", "xs"),  
      ("collection.mutable.Seq(" + colText + ").par", "xs"), 
      ("collection.mutable.IndexedSeq(" + colText + ").par", "xs")
    )
    
  def genColCol[T](colType: String, colTexts: Array[String], arrayXsText: String) = 
    List[(String, String)](
      ("Set(" + colTexts.map("Set(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("List(" + colTexts.map("List(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("Seq(" + colTexts.map("Seq(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("Array[Array[" + colType + "]](" + colTexts.map("Array[" + colType + "](" + _ + ")").mkString(", ") + ")", "arrayToString(xs)"), 
      ("IndexedSeq(" + colTexts.map("IndexedSeq(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("Vector(" + colTexts.map("Vector(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("Set(" + colTexts.map("Set(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("List(" + colTexts.map("List(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("Seq(" + colTexts.map("Seq(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("IndexedSeq(" + colTexts.map("IndexedSeq(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("collection.mutable.Set(" + colTexts.map("collection.mutable.Set(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("new collection.mutable.ListBuffer() ++ List(" + colTexts.map("new collection.mutable.ListBuffer() ++ List(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("collection.mutable.Seq(" + colTexts.map("collection.mutable.Seq(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("collection.mutable.IndexedSeq(" + colTexts.map("collection.mutable.IndexedSeq(" + _ + ")").mkString(", ") + ")", "xs"), 
      ("collection.mutable.Set(" + colTexts.map("collection.mutable.Set(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("(new collection.mutable.ListBuffer() ++ List(" + colTexts.map("(new collection.mutable.ListBuffer() ++ List(" + _ + ")).par").mkString(", ") + ")).par", "xs"),  
      ("collection.mutable.Seq(" + colTexts.map("collection.mutable.Seq(" + _ + ").par").mkString(", ") + ").par", "xs"), 
      ("collection.mutable.IndexedSeq(" + colTexts.map("collection.mutable.IndexedSeq(" + _ + ").par").mkString(", ") + ").par", "xs")
    )
    
  def genMap[T](colTexts: Array[String]) = 
    List[(String, String)](
      (colTexts.map("Map(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("collection.mutable.Map(" + _ + ")").mkString(", "), "xs"), 
      (colTexts.map("collection.immutable.HashMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.mutable.HashMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.mutable.LinkedHashMap(" + _ + ")").mkString(", "), "xs"), 
      (colTexts.map("collection.immutable.ListMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.mutable.ListMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.mutable.OpenHashMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.immutable.SortedMap(" + _ + ")").mkString(", "), "xs"),
      (colTexts.map("collection.immutable.TreeMap(" + _ + ")").mkString(", "), "xs")
    )
    
  def genColMap[T](mapText: String, arrayXsText: String) = 
    List[(String, String)](
      ("Set(" + mapText + ")", "xs"), 
      ("List(" + mapText + ")", "xs"), 
      ("Seq(" + mapText + ")", "xs"), 
      ("Array(" + mapText + ")", "arrayToString(xs)"), 
      ("IndexedSeq(" + mapText + ")", "xs"), 
      ("Vector(" + mapText + ")", "xs"), 
      ("Set(" + mapText + ").par", "xs"), 
      ("List(" + mapText + ").par", "xs"), 
      ("Seq(" + mapText + ").par", "xs"), 
      ("IndexedSeq(" + mapText + ").par", "xs"), 
      ("collection.mutable.Set(" + mapText + ")", "xs"), 
      ("new collection.mutable.ListBuffer() ++ List(" + mapText + ")", "xs"), 
      ("collection.mutable.Seq(" + mapText + ")", "xs"), 
      ("collection.mutable.IndexedSeq(" + mapText + ")", "xs"), 
      ("collection.mutable.Set(" + mapText + ").par", "xs"), 
      ("(new collection.mutable.ListBuffer() ++ List(" + mapText + ")).par", "xs"),  
      ("collection.mutable.Seq(" + mapText + ").par", "xs"), 
      ("collection.mutable.IndexedSeq(" + mapText + ").par", "xs")
    )
    
  def genJavaCol[T](colTexts: Array[String]) = 
    List[(String, String)](
      (colTexts.map("javaArrayList(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaHashSet(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaLinkedList(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaStack(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaTreeSet(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaVector(" + _ + ")").mkString(", ") , "xs"), 
      (colTexts.map("javaArrayBlockingQueue(" + _ + ", " + colTexts.length + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaArrayDeque(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaConcurrentLinkedQueue(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaConcurrentSkipListSet(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaCopyOnWriteArrayList(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaCopyOnWriteArraySet(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaLinkedBlockingDeque(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaLinkedBlockingQueue(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaLinkedHashSet(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaPriorityBlockingQueue(" + _ + ")").mkString(", ") , "xs"),  
      (colTexts.map("javaPriorityQueue(" + _ + ")").mkString(", ") , "xs")
    )
    
  def genJavaMap[T](colTexts: Array[String], keyType: String, valueType: String) = 
    List[(String, String, String)](
      (colTexts.map("javaHashMap(" + _ + ")").mkString(", ") , "xs", "java.util.HashMap[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaTreeMap(" + _ + ")").mkString(", ") , "xs", "java.util.TreeMap[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaHashtable(" + _ + ")").mkString(", ") , "xs", "java.util.Hashtable[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaConcurrentHashMap(" + _ + ")").mkString(", ") , "xs", "java.util.concurrent.ConcurrentHashMap[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaConcurrentSkipListMap(" + _ + ")").mkString(", ") , "xs", "java.util.concurrent.ConcurrentSkipListMap[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaLinkedHashMap(" + _ + ")").mkString(", ") , "xs", "java.util.LinkedHashMap[" + keyType + ", " + valueType + "]"), 
      (colTexts.map("javaWeakHashMap(" + _ + ")").mkString(", ") , "xs", "java.util.WeakHashMap[" + keyType + ", " + valueType + "]")/*, 
      (colTexts.map("javaProperties(" + _ + ")").mkString(", ") , "xs", "java.util.Properties")*/
    )
  
  def getFun(funString: String, right: Int): Int => Boolean = {
    funString match {
      case "indexElementNotEqual[Int]" => _ != right
      case "indexElementEqual[Int]" => _ == right
      case "indexElementNotEqual[String]" => _ != right
      case "indexElementEqual[String]" => _ == right
      case "indexElementLessThanEqual" => _ <= right
      case "indexElementLessThan" => _ < right
      case "indexElementMoreThanEqual" => _ >= right
      case "indexElementMoreThan" => _ > right
    }
  }
  
  def getSizeFun(funString: String, right: Int): String => Boolean = {
    funString match {
      case "indexElementSizeEqual[String]" => _.size == right
      case "indexElementSizeNotEqual[String]" => _.size != right
      case "indexElementSizeEqualGenTraversable[String]" => _.size == right
      case "indexElementSizeNotEqualGenTraversable[String]" => _.size != right
    }
  }
  
  def getFun(funString: String, right: String): String => Boolean = {
    funString match {
      case "indexElementNotEqual[String]" => _ != right
      case "indexElementEqual[String]" => _ == right
      case "indexElementNotRefEqual[String]" => _ != right
      case "indexElementRefEqual[String]" => _ == right
      case "indexElementIsEmpty" => _.isEmpty
      case "indexElementIsNotEmpty" => !_.isEmpty
      case "indexElementLengthEqual" => _.length == right.length
      case "indexElementLengthNotEqual" => _.length != right.length
      case "indexElementLengthNotEqualLength" => _.length != right.length
      case "indexElementSizeEqual" => _.size == right.size
      case "indexElementSizeNotEqual" => _.size != right.size
      case "indexElementStartsWith" => _.startsWith(right)
      case "indexElementNotStartsWith" => !_.startsWith(right) 
      case "indexElementEndsWith" => _.endsWith(right)
      case "indexElementNotEndsWith" => !_.endsWith(right) 
      case "indexElementInclude" => _.indexOf(right) >= 0
      case "indexElementNotInclude" => _.indexOf(right) < 0
      case "indexElementMatches" => _.matches(right)
      case "indexElementNotMatches" => !_.matches(right)
    }
  }
  
  def getTraversableFun(funString: String, right: Any): List[String] => Boolean = {
    funString match {
      case "indexElementSizeEqualGenTraversable[String]" => _.size == right
      case "indexElementSizeNotEqualGenTraversable[String]" => _.size != right
      //case s: String if s.startsWith("_.exists") => _.exists(_ == right)
      //case s: String if s.startsWith("!_.exists") => !_.exists(_ == right)
      case "indexElementContainGenTraversable[String]" => _.contains(right)
      case "indexElementNotContainGenTraversable[String]" => !_.contains(right)
    }
  }
  
  def getMapFun(funString: String, right: Any): Map[String, String] => Boolean = {
    funString match {
      case "indexElementContainKey[String, String]" => _.exists(_._1 == right)
      case "indexElementNotContainKey[String, String]" => !_.exists(_._1 == right)
      case "indexElementContainValue[String, String]" => _.exists(_._2 == right)
      case "indexElementNotContainValue[String, String]" => !_.exists(_._2 == right)
    }
  }
  
  def getJavaColFun(funString: String, right: Any): List[String] => Boolean = {
    funString match {
      case "indexElementJavaColSizeEqual[String]" => _.size == right
      case "indexElementJavaColSizeNotEqual[String]" => _.size != right
      case "indexElementJavaColIsEmpty[String]" => _.isEmpty
      case "indexElementJavaColNotIsEmpty[String]" => !_.isEmpty
      case "indexElementJavaColContain[String]" => _.contains(right)
      case "indexElementJavaColNotContain[String]" => !_.contains(right)
    }
  }
  
  def getJavaMapFun(funString: String, right: Any): java.util.Map[String, String] => Boolean = {
    funString match {
      case "indexElementJavaMapIsEmpty[String, String]" => _.isEmpty
      case "indexElementJavaMapNotIsEmpty[String, String]" => !_.isEmpty
      case "indexElementJavaMapSizeEqual[String, String]" => _.size == right
      case "indexElementJavaMapSizeNotEqual[String, String]" => _.size != right
      case "indexElementJavaMapContainKey[String, String]" => _.containsKey(right)
      case "indexElementJavaMapNotContainKey[String, String]" => !_.containsKey(right)
      case "indexElementJavaMapContainValue[String, String]" => _.containsValue(right)
      case "indexElementJavaMapNotContainValue[String, String]" => !_.containsValue(right)
    }
  }
    
  def stdInt123Types(leftTemplateFun: (String, String) => Template, right: Int, errorFunPrefix: String, autoQuoteString: Boolean = true) = 
    List(
      ("'should equal' failed", " should equal (" + right + ")", "Equal[Int]", errorFunPrefix + "NotEqual[Int]", "" + right, (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should not equal' failed", " should not equal " + right, "NotEqual[Int]", errorFunPrefix + "Equal[Int]", "" + right, (errorFun: String, errorValue: String) => new EqualedMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),  
      ("'should be' failed", " should be (" + right + ")", "Equal[Int]", errorFunPrefix + "NotEqual[Int]", "" + right, (errorFun: String, errorValue: String) => new WasNotEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should not be' failed", " should not be " + right, "NotEqual[Int]", errorFunPrefix + "Equal[Int]", "" + right, (errorFun: String, errorValue: String) => new WasEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should be less than comparison' failed", " should be < " + right, "LessThan", errorFunPrefix + "MoreThanEqual", "" + right, (errorFun: String, errorValue: String) => new WasNotLessThanMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not be less than comparison' failed", " should not be < (" + right + ")", "MoreThanEqual", errorFunPrefix + "LessThan", "" + right, (errorFun: String, errorValue: String) => new WasLessThanMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should be less than or equal comparison' failed", " should be <= " + right, "LessThanEqual", errorFunPrefix + "MoreThan", "" + right, (errorFun: String, errorValue: String) => new WasNotLessThanOrEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not be less than or equal comparison' failed", " should not be <= (" + right + ")", "MoreThan", errorFunPrefix + "LessThanEqual", "" + right, (errorFun: String, errorValue: String) => new WasLessThanOrEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should be greater than comparison' failed", " should be > " + right, "MoreThan", errorFunPrefix + "LessThanEqual", "" + right, (errorFun: String, errorValue: String) => new WasNotGreaterThanMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not be greater than comparison' failed", " should not be > (" + right + ")", "LessThanEqual", errorFunPrefix + "MoreThan", "" + right, (errorFun: String, errorValue: String) => new WasGreaterThanMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should be greater than or equal comparison' failed", " should be >= " + right, "MoreThanEqual", errorFunPrefix + "LessThan", "" + right, (errorFun: String, errorValue: String) => new WasNotGreaterThanOrEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should not be greater than or equal comparison' failed", " should not be >= (" + right + ")", "LessThan", errorFunPrefix + "MoreThanEqual", "" + right, (errorFun: String, errorValue: String) => new WasGreaterThanOrEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should be ===' failed", " should be === " + right, "Equal[Int]", errorFunPrefix + "NotEqual[Int]", "" + right, (errorFun: String, errorValue: String) => new WasNotEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)), 
      ("'should not be ===' failed", " should not be === (" + right + ")", "NotEqual[Int]", errorFunPrefix + "Equal[Int]", "" + right, (errorFun: String, errorValue: String) => new WasEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString))
    )
    
  def stdNullStringTypes(leftTemplateFun: (String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) = 
    List(
      ("'should be null' failed", " should be (null)", "Equal[String]", errorFunPrefix + "NotEqual[String]", "null", (errorFun: String, errorValue: String) => new WasNotNullMessageTemplate(leftTemplateFun(errorFun, errorValue), autoQuoteString)), 
      ("'should not be null' failed", " should not be null", "NotEqual[String]", errorFunPrefix + "Equal[String]", "null", (errorFun: String, errorValue: String) => new WasNullMessageTemplate)
    )
    
  def stdPropertyCheckTypes(expectedLengthSize:Int, leftTemplateFun: (String, String) => Template, targetTemplateFun: (String, String) => Template, leftLengthTemplateFun: (String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'should be symbol' failed", " should be ('empty)", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should not be symbol' failed", " should not be 'empty", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should be property' failed", " should be (empty)", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be property' failed", " should not be empty", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should be a symbol' failed", " should be a 'empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should not be a symbol' failed", " should not be a ('empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should be a property' failed", " should be a empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should not be a property' failed", " should not be a (empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should be an symbol' failed", " should be an 'empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be an symbol' failed", " should not be an ('empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should be an property' failed", " should be an empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should not be an property' failed", " should not be an (empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)), 
      ("'should have property' failed", " should have (plength(" + expectedLengthSize + "))", "LengthEqual", errorFunPrefix + "LengthNotEqualLength", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new PropertyHadUnexpectedValueMessageTemplate("length", 0, leftLengthTemplateFun(errorFun, errorValue), targetTemplateFun(errorFun, errorValue), autoQuoteString)),
      ("'should not have property' failed", " should not have plength(" + expectedLengthSize + ")", "LengthNotEqual", errorFunPrefix + "LengthEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new PropertyHadExpectedValueMessageTemplate("length", 0, leftTemplateFun(errorFun, errorValue), autoQuoteString)),
      ("'should have length' failed", " should have length " + expectedLengthSize, "LengthEqual", errorFunPrefix + "LengthNotEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new DidNotHaveLengthMessageTemplate(leftTemplateFun(errorFun, errorValue), 0, autoQuoteString)),
      ("'should not have length' failed", " should not have length (" + expectedLengthSize + ")", "LengthNotEqual", errorFunPrefix + "LengthEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(errorFun, errorValue), 0, autoQuoteString)),
      ("'should have size' failed", " should have size " + expectedLengthSize, "SizeEqual", errorFunPrefix + "SizeNotEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new DidNotHaveSizeMessageTemplate(leftTemplateFun(errorFun, errorValue), 0, autoQuoteString)), 
      ("'should not have size' failed", " should not have size (" + expectedLengthSize + ")", "SizeNotEqual", errorFunPrefix + "SizeEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(errorFun, errorValue), 0, autoQuoteString))
    )
    
  def stdInstanceCheckTypes(leftTemplateFun: (String, String) => Template, right: Any, errorFunPrefix: String, autoQuoteString: Boolean = true) =  
    List(
      ("'should be theSameInstanceAs' failed", " should be theSameInstanceAs theInstance", "RefEqual[String]", errorFunPrefix + "NotRefEqual[String]", "theInstance", (errorFun: String, errorValue: String) => new WasNotTheSameInstanceAsMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not be theSameInstanceAs' failed", " should not be theSameInstanceAs (theInstance)", "NotRefEqual[String]", errorFunPrefix + "RefEqual[String]", "theInstance", (errorFun: String, errorValue: String) => new WasTheSameInstanceAsMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString))
    )
    
  def stdStringCheckTypes(leftTemplateFun: (String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) = 
    List(
      ("'should startWith' failed", " should startWith (\"hello\")", "StartsWith", errorFunPrefix + "NotStartsWith", "hello", (errorFun: String, errorValue: String) => new DidNotStartWithSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "hello", autoQuoteString)),
      ("'should not startWith' failed", " should not startWith (\"hello\")", "NotStartsWith", errorFunPrefix + "StartsWith", "hello", (errorFun: String, errorValue: String) => new StartedWithSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "hello", autoQuoteString)), 
      ("'should endWith' failed", " should endWith (\"!\")", "EndsWith", errorFunPrefix + "NotEndsWith", "!", (errorFun: String, errorValue: String) => new DidNotEndWithSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "!", autoQuoteString)), 
      ("'should not endWith' failed", " should not endWith (\"!\")", "NotEndsWith", errorFunPrefix + "EndsWith", "!", (errorFun: String, errorValue: String) => new EndedWithSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "!", autoQuoteString)),
      ("'should include' failed", " should include (\"ell\")", "Include", errorFunPrefix + "NotInclude", "ell", (errorFun: String, errorValue: String) => new DidNotIncludeSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "ell", autoQuoteString)),
      ("'should not include' failed", " should not include \"ell\"", "NotInclude", errorFunPrefix + "Include", "ell", (errorFun: String, errorValue: String) => new IncludedSubstringMessageTemplate(leftTemplateFun(errorFun, errorValue), "ell", autoQuoteString)),
      ("'should startWith regex' failed", " should startWith regex \"hel*o\"", "StartsWith", errorFunPrefix + "NotStartsWith", "hello", (errorFun: String, errorValue: String) => new DidNotStartWithRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "hel*o".r, autoQuoteString)),
      ("'should not startWith regex' failed", " should not startWith regex (\"hel*o\")", "NotStartsWith", errorFunPrefix + "StartsWith", "hello", (errorFun: String, errorValue: String) => new StartedWithRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "hel*o".r, autoQuoteString)), 
      ("'should endWith regex' failed", " should endWith regex \"!\"", "EndsWith", errorFunPrefix + "NotEndsWith", "!", (errorFun: String, errorValue: String) => new DidNotEndWithRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "!".r, autoQuoteString)), 
      ("'should not endWith regex' failed", " should not endWith regex (\"!\")", "NotEndsWith", errorFunPrefix + "EndsWith", "!", (errorFun: String, errorValue: String) => new EndedWithRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "!".r, autoQuoteString)),
      ("'should include regex' failed", " should include regex \"ell\"", "Include", errorFunPrefix + "NotInclude", "ell", (errorFun: String, errorValue: String) => new DidNotIncludeRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "ell".r, autoQuoteString)), 
      ("'should not include regex' failed", " should not include regex (\"ell\")", "NotInclude", errorFunPrefix + "Include", "ell", (errorFun: String, errorValue: String) => new IncludedRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "ell".r, autoQuoteString)),
      ("'should fullyMatch regex' failed", " should fullyMatch regex \"h.*!\"", "Matches", errorFunPrefix + "NotMatches", "h.*!", (errorFun: String, errorValue: String) => new DidNotFullyMatchRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "h.*!".r, autoQuoteString)),
      ("'should not fullyMatch regex' failed", " should not fullyMatch regex (\"h.*!\")", "NotMatches", errorFunPrefix + "Matches", "h.*!", (errorFun: String, errorValue: String) => new FullyMatchRegexMessageTemplate(leftTemplateFun(errorFun, errorValue), "h.*!".r, autoQuoteString))
    )
    
  def stdTraversablePropertyCheckTypes(leftTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'traversable should be symbol' failed", " should be ('empty)", "SizeEqualGenTraversable[String]", errorFunPrefix + "SizeNotEqualGenTraversable[String]", "0", (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'traversable should not be symbol' failed", " should not be 'empty", "SizeNotEqualGenTraversable[String]", errorFunPrefix + "SizeEqualGenTraversable[String]", "0", (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString))
    )
    
  def stdTraversableCheckTypes(leftTemplateFun: (String, String, String) => Template, size: Int, notSize: Int, containText: String, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'traversable should have size' failed", " should have size " + size, "SizeEqualGenTraversable[String]", errorFunPrefix + "SizeNotEqualGenTraversable[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'traversable should not have size' failed", " should not have size (" + notSize +")", "SizeNotEqualGenTraversable[String]", errorFunPrefix + "SizeEqualGenTraversable[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'traversable should have length' failed", " should have length " + size, "SizeEqualGenTraversable[String]", errorFunPrefix + "SizeNotEqualGenTraversable[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'traversable should not have length' failed", " should not have length (" + notSize + ")", "SizeNotEqualGenTraversable[String]", errorFunPrefix + "SizeEqualGenTraversable[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'traversable should contain' failed", " should contain (\"" + containText + "\")", "ContainGenTraversable[String]", errorFunPrefix + "NotContainGenTraversable[String]", "\"" + containText + "\"", containText, (colType: String, errorFun: String, errorValue: String) => new DidNotContainElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString)),
      ("'traversable should not contain' failed", " should not contain \"" + containText + "\"", "NotContainGenTraversable[String]", errorFunPrefix + "ContainGenTraversable[String]", "\"" + containText + "\"", containText, (colType: String, errorFun: String, errorValue: String) => new ContainedElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString))
    )
    
  def stdMapCheckTypes(leftTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) = 
    List(
      ("'map should contain key' failed", " should contain key \"2\"", "ContainKey[String, String]", errorFunPrefix + "NotContainKey[String, String]", "\"2\"", "2",  (colType: String, errorFun: String, errorValue: String) => new DidNotContainKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "2", autoQuoteString)),
      ("'map should not contain key' failed", " should not contain key (\"2\")", "NotContainKey[String, String]", errorFunPrefix + "ContainKey[String, String]", "\"2\"", "2", (colType: String, errorFun: String, errorValue: String) => new ContainedKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "2", autoQuoteString)), 
      ("'map should contain value' failed", " should contain value \"two\"", "ContainValue[String, String]", errorFunPrefix + "NotContainValue[String, String]", "\"two\"", "two", (colType: String, errorFun: String, errorValue: String) => new DidNotContainValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "two", autoQuoteString)),
      ("'map should not contain value' failed", " should not contain value (\"two\")", "NotContainValue[String, String]", errorFunPrefix + "ContainValue[String, String]", "\"two\"", "two",  (colType: String, errorFun: String, errorValue: String) => new ContainedValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "two", autoQuoteString))
    )
    
  def stdJavaColCheckTypes(size: Int, notSize: Int, leftTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'java collection should be symbol' failed", " should be ('empty)", "JavaColIsEmpty[String]", errorFunPrefix + "JavaColNotIsEmpty[String]", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java collection should not be symbol' failed", " should not be 'empty", "JavaColNotIsEmpty[String]", errorFunPrefix + "JavaColIsEmpty[String]", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)), 
      ("'java collection should have size' failed", " should have size " + size, "JavaColSizeEqual[String]", errorFunPrefix + "JavaColSizeNotEqual[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java collection should not have size' failed", " should not have size (" + notSize + ")", "JavaColSizeNotEqual[String]", errorFunPrefix + "JavaColSizeEqual[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java collection should have length' failed", " should have length " + size, "JavaColSizeEqual[String]", errorFunPrefix + "JavaColSizeNotEqual[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java collection should not have length' failed", " should not have length (" + notSize + ")", "JavaColSizeNotEqual[String]", errorFunPrefix + "JavaColSizeEqual[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java collection should contain' failed", " should contain (\"hi\")", "JavaColContain[String]", errorFunPrefix + "JavaColNotContain[String]", "\"hi\"", "hi", (colType: String, errorFun: String, errorValue: String) => new DidNotContainElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString)),
      ("'java collection should not contain' failed", " should not contain \"hi\"", "JavaColNotContain[String]", errorFunPrefix + "JavaColContain[String]", "\"hi\"", "hi", (colType: String, errorFun: String, errorValue: String) => new ContainedElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString))
    )
    
  def stdJavaMapCheckTypes(size: Int, notSize: Int, leftTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'java map should be symbol' failed", " should be ('empty)", "JavaMapIsEmpty[String, String]", errorFunPrefix + "JavaMapNotIsEmpty[String, String]", "0",  None, (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java map should not be symbol' failed", " should not be 'empty", "JavaMapNotIsEmpty[String, String]", errorFunPrefix + "JavaMapIsEmpty[String, String]", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)), 
      ("'java map should have size' failed", " should have size " + size, "JavaMapSizeEqual[String, String]", errorFunPrefix + "JavaMapSizeNotEqual[String, String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java map should not have size' failed", " should not have size (" + notSize + ")", "JavaMapSizeNotEqual[String, String]", errorFunPrefix + "JavaMapSizeEqual[String, String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java map should have length' failed", " should have length " + size, "JavaMapSizeEqual[String, String]", errorFunPrefix + "JavaMapSizeNotEqual[String, String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new DidNotHaveLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java map should not have length' failed", " should not have length (" + notSize + ")", "JavaMapSizeNotEqual[String, String]", errorFunPrefix + "JavaMapSizeEqual[String, String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java map should contain key' failed", " should contain key \"b\"", "JavaMapContainKey[String, String]", errorFunPrefix + "JavaMapNotContainKey[String, String]", "\"b\"", "b", (colType: String, errorFun: String, errorValue: String) => new DidNotContainKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "b", autoQuoteString)),
      ("'java map should not contain key' failed", " should not contain key (\"b\")", "JavaMapNotContainKey[String, String]", errorFunPrefix + "JavaMapContainKey[String, String]", "\"b\"", "b", (colType: String, errorFun: String, errorValue: String) => new ContainedKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "b", autoQuoteString)), 
      ("'java map should contain value' failed", " should contain value \"boom!\"", "JavaMapContainValue[String, String]", errorFunPrefix + "JavaMapNotContainValue[String, String]", "\"boom!\"", "boom!", (colType: String, errorFun: String, errorValue: String) => new DidNotContainValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "boom!", autoQuoteString)),
      ("'java map should not contain value' failed", " should not contain value (\"boom!\")", "JavaMapNotContainValue[String, String]", errorFunPrefix + "JavaMapContainValue[String, String]", "\"boom!\"", "boom!", (colType: String, errorFun: String, errorValue: String) => new ContainedValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "boom!", autoQuoteString))
    )
    
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
  
  def simpleMessageFun(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{1}")
  def trvSimpleMessageFun(colType: String, errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{1}")
  def quotedSimpleMessageFun(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("\\\"{1}\\\"")
  def quotedSimpleMessageFun2(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("\\\"{2}\\\"")
  
  def genInspectorShorthandsForAllSpecFile(targetDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")
              
    val succeedTests = 
    int123Col map { case (colText, xsText) =>
      new InspectorShorthandsSucceedTemplate("all(" + colText + ")", "all elements succeeded", "all(" + colText + ") should be < 4")
    }
            
    def intDynaFirst(errorFun: String, errorValue: String) = 
      new DynamicFirstElementTemplate("Int", errorFun, errorValue)
            
    val int123Types = 
      List(
        ("at least one element failed", " should not equal 2", "getFirstNotEqual[Int]", "getFirstEqual[Int]", "2", (errorFun: String, errorValue: String) => new EqualedMessageTemplate(intDynaFirst(errorFun, errorValue), 2)), 
        ("more than one element failed", " should be < 2", "getFirstLessThan", "getFirstMoreThanEqual", "2", (errorFun: String, errorValue: String) => new WasNotLessThanMessageTemplate(intDynaFirst(errorFun, errorValue), 2))
      ) ++ stdInt123Types(intDynaFirst, 2, "getFirst")
              
      val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
            
      def stringDynaFirst(errorFun: String, errorValue: String) = 
        new DynamicFirstElementTemplate("String", errorFun, errorValue)
            
      def stringDynaFirstLength(errorFun: String, errorValue: String) = 
        new DynamicFirstElementLengthTemplate("String", errorFun, errorValue)
            
      val nullStringTypes = stdNullStringTypes(stringDynaFirst, "getFirst")
              
      val propertyCheckCol = genCol("\"\", \"boom!\", \"\"", "\"WrappedArray(, boom!, )\"")
            
      val propertyCheckTypes = stdPropertyCheckTypes(0, stringDynaFirst, stringDynaFirst, stringDynaFirstLength, "getFirst")
              
      val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
            
      val instanceCheckTypes = stdInstanceCheckTypes(stringDynaFirst, "2", "getFirst")
            
      val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
              
      val stringCheckTypes = stdStringCheckTypes(stringDynaFirst, "getFirst")
            
      def trvStringDynaFirst(colType: String, errorFun: String, errorValue: String) = 
        if (colType.startsWith("Array"))
          new DynamicFirstArrayElementTemplate(colType, errorFun, errorValue)
        else  
          new DynamicFirstElementTemplate(colType, errorFun, errorValue)
              
      val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
            
      val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvStringDynaFirst, "getFirst")
              
      val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
            
      val traversableCheckTypes = stdTraversableCheckTypes(trvStringDynaFirst, 0, 1, "hi", "getFirst")
              
      val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"", 
                              "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"", 
                              "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
                                           
      val mapCheckCol = 
        maps flatMap { case (mapText, xsText) => 
          genColMap(mapText, xsText)
        }
            
      val mapCheckTypes = stdMapCheckTypes(trvStringDynaFirst, "getFirst")
              
      val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
            
      val javaColCheckCol = 
        javaCols flatMap { case (mapText, xsText) => 
          genColMap(mapText, xsText)
        }
            
      val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvStringDynaFirst, "getFirst")
              
      val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
            
      val javaMapCheckCol = 
        javaMaps flatMap { case (mapText, xsText, colType) =>
          genColMap(mapText, xsText).map { case (colText, xsText) =>
            (colText, xsText, colType)
          }
        }

      val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvStringDynaFirst, "getFirst")
            
      val allColText = "all(xs)"
      val failedTestConfigs = 
        (int123Col flatMap { case (colText, xsText) =>
          int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            (colText, condition, allColText + assertText, "Int", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText)
           }
         }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText)
           }
         })++ 
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText)
          }
        }) ++ 
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            (colText, condition, allColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
            val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText)
          }
        }) ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
            val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText)
          }
        }) ++ 
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
            val colType = "GenMap[String, String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
            val colType = "java.util.Collection[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText)
          }
        }) ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText)
          }
        })
    
    genFile(
      new File(targetDir, "InspectorShorthandsForAllSucceededSpec.scala"), 
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.all"), 
        importList = List(
                       "org.scalatest._", 
                       "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                       "collection.GenTraversable", 
                       "collection.GenMap"
                     ), 
        classTemplate = new ClassTemplate {
          val name = "InspectorShorthandsForAllSucceededSpec"
          override val extendName = Some("Spec")
          override val withList = List("Matchers", "SharedHelpers")
          override val children = succeedTests
        }
      )
    )
        
    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) => 
      val className = "InspectorShorthandsForAllFailedSpec" + i
      val inspectorShorthandsForAllFailedSpecFile = new File(targetDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, causeErrMsg, xsText) =>
        new InspectorShorthandsForAllErrorTemplateWithCause(colText, condition, assertText, inspectorShorthandsForAllFailedSpecFile.getName, 
                                                           colType, errorFun, errorValue, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForAllFailedSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.all"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }
  
  def genInspectorShorthandsForAtLeastSpecFile(targetDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")
              
    val succeedTests = 
    (int123Col map { case (colText, xsText) =>
      new InspectorShorthandsSucceedTemplate("atLeast(2, " + colText + ")", "elements succeeded count is equal to the min", "atLeast(2, " + colText + ") should be < 3")
    })
    
    val inspectorShorthandsForAtLeastSucceededSpecFile = new File(targetDir, "InspectorShorthandsForAtLeastSucceededSpec.scala")
    genFile(
        inspectorShorthandsForAtLeastSucceededSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.atLeast"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = "InspectorShorthandsForAtLeastSucceededSpec"
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = succeedTests
          }
        )
      )
      
    val int123Types = 
      List(
        ("at least one element failed", " should equal (2)", "indexElementEqual[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)
    
    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)
    
    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"WrappedArray(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    
    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")
    
    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)
    
    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 0, 1, "hi", "indexElement", true)
    
    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"", 
                              "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"", 
                              "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
    val mapCheckCol = 
      maps flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
    val javaColCheckCol = 
      javaCols flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
    
    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol = 
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
    
    val atLeast2ColText = "atLeast(3, xs)"
    val failedTestConfigs = 
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "")
          val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "2")
          val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, errorValue)
          val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++ 
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getSizeFun(errorFun, 0)
          val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getTraversableFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "GenMap[String, String]"
          val errorAssertFun = getMapFun(errorFun, right)
          val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"), 
                                     Map("4" -> "four", "5" -> "five", "6" -> "six"), 
                                     Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length  
          (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "java.util.Collection[String]"
          val errorAssertFun = getJavaColFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val errorAssertFun = getJavaMapFun(errorFun, right)
          import collection.JavaConversions._
          val passedCount = 3 - List[java.util.Map[String, String]](Map.empty[String, String], 
                                     Map("b" -> "boom!"), 
                                     Map("h" -> "hello!")).filter(errorAssertFun).length  
          (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })
      
    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) => 
      val className = "InspectorShorthandsForAtLeastFailedSpec" + i
      val inspectorShorthandsForAtLeastFailedSpecFile = new File(targetDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue,  passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForAtLeastErrorTemplate(colText, condition, assertText, inspectorShorthandsForAtLeastFailedSpecFile.getName, 
                                                      colType, errorFun, errorValue, 3, 3, passedCount, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForAtLeastFailedSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.atLeast"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }
  
  def genInspectorShorthandsForEverySpecFile(targetMatchersDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")
              
    val succeedTests = 
    int123Col map { case (colText, xsText) =>
      new InspectorShorthandsSucceedTemplate("every(" + colText + ")", "all elements succeeded", "every(" + colText + ") should be < 4")
    }
    
    val inspectorShorthandsForEverySucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForEverySucceededSpec.scala")
    genFile(
      inspectorShorthandsForEverySucceededSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.every"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = "InspectorShorthandsForEverySucceededSpec"
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = succeedTests
          }
        )
    )
    
    val int123Types = 
      List(
        ("at least one element failed", " should not equal 2", "indexElementNotEqual[Int]", "indexElementEqual[Int]", "2", (errorFun: String, errorValue: String) => new EqualedMessageTemplate("{1}", 2, false)), 
        ("more than one element failed", " should be < 2", "indexElementLessThan", "indexElementMoreThanEqual", "2", (errorFun: String, errorValue: String) => new WasNotLessThanMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)
      
    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)
    
    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"WrappedArray(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    
    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")
    
    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)
    
    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 0, 1, "hi", "indexElement", true)
    
    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"", 
                              "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"", 
                              "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
    val mapCheckCol = 
      maps flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
    val javaColCheckCol = 
      javaCols flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
    
    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol = 
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
      
    val everyColText = "every(xs)"
    val failedTestConfigs = 
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "")
          val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "2")
          val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, errorValue)
          val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++ 
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getSizeFun(errorFun, 0)
          val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getTraversableFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "GenMap[String, String]"
          val errorAssertFun = getMapFun(errorFun, right)
          val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"), 
                                     Map("4" -> "four", "5" -> "five", "6" -> "six"), 
                                     Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length  
          (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "java.util.Collection[String]"
          val errorAssertFun = getJavaColFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val errorAssertFun = getJavaMapFun(errorFun, right)
          import collection.JavaConversions._
          val passedCount = 3 - List[java.util.Map[String, String]](Map.empty[String, String], 
                                     Map("b" -> "boom!"), 
                                     Map("h" -> "hello!")).filter(errorAssertFun).length  
          (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })
      
    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) => 
      val className = "InspectorShorthandsForEveryFailedSpec" + i
      val inspectorShorthandsForEveryFailedSpecFile = new File(targetMatchersDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForEveryErrorTemplate(colText, condition, assertText, inspectorShorthandsForEveryFailedSpecFile.getName, 
                                                    colType, errorFun, errorValue, 3, passedCount, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForEveryFailedSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.every"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }
  
  def genInspectorShorthandsForExactlySpecFile(targetMatchersDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")
              
    val succeedTests = 
    (int123Col map { case (colText, xsText) =>
      new InspectorShorthandsSucceedTemplate("atLeast(2, " + colText + ")", "elements succeeded count is exactly the same as specified", "exactly(3, " + colText + ") should be < 4")
    })
    
    val inspectorShorthandsForExactlySucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForExactlySucceededSpec.scala")
    genFile(
        inspectorShorthandsForExactlySucceededSpecFile, 
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.exactly"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = "InspectorShorthandsForExactlySucceededSpec"
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = succeedTests
          }
        )
    )
    
    val int123Types = 
      List(
        ("less one element passed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)
    
    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)
    
    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"WrappedArray(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
      
    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")
    
    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)
    
    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 0, 1, "hi", "indexElement", true)
    
    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"", 
                              "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"", 
                              "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
    val mapCheckCol = 
      maps flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)
    
    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
    val javaColCheckCol = 
      javaCols flatMap { case (mapText, xsText) => 
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
    
    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol = 
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)
    
    val exactly3ColText = "exactly(3, xs)"
      
    val failedTestConfigs = 
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "")
          val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, "2")
          val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val errorAssertFun = getFun(errorFun, errorValue)
          val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++ 
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getSizeFun(errorFun, 0)
          val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getTraversableFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "GenMap[String, String]"
          val errorAssertFun = getMapFun(errorFun, right)
          val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"), 
                                     Map("4" -> "four", "5" -> "five", "6" -> "six"), 
                                     Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length  
          (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val colType = "java.util.Collection[String]"
          val errorAssertFun = getJavaColFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++ 
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) => 
          val errorAssertFun = getJavaMapFun(errorFun, right)
          import collection.JavaConversions._
          val passedCount = 3 - List[java.util.Map[String, String]](Map.empty[String, String], 
                                     Map("b" -> "boom!"), 
                                     Map("h" -> "hello!")).filter(errorAssertFun).length  
          (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })
      
    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) => 
      val className = "InspectorShorthandsForExactlyFailedSpec" + i
      val inspectorShorthandsForExactlyFailedSpecFile = new File(targetMatchersDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForExactlyErrorTemplate(colText, condition, assertText, inspectorShorthandsForExactlyFailedSpecFile.getName,
                                                      colType, okFun, errorFun, errorValue, 3, 3, passedCount, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForExactlyFailedSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.exactly"), 
          importList = List(
                         "org.scalatest._", 
                         "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
                         "collection.GenTraversable", 
                         "collection.GenMap"
                       ), 
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }

  def genInspectorShorthandsForNoSpecFile(targetMatchersDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")

    val succeedTests =
      int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("no(" + colText + ")", "no elements succeeded", "every(" + colText + ") should be > 0")
      }

    val inspectorShorthandsForNoSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForNoSucceededSpec.scala")
    genFile(
      inspectorShorthandsForNoSucceededSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.no"),
        importList = List(
          "org.scalatest._", 
          "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
          "collection.GenTraversable",
          "collection.GenMap"
        ),
        classTemplate = new ClassTemplate {
          val name = "InspectorShorthandsForNoSucceededSpec"
          override val extendName = Some("Spec")
          override val withList = List("Matchers", "SharedHelpers")
          override val children = succeedTests
        }
      )
    )

    val int123Types =
      List(
        ("at least one element failed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"WrappedArray(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true)

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 1, 2, "hi", "indexElement", true)

    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"",
      "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"",
      "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
    val mapCheckCol =
      maps flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)

    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
    val javaColCheckCol =
      javaCols flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)

    val noColText = "no(xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, "")
          val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, "2")
          val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, errorValue)
          val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getSizeFun(errorFun, 0)
          val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
          (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getTraversableFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "GenMap[String, String]"
          val errorAssertFun = getMapFun(errorFun, right)
          val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
            Map("4" -> "four", "5" -> "five", "6" -> "six"),
            Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "java.util.Collection[String]"
          val errorAssertFun = getJavaColFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val errorAssertFun = getJavaMapFun(errorFun, right)
          import collection.JavaConversions._
          val passedCount = 3 - List[java.util.Map[String, String]](Map.empty[String, String],
            Map("b" -> "boom!"),
            Map("h" -> "hello!")).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })

    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) =>
      val className = "InspectorShorthandsForNoFailedSpec" + i
      val inspectorShorthandsForNoFailedSpecFile = new File(targetMatchersDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForNoErrorTemplate(colText, condition, assertText, inspectorShorthandsForNoFailedSpecFile.getName,
          colType, okFun, errorFun, errorValue, xsText)
      }
      genFile(
        inspectorShorthandsForNoFailedSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.no"),
          importList = List(
            "org.scalatest._", 
            "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
            "collection.GenTraversable",
            "collection.GenMap"
          ),
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }

  def genInspectorShorthandsForBetweenSpecFile(targetMatchersDir: File) {
    val int123Col = genCol("1, 2, 3", "\"WrappedArray(1, 2, 3)\"")

    val succeedTests =
      int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("between(2, 4, " + colText + ")", "elements succeeded count is in range", "between(2, 4, " + colText + ") should be > 0")
      }

    val inspectorShorthandsForBetweenSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForBetweenSucceededSpec.scala")
    genFile(
      inspectorShorthandsForBetweenSucceededSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.between"),
        importList = List(
          "org.scalatest._", 
          "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
          "collection.GenTraversable",
          "collection.GenMap"
        ),
        classTemplate = new ClassTemplate {
          val name = "InspectorShorthandsForBetweenSucceededSpec"
          override val extendName = Some("Spec")
          override val withList = List("Matchers", "SharedHelpers")
          override val children = succeedTests
        }
      )
    )

    val int123Types =
      List(
        ("less one element passed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"WrappedArray(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"WrappedArray(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"WrappedArray(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true)

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 0, 1, "hi", "indexElement", true)

    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"",
      "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"",
      "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\""))
    val mapCheckCol =
      maps flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)

    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]"))
    val javaColCheckCol =
      javaCols flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, "indexElement", true)

    val betweenColText = "between(7, 8, xs)"

    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, "")
          val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, "2")
          val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, errorValue)
          val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getSizeFun(errorFun, 0)
          val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val errorAssertFun = getTraversableFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "GenMap[String, String]"
          val errorAssertFun = getMapFun(errorFun, right)
          val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
            Map("4" -> "four", "5" -> "five", "6" -> "six"),
            Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "java.util.Collection[String]"
          val errorAssertFun = getJavaColFun(errorFun, right)
          val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val errorAssertFun = getJavaMapFun(errorFun, right)
          import collection.JavaConversions._
          val passedCount = 3 - List[java.util.Map[String, String]](Map.empty[String, String],
            Map("b" -> "boom!"),
            Map("h" -> "hello!")).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })

    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) =>
      val className = "InspectorShorthandsForBetweenFailedSpec" + i
      val inspectorShorthandsForBetweenFailedSpecFile = new File(targetMatchersDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForBetweenErrorTemplate(colText, condition, assertText, inspectorShorthandsForBetweenFailedSpecFile.getName,
          colType, okFun, errorFun, errorValue, 7, 8, 3, passedCount, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForBetweenFailedSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.between"),
          importList = List(
            "org.scalatest._", 
            "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
            "collection.GenTraversable",
            "collection.GenMap"
          ),
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }

  def genInspectorShorthandsForAtMostSpecFile(targetMatchersDir: File) {
    val int123Col = genCol("1, 2, 3, 4, 5", "\"WrappedArray(1, 2, 3, 4, 5)\"")

    val succeedTests =
      (int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("atMost(5, " + colText + ")", "elements succeeded count is equal to the max", "atMost(5, " + colText + ") should be < 5")
      })

    val inspectorShorthandsForAtMostSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForAtMostSucceededSpec.scala")
    genFile(
      inspectorShorthandsForAtMostSucceededSpecFile,
      new SingleClassFile(
        packageName = Some("org.scalatest.inspectors.atMost"),
        importList = List(
          "org.scalatest._", 
          "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
          "collection.GenTraversable",
          "collection.GenMap"
        ),
        classTemplate = new ClassTemplate {
          val name = "InspectorShorthandsForAtMostSucceededSpec"
          override val extendName = Some("Spec")
          override val withList = List("Matchers", "SharedHelpers")
          override val children = succeedTests
        }
      )
    )

    val int123Types =
      List(
        ("more than max element succeeded", " should not equal (2)", "NotEqual[Int]", "indexElementEqual[Int]", "2", (errorFun: String, errorValue: String) => new EqualedMessageTemplate("{1}", 2, false))
      ) ++
      (stdInt123Types(simpleMessageFun, 3, "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
        condition != "'should equal' failed" &&
        condition != "'should be' failed" &&
        condition != "'should be ===' failed"// no way to get this two to fail with atMost(1).
      })

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"WrappedArray(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be null' failed"
    }

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\", \"cool!\", \"great!\"", "\"WrappedArray(, boom!, hi, cool!, great!)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(5, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement").filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be symbol' failed" &&
      condition != "'should be property' failed" &&
      condition != "'should be a symbol' failed" &&
      condition != "'should be a property' failed" &&
      condition != "'should be an symbol' failed" &&
      condition != "'should be an property' failed" &&
      condition != "'should have property' failed"
    }

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\", \"4\", \"5\"", "\"WrappedArray(1, 2, 3, 4, 5)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement").filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be theSameInstanceAs' failed"
    }

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\", \"hi D\", \"hello E!\"", "\"WrappedArray(hello A!, hi B, hello C!, hi D, hello E!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"\", \"boom!\", \"great!\"", "", "\"hi\", \"cool!\""), "\"WrappedArray(Array(), Array(\\\"boom!\\\"), Array(\\\"hi\\\"), Array(\\\"cool!\\\"), Array(\\\"great!\\\"))\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\"", "\"boom!\", \"hi\""), "\"WrappedArray(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, 1, 2, "hi", "indexElement", true)

    val maps = genMap(Array("\"1\" -> \"one\", \"2\" -> \"two\", \"3\" -> \"three\"",
                            "\"4\" -> \"four\", \"5\" -> \"five\", \"6\" -> \"six\"",
                            "\"2\" -> \"two\", \"6\" -> \"six\", \"8\" -> \"eight\"",
                            "\"7\" -> \"seven\", \"8\" -> \"eight\", \"9\" -> \"nine\""))
    val mapCheckCol =
      maps flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val mapCheckTypes = stdMapCheckTypes(trvSimpleMessageFun, "indexElement", true)

    val javaCols = genJavaCol(Array("List(\"hi\")", "List(\"boom!\")", "List.empty[String]", "List(\"great!\")", "List(\"hi\", \"cool!\")"))
    val javaColCheckCol =
      javaCols flatMap { case (mapText, xsText) =>
        genColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(1, 0, trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
      condition != "'java collection should be symbol' failed"
    }

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")", "Map.empty[String, String]", "Map(\"b\" -> \"hello!\")", "Map(\"h\" -> \"boom!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(1, 0, trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
      condition != "'java map should be symbol' failed"
    }

    val atMostColText = "atMost(1, xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2  // always 2 as atMost will fail early
          (colText, condition, atMostColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      })  ++
      (nullStringCol flatMap { case (colText, xsText) =>
        nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2
          (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (propertyCheckCol flatMap { case (colText, xsText) =>
        propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2
          (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (instanceCheckCol flatMap { case (colText, xsText) =>
        instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2
          (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText)
        }
      }) ++
      (stringCheckCol flatMap { case (colText, xsText) =>
        stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2
          (colText, condition, atMostColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText)
        }
      }) ++
      (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
        traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val passedCount = 2
          (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (traversableCheckCol flatMap { case (colText, xsText) =>
        traversableCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = if (colText.startsWith("Array")) "Array[String]" else "GenTraversable[String]"
          val passedCount = 2
          (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (mapCheckCol flatMap { case (colText, xsText) =>
        mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "GenMap[String, String]"
          val passedCount = 2
          (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaColCheckCol flatMap { case (colText, xsText) =>
        javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val colType = "java.util.Collection[String]"
          val passedCount = 2
          (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      }) ++
      (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
        javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
          val passedCount = 2
          (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText)
        }
      })


    failedTestConfigs.grouped(500).toList.zipWithIndex foreach { case (configs, i) =>
      val className = "InspectorShorthandsForAtMostFailedSpec" + i
      val inspectorShorthandsForAtMostFailedSpecFile = new File(targetMatchersDir, className + ".scala")
      val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText) =>
        new InspectorShorthandsForAtMostErrorTemplate(colText, condition, assertText, inspectorShorthandsForAtMostFailedSpecFile.getName,
          colType, okFun, errorFun, errorValue, 1, passedCount, causeErrMsg, xsText)
      }
      genFile(
        inspectorShorthandsForAtMostFailedSpecFile,
        new SingleClassFile(
          packageName = Some("org.scalatest.inspectors.atMost"),
          importList = List(
            "org.scalatest._", 
            "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}", 
            "collection.GenTraversable",
            "collection.GenMap"
          ),
          classTemplate = new ClassTemplate {
            val name = className
            override val extendName = Some("Spec")
            override val withList = List("Matchers", "SharedHelpers")
            override val children = new InspectorShorthandsHelpersTemplate :: failedTests
          }
        )
      )
    }
  }
  
  def targetDir(targetBaseDir: String, packageName: String): File = {
    val targetDir = new File(targetBaseDir + "/org/scalatest/inspectors/" + packageName)
    if (!targetDir.exists)
      targetDir.mkdirs()
    targetDir
  }

  def genTest(targetBaseDir: String, scalaVersion: String) {
    genNestedInspectorsSpecFile(targetDir(targetBaseDir, "nested"))
    genInspectorShorthandsForAllSpecFile(targetDir(targetBaseDir, "all"))
    genInspectorShorthandsForAtLeastSpecFile(targetDir(targetBaseDir, "atLeast"))
    genInspectorShorthandsForEverySpecFile(targetDir(targetBaseDir, "every"))
    genInspectorShorthandsForExactlySpecFile(targetDir(targetBaseDir, "exactly"))
    genInspectorShorthandsForNoSpecFile(targetDir(targetBaseDir, "no"))
    genInspectorShorthandsForBetweenSpecFile(targetDir(targetBaseDir, "between"))
    genInspectorShorthandsForAtMostSpecFile(targetDir(targetBaseDir, "atMost"))
  }
  
  def main(args: Array[String]) {
    val targetBaseDir = args(0)
    val scalaVersion = args(1)
    genTest(targetBaseDir, scalaVersion)
  }
  
}
