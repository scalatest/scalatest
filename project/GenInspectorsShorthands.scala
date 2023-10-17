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
import scala.annotation.tailrec

trait GenInspectorsShorthandsBase {

  import Generator._
  import GenInspectors._

  class DynamicErrorDetailTemplate(fileName: String, lineNumber: String, messageTemplate: Template, formatParams: String, useIndex: Boolean) extends Template {
    override def toString =
      "\" + new java.text.MessageFormat(\"at " + (if (useIndex) "index" else "key") + " {0}, " + messageTemplate + " (" + fileName + ":\" + " + lineNumber + " + \")\"" + ").format(" + formatParams + ") + \""
  }

  class DynamicFirstIndexErrorDetailTemplate(colType: String, errorFun: String, errorValue: String, fileName: String, lineNumber: String, messageTemplate: Template) extends
    ErrorDetailTemplate("\" + getIndex(xs, " + errorFun + "(xs, " + errorValue + ")) + \"", fileName, lineNumber, messageTemplate)

  class DynamicFirstElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      if (colType == "String")
        "\" + decorateToStringValue(prettifier, " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ")) + \""
      else
        "\" + decorateToStringValue(prettifier, " + errorFun + "(xs, " + errorValue + ")) + \""
  }

  class DynamicFirstArrayElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      "\" + decorateToStringValue(prettifier, deep(" + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + "))) + \""
  }

  class DynamicFirstElementLengthTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      "\" + " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ").length + \""
  }

  class DynamicFirstElementSizeTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      "\" + " + getErrorMessageValuesFunName(colType, errorFun) + "(xs, " + errorValue + ").size + \""
  }

  class DynamicFirstElementGetKeyTemplate(colType: String, colText: String, errorFun: String, errorValue: String, fileName: String, lineNumber: String, messageTemplate: Template) extends
    ErrorDetailTemplate("\" + " + errorFun + "(xs, " + errorValue + ")." + (if (colText.contains("java")) "getKey" else "_1") + " + \"", fileName, lineNumber, messageTemplate) {
    override val at: String = "key"
  }

  class DynamicNextIndexErrorDetailTemplate(errorValue: String, fileName: String, lineNumber: String, messageTemplate: Template, messageValuesFunName: String, useIndex: Boolean) extends
    DynamicErrorDetailTemplate(fileName, lineNumber, messageTemplate, messageValuesFunName + "(itr, xs, " + errorValue + ")", useIndex)

  class DynamicNextElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      if (colType == "String")
        "\\\"\" + " + errorFun + "(itr, " + errorValue + ") + \"\\\""
      else
        "\" + " + errorFun + "(itr, " + errorValue + ") + \""
  }

  class DynamicNextArrayElementTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      "\" + deep(" + errorFun + "[" + colType + "](itr, " + errorValue + ")) + \""
  }

  class DynamicNextElementLengthTemplate(colType: String, errorFun: String, errorValue: String) extends Template {
    override def toString =
      "\" + " + errorFun + "[" + colType + "](itr, " + errorValue + ").length + \""
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
                                                         errorValue: String, causeErrMsg: String, xsText: String,
                                                         useIndex: Boolean
                                                       ) extends Template {

    val causeErrorMessage = new SimpleMessageTemplate(causeErrMsg)
    val errorMessage = new ForAllErrMsgTemplate("'all' inspection",
      if (useIndex)
        new DynamicFirstIndexErrorDetailTemplate(colType, getErrorMessageValuesFunName(colType, errorFun), errorValue, fileName, "assertLineNumber", causeErrorMessage)
      else
        new DynamicFirstElementGetKeyTemplate(colType, colText, errorFun, errorValue, fileName, "assertLineNumber", causeErrorMessage)
    )
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

  def iteratorText(useIndex: Boolean, colText: String): String =
    if (!useIndex && colText.contains("java")) "entrySet.iterator" else if (colText.contains("java")) "iterator" else "toIterator"

  class InspectorShorthandsForAtLeastErrorTemplate(
                                                    colText: String, condition: String, assertText: String,
                                                    fileName: String, colType: String, errorFun: String, errorValue: String,
                                                    min: Int, totalCount: Int, passedCount: Int, detailErrorMessage: String,
                                                    xsText: String, useIndex: Boolean) extends Template {

    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun), useIndex) }
    val errorMessage = new ForAtLeastErrMsgTemplate("'atLeast(" + min + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, details)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
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
                                                  xsText: String, useIndex: Boolean) extends Template {

    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun), useIndex) }
    val errorMessage = new ForEveryErrMsgTemplate("'every' inspection", details)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
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
                                                    xsText: String, useIndex: Boolean) extends Template {

    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun), useIndex) }
    val errorMessage = new ForExactlyErrMsgTemplate("'exactly(" + count + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType, details)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
        "  val e = intercept[exceptions.TestFailedException] {\n" +
        "    " + assertText + "\n" +
        "  }\n" +
        "  val assertLineNumber = thisLineNumber - 2\n" +
        "  checkError(e, assertLineNumber, \"" + fileName + "\", " + splitMultilineErrorMessage(errorMessage.toString) + ")\n" +
        "}\n"

  }

  class InspectorShorthandsForNoErrorTemplate(colText: String, condition: String, assertText: String,
                                              fileName: String, colType: String, okFun: String, errorFun: String, errorValue: String,
                                              xsText: String, useIndex: Boolean) extends Template {

    val keyIndexFun =
      if (useIndex)
        "getIndex(xs, getFirst" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + "))"
      else
        "getFirst" + getErrorMessageValuesFunName(colType, okFun) + "(xs, " + errorValue + ")." + (if (colText.contains("java")) "getKey" else "_1")
    val errorMessage = new ForNoErrMsgTemplate("'no' inspection", "\" + " + keyIndexFun + " + \"", useIndex)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
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
                                                   xsText: String, useIndex: Boolean) extends Template {

    val details = buildList(totalCount - passedCount, detailErrorMessage) map { errMsg => new DynamicNextIndexErrorDetailTemplate(errorValue, fileName, "assertLineNumber", new SimpleMessageTemplate(errMsg.toString), getErrorMessageValuesFunName(colType, errorFun), useIndex) }
    val errorMessage = new ForBetweenLessErrMsgTemplate("'between(" + from + ", " + upTo + ")' inspection", (if (passedCount > 0) "only " else "") + new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType, details)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
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
                                                  xsText: String, useIndex: Boolean) extends Template {

    val errorMessage = new ForAtMostErrMsgTemplate("'atMost(" + max + ")' inspection", max, new ElementTemplate(passedCount).toString, okFun, errorFun, errorValue, colType)

    override def toString =
      "def `" + colText + " should throw TestFailedException with correct stack depth and message when " + condition + "` {\n" +
        "  val xs = " + colText + "\n" +
        "  val itr = xs." + iteratorText(useIndex, colText) + "\n" +
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
        "val emptyMatcher = new EmptyBePropertyMatcher()\n" +
        "def plength(expectedValue: Int) = new StringLengthMatcher(expectedValue)\n" +
        "val theInstance = \"2\"\n" +
        "def arrayToString(xs: Iterable[_]): String = FailureMessages.decorateToStringValue(prettifier, xs)\n" +
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
  private def buildList[T](size: Int, element: T, list: List[T] = List.empty[T]): List[T] =
    if (list.size < size) {
      buildList(size, element, element :: list)
    }
    else
      list

  def splitMultilineErrorMessage(errorMessage: String) =
    errorMessage.toString.split("\n").map("\"" + _).mkString("\n")

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
      ("collection.mutable.IndexedSeq(" + colText + ").par", "xs"),
      ("javaList(" + colText + ")", "xs"),
      ("javaSet(" + colText + ")", "xs"),
      ("Every(" + colText + ")", "xs")
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
      ("collection.mutable.IndexedSeq(" + colText + ").par", "xs"),
      ("javaList(" + colText + ")", "xs"),
      ("javaSet(" + colText + ")", "xs")
    )

  def genColCol[T](colType: String, colTexts: Array[String], arrayXsText: String) =
    List[(String, String)](
      ("Set(" + colTexts.map("Set[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("List(" + colTexts.map("List[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("Seq(" + colTexts.map("Seq[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("Array[Array[" + colType + "]](" + colTexts.map("Array[" + colType + "](" + _ + ")").mkString(", ") + ")", "arrayToString(xs)"),
      ("IndexedSeq(" + colTexts.map("IndexedSeq[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("Vector(" + colTexts.map("Vector[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("Set(" + colTexts.map("Set[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("List(" + colTexts.map("List[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("Seq(" + colTexts.map("Seq[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("IndexedSeq(" + colTexts.map("IndexedSeq[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("collection.mutable.Set(" + colTexts.map("collection.mutable.Set[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("new collection.mutable.ListBuffer() ++ List(" + colTexts.map("new collection.mutable.ListBuffer[" + colType + "]() ++ List[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("collection.mutable.Seq(" + colTexts.map("collection.mutable.Seq[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("collection.mutable.IndexedSeq(" + colTexts.map("collection.mutable.IndexedSeq[" + colType + "](" + _ + ")").mkString(", ") + ")", "xs"),
      ("collection.mutable.Set(" + colTexts.map("collection.mutable.Set[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("(new collection.mutable.ListBuffer() ++ List(" + colTexts.map("(new collection.mutable.ListBuffer[" + colType + "]() ++ List[" + colType + "](" + _ + ")).par").mkString(", ") + ")).par", "xs"),
      ("collection.mutable.Seq(" + colTexts.map("collection.mutable.Seq[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs"),
      ("collection.mutable.IndexedSeq(" + colTexts.map("collection.mutable.IndexedSeq[" + colType + "](" + _ + ").par").mkString(", ") + ").par", "xs")
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

  def genJavaColMap[T](mapText: String, arrayXsText: String) =
    List[(String, String)](
      ("javaArrayList(List(" + mapText + "))", "xs"),
      ("javaHashSet(List(" + mapText + "))", "xs"),
      ("javaLinkedList(List(" + mapText + "))", "xs"),
      ("javaVector(List(" + mapText + "))", "xs"),
      ("javaArrayDeque(List(" + mapText + "))", "xs"),
      ("javaConcurrentLinkedQueue(List(" + mapText + "))", "xs"),
      ("javaCopyOnWriteArrayList(List(" + mapText + "))", "xs"),
      ("javaCopyOnWriteArraySet(List(" + mapText + "))", "xs"),
      ("javaLinkedBlockingDeque(List(" + mapText + "))", "xs"),
      ("javaLinkedBlockingQueue(List(" + mapText + "))", "xs"),
      ("javaLinkedHashSet(List(" + mapText + "))", "xs")
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
      case "indexElementNotEqual[(Int, Int)]" => _ != right
      case "indexElementEqual[(Int, Int)]" => _ == right
      case "indexElementNotEqual[Int, Int]" => _ != right
      case "indexElementEqual[Int, Int]" => _ == right
      case "indexElementNotEqual" => _ != right
      case "indexElementEqual" => _ == right
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
      case "indexElementSizeEqual[(Int, Int)]" => _.size == right
      case "indexElementSizeNotEqual[(Int, Int)]" => _.size != right
      case "indexElementSizeEqual[Int, Int]" => _.size == right
      case "indexElementSizeNotEqual[Int, Int]" => _.size != right
      case "indexElementSizeEqualIterable[String]" => _.size == right
      case "indexElementSizeNotEqualIterable[String]" => _.size != right
    }
  }

  def getFun(funString: String, right: String): String => Boolean = {
    funString match {
      case "indexElementNotEqual[String]" => _ != right
      case "indexElementEqual[String]" => _ == right
      case "indexElementNotEqual[(Int, Int)]" => _ != right
      case "indexElementEqual[(Int, Int)]" => _ == right
      case "indexElementNotEqual[Int, Int]" => _ != right
      case "indexElementEqual[Int, Int]" => _ == right
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
      case "indexElementSizeEqualIterable[String]" => _.size == right
      case "indexElementSizeNotEqualIterable[String]" => _.size != right
      //case s: String if s.startsWith("_.exists") => _.exists(_ == right)
      //case s: String if s.startsWith("!_.exists") => !_.exists(_ == right)
      case "indexElementContainIterable[String]" => _.contains(right)
      case "indexElementNotContainIterable[String]" => !_.contains(right)
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
      case "indexElementJavaColSizeEqual" => _.size == right
      case "indexElementJavaColSizeNotEqual" => _.size != right
      case "indexElementJavaColIsEmpty" => _.isEmpty
      case "indexElementJavaColNotIsEmpty" => !_.isEmpty
      case "indexElementJavaColContain" => _.contains(right)
      case "indexElementJavaColNotContain" => !_.contains(right)
    }
  }

  def getJavaMapFun(funString: String, right: Any): java.util.Map[String, String] => Boolean = {
    funString match {
      case "indexElementJavaMapIsEmpty" => _.isEmpty
      case "indexElementJavaMapNotIsEmpty" => !_.isEmpty
      case "indexElementJavaMapSizeEqual" => _.size == right
      case "indexElementJavaMapSizeNotEqual" => _.size != right
      case "indexElementJavaMapContainKey" => _.containsKey(right)
      case "indexElementJavaMapNotContainKey" => !_.containsKey(right)
      case "indexElementJavaMapContainValue" => _.containsValue(right)
      case "indexElementJavaMapNotContainValue" => !_.containsValue(right)
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
      ("'should not be greater than or equal comparison' failed", " should not be >= (" + right + ")", "LessThan", errorFunPrefix + "MoreThanEqual", "" + right, (errorFun: String, errorValue: String) => new WasGreaterThanOrEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString))
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
      ("'should be property' failed", " should be (emptyMatcher)", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be property' failed", " should not be emptyMatcher", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should be a symbol' failed", " should be a 'empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be a symbol' failed", " should not be a ('empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should be a property' failed", " should be a emptyMatcher", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be a property' failed", " should not be a (emptyMatcher)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should be an symbol' failed", " should be an 'empty", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be an symbol' failed", " should not be an ('empty)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should be an property' failed", " should be an emptyMatcher", "IsEmpty", errorFunPrefix + "IsNotEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasNotAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should not be an property' failed", " should not be an (emptyMatcher)", "IsNotEmpty", errorFunPrefix + "IsEmpty", "\"\"", (errorFun: String, errorValue: String) => new WasAnMessageTemplate(leftTemplateFun(errorFun, errorValue), empty, autoQuoteString)),
      ("'should have property' failed", " should have (plength(" + expectedLengthSize + "))", "LengthEqual", errorFunPrefix + "LengthNotEqualLength", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new PropertyHadUnexpectedValueMessageTemplate("length", 0, leftLengthTemplateFun(errorFun, errorValue), targetTemplateFun(errorFun, errorValue), autoQuoteString)),
      ("'should not have property' failed", " should not have plength(" + expectedLengthSize + ")", "LengthNotEqual", errorFunPrefix + "LengthEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new PropertyHadExpectedValueMessageTemplate("length", 0, leftTemplateFun(errorFun, errorValue), autoQuoteString))
    )

  def stdLengthSizeCheckTypes(expectedLengthSize:Int, leftTemplateFun: (String, String) => Template, leftLengthTemplateFun: (String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'should have length' failed", " should have length " + expectedLengthSize, "LengthEqual", errorFunPrefix + "LengthNotEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new HadLengthInsteadOfExpectedLengthMessageTemplate(leftTemplateFun(errorFun, errorValue), leftLengthTemplateFun(errorFun, errorValue), 0, autoQuoteString)),
      ("'should not have length' failed", " should not have length (" + expectedLengthSize + ")", "LengthNotEqual", errorFunPrefix + "LengthEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(errorFun, errorValue), 0, autoQuoteString)),
      ("'should have size' failed", " should have size " + expectedLengthSize, "SizeEqual", errorFunPrefix + "SizeNotEqual", "" + expectedLengthSize, (errorFun: String, errorValue: String) => new HadSizeInsteadOfExpectedSizeMessageTemplate(leftTemplateFun(errorFun, errorValue), leftLengthTemplateFun(errorFun, errorValue), 0, autoQuoteString)),
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
      ("'traversable should be symbol' failed", " should be ('empty)", "SizeEqualIterable[String]", errorFunPrefix + "SizeNotEqualIterable[String]", "0", (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'traversable should not be symbol' failed", " should not be 'empty", "SizeNotEqualIterable[String]", errorFunPrefix + "SizeEqualIterable[String]", "0", (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString))
    )

  def stdTraversableCheckTypes(leftTemplateFun: (String, String, String) => Template, leftLengthTemplateFun: (String, String, String) => Template, size: Int, notSize: Int, containText: String, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'traversable should have size' failed", " should have size " + size, "SizeEqualIterable[String]", errorFunPrefix + "SizeNotEqualIterable[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadSizeInsteadOfExpectedSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftLengthTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'traversable should not have size' failed", " should not have size (" + notSize +")", "SizeNotEqualIterable[String]", errorFunPrefix + "SizeEqualIterable[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'traversable should have length' failed", " should have length " + size, "SizeEqualIterable[String]", errorFunPrefix + "SizeNotEqualIterable[String]", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadLengthInsteadOfExpectedLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftLengthTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'traversable should not have length' failed", " should not have length (" + notSize + ")", "SizeNotEqualIterable[String]", errorFunPrefix + "SizeEqualIterable[String]", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'traversable should contain' failed", " should contain (\"" + containText + "\")", "ContainIterable[String]", errorFunPrefix + "NotContainIterable[String]", "\"" + containText + "\"", containText, (colType: String, errorFun: String, errorValue: String) => new DidNotContainElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString)),
      ("'traversable should not contain' failed", " should not contain \"" + containText + "\"", "NotContainIterable[String]", errorFunPrefix + "ContainIterable[String]", "\"" + containText + "\"", containText, (colType: String, errorFun: String, errorValue: String) => new ContainedElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString))
    )

  def stdMapCheckTypes(leftTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'map should contain key' failed", " should contain key \"2\"", "ContainKey[String, String]", errorFunPrefix + "NotContainKey[String, String]", "\"2\"", "2",  (colType: String, errorFun: String, errorValue: String) => new DidNotContainKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "2", autoQuoteString)),
      ("'map should not contain key' failed", " should not contain key (\"2\")", "NotContainKey[String, String]", errorFunPrefix + "ContainKey[String, String]", "\"2\"", "2", (colType: String, errorFun: String, errorValue: String) => new ContainedKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "2", autoQuoteString)),
      ("'map should contain value' failed", " should contain value \"two\"", "ContainValue[String, String]", errorFunPrefix + "NotContainValue[String, String]", "\"two\"", "two", (colType: String, errorFun: String, errorValue: String) => new DidNotContainValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "two", autoQuoteString)),
      ("'map should not contain value' failed", " should not contain value (\"two\")", "NotContainValue[String, String]", errorFunPrefix + "ContainValue[String, String]", "\"two\"", "two",  (colType: String, errorFun: String, errorValue: String) => new ContainedValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "two", autoQuoteString))
    )

  def stdStringTypes(leftTemplateFun: (String, String) => Template, right: Char, errorFunPrefix: String, autoQuoteString: Boolean, useMessageFormat: Boolean) = {
    val quote = if (useMessageFormat) "''" else "'"
    List(
      ("'should equal' failed", " should equal ('" + right + "')", "Equal", errorFunPrefix + "NotEqual", "'" + right + "'", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(quote + right + quote), autoQuoteString)),
      ("'should not equal' failed", " should not equal '" + right + "'", "NotEqual", errorFunPrefix + "Equal", "'" + right + "'", (errorFun: String, errorValue: String) => new EqualedMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(quote + right + quote), autoQuoteString)),
      ("'should be' failed", " should be ('" + right + "')", "Equal", errorFunPrefix + "NotEqual", "'" + right + "'", (errorFun: String, errorValue: String) => new WasNotEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(quote + right + quote), autoQuoteString)),
      ("'should not be' failed", " should not be '" + right + "'", "NotEqual", errorFunPrefix + "Equal", "'" + right + "'", (errorFun: String, errorValue: String) => new WasEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(quote + right + quote), autoQuoteString))
    )
  }

  def stdNumberMapTypes(leftTemplateFun: (String, String) => Template, right: Tuple2[_, _], errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'should equal' failed", " should equal (" + right + ")", "Equal[(Int, Int)]", errorFunPrefix + "NotEqual[(Int, Int)]", "" + right, (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not equal' failed", " should not equal " + right, "NotEqual[(Int, Int)]", errorFunPrefix + "Equal[(Int, Int)]", "" + right, (errorFun: String, errorValue: String) => new EqualedMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should be' failed", " should be (" + right + ")", "Equal[(Int, Int)]", errorFunPrefix + "NotEqual[(Int, Int)]", "" + right, (errorFun: String, errorValue: String) => new WasNotEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString)),
      ("'should not be' failed", " should not be " + right, "NotEqual[(Int, Int)]", errorFunPrefix + "Equal[(Int, Int)]", "" + right, (errorFun: String, errorValue: String) => new WasEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), right, autoQuoteString))
    )

  def stdNumberJavaMapTypes(leftTemplateFun: (String, String) => Template, right: Tuple2[_, _], errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'should equal' failed", " should equal (org.scalatest.Entry(" + right._1 + ", " + right._2 + "))", "Equal[Int, Int]", errorFunPrefix + "NotEqual[Int, Int]", "org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(right._1 + "=" + right._2), autoQuoteString)),
      ("'should not equal' failed", " should not equal org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", "NotEqual[Int, Int]", errorFunPrefix + "Equal[Int, Int]", "org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", (errorFun: String, errorValue: String) => new EqualedMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(right._1 + "=" + right._2), autoQuoteString)),
      ("'should be' failed", " should be (org.scalatest.Entry(" + right._1 + ", " + right._2 + "))", "Equal[Int, Int]", errorFunPrefix + "NotEqual[Int, Int]", "org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", (errorFun: String, errorValue: String) => new WasNotEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(right._1 + "=" + right._2), autoQuoteString)),
      ("'should not be' failed", " should not be org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", "NotEqual[Int, Int]", errorFunPrefix + "Equal[Int, Int]", "org.scalatest.Entry(" + right._1 + ", " + right._2 + ")", (errorFun: String, errorValue: String) => new WasEqualToMessageTemplate(leftTemplateFun(errorFun, errorValue), UnquotedString(right._1 + "=" + right._2), autoQuoteString))
    )

  def stdJavaColCheckTypes(size: Int, notSize: Int, leftTemplateFun: (String, String, String) => Template, leftSizeTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'java collection should be symbol' failed", " should be ('empty)", "JavaColIsEmpty", errorFunPrefix + "JavaColNotIsEmpty", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java collection should not be symbol' failed", " should not be 'empty", "JavaColNotIsEmpty", errorFunPrefix + "JavaColIsEmpty", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java collection should have size' failed", " should have size " + size, "JavaColSizeEqual", errorFunPrefix + "JavaColSizeNotEqual", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadSizeInsteadOfExpectedSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftSizeTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java collection should not have size' failed", " should not have size (" + notSize + ")", "JavaColSizeNotEqual", errorFunPrefix + "JavaColSizeEqual", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java collection should have length' failed", " should have length " + size, "JavaColSizeEqual", errorFunPrefix + "JavaColSizeNotEqual", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadLengthInsteadOfExpectedLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftSizeTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java collection should not have length' failed", " should not have length (" + notSize + ")", "JavaColSizeNotEqual", errorFunPrefix + "JavaColSizeEqual", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java collection should contain' failed", " should contain (\"hi\")", "JavaColContain", errorFunPrefix + "JavaColNotContain", "\"hi\"", "hi", (colType: String, errorFun: String, errorValue: String) => new DidNotContainElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString)),
      ("'java collection should not contain' failed", " should not contain \"hi\"", "JavaColNotContain", errorFunPrefix + "JavaColContain", "\"hi\"", "hi", (colType: String, errorFun: String, errorValue: String) => new ContainedElementMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "hi", autoQuoteString))
    )

  def stdJavaMapCheckTypes(size: Int, notSize: Int, leftTemplateFun: (String, String, String) => Template, leftSizeTemplateFun: (String, String, String) => Template, errorFunPrefix: String, autoQuoteString: Boolean = true) =
    List(
      ("'java map should be symbol' failed", " should be ('empty)", "JavaMapIsEmpty", errorFunPrefix + "JavaMapNotIsEmpty", "0",  None, (colType: String, errorFun: String, errorValue: String) => new WasNotMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java map should not be symbol' failed", " should not be 'empty", "JavaMapNotIsEmpty", errorFunPrefix + "JavaMapIsEmpty", "0", None, (colType: String, errorFun: String, errorValue: String) => new WasMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), empty, autoQuoteString)),
      ("'java map should have size' failed", " should have size " + size, "JavaMapSizeEqual", errorFunPrefix + "JavaMapSizeNotEqual", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadSizeInsteadOfExpectedSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftSizeTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java map should not have size' failed", " should not have size (" + notSize + ")", "JavaMapSizeNotEqual", errorFunPrefix + "JavaMapSizeEqual", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadSizeMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java map should have length' failed", " should have length " + size, "JavaMapSizeEqual", errorFunPrefix + "JavaMapSizeNotEqual", "" + size, size, (colType: String, errorFun: String, errorValue: String) => new HadLengthInsteadOfExpectedLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), leftSizeTemplateFun(colType, errorFun, errorValue), 0, autoQuoteString)),
      ("'java map should not have length' failed", " should not have length (" + notSize + ")", "JavaMapSizeNotEqual", errorFunPrefix + "JavaMapSizeEqual", "" + notSize, notSize, (colType: String, errorFun: String, errorValue: String) => new HadLengthMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), 1, autoQuoteString)),
      ("'java map should contain key' failed", " should contain key \"b\"", "JavaMapContainKey", errorFunPrefix + "JavaMapNotContainKey", "\"b\"", "b", (colType: String, errorFun: String, errorValue: String) => new DidNotContainKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "b", autoQuoteString)),
      ("'java map should not contain key' failed", " should not contain key (\"b\")", "JavaMapNotContainKey", errorFunPrefix + "JavaMapContainKey", "\"b\"", "b", (colType: String, errorFun: String, errorValue: String) => new ContainedKeyMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "b", autoQuoteString)),
      ("'java map should contain value' failed", " should contain value \"boom!\"", "JavaMapContainValue", errorFunPrefix + "JavaMapNotContainValue", "\"boom!\"", "boom!", (colType: String, errorFun: String, errorValue: String) => new DidNotContainValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "boom!", autoQuoteString)),
      ("'java map should not contain value' failed", " should not contain value (\"boom!\")", "JavaMapNotContainValue", errorFunPrefix + "JavaMapContainValue", "\"boom!\"", "boom!", (colType: String, errorFun: String, errorValue: String) => new ContainedValueMessageTemplate(leftTemplateFun(colType, errorFun, errorValue), "boom!", autoQuoteString))
    )

  def simpleMessageFun(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{1}")
  def lengthSimpleMessageFun(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{2}")
  def trvSimpleMessageFun(colType: String, errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{1}")
  def trvLengthSimpleMessageFun(colType: String, errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{2}")
  def trvSizeSimpleMessageFun(colType: String, errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{2}")
  def quotedSimpleMessageFun(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("{1}")
  def quotedSimpleMessageFun2(errorFun: String, errorValue: String): Template = new SimpleMessageTemplate("\\\"{2}\\\"")

  def filterSetLength(colText: String, condition: String): Boolean =
    !(colText.startsWith("Set(") && condition == "'should have length' failed") &&
      !(colText.startsWith("Set(") && condition == "'should not have length' failed") &&
      !(colText.startsWith("collection.mutable.Set") && condition == "'should have length' failed") &&
      !(colText.startsWith("collection.mutable.Set") && condition == "'should not have length' failed") &&
      !(colText.startsWith("Set(") && condition == "'traversable should have length' failed") &&
      !(colText.startsWith("Set(") && condition == "'traversable should not have length' failed") &&
      !(colText.startsWith("collection.mutable.Set") && condition == "'traversable should have length' failed") &&
      !(colText.startsWith("collection.mutable.Set") && condition == "'traversable should not have length' failed")

  def filterJavaColLength(colText: String, condition: String): Boolean =
    !(colText.contains("javaHashSet") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaHashSet") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaTreeSet") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaTreeSet") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaLinkedHashSet") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaLinkedHashSet") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaArrayBlockingQueue") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaArrayBlockingQueue") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaArrayDeque") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaArrayDeque") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaConcurrentSkipListSet") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaConcurrentSkipListSet") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaCopyOnWriteArraySet") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaCopyOnWriteArraySet") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaLinkedBlockingDeque") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaLinkedBlockingDeque") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaLinkedBlockingQueue") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaLinkedBlockingQueue") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaPriorityBlockingQueue") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaPriorityBlockingQueue") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaPriorityQueue") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaPriorityQueue") && condition == "'java collection should not have length' failed") &&
      !(colText.contains("javaConcurrentLinkedQueue") && condition == "'java collection should have length' failed") &&
      !(colText.contains("javaConcurrentLinkedQueue") && condition == "'java collection should not have length' failed")

  def filterJavaMapLength(colText: String, condition: String): Boolean =
    !(colText.contains("javaHashMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaHashMap") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaTreeMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaTreeMap") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaHashtable") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaHashtable") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaConcurrentHashMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaConcurrentHashMap") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaConcurrentSkipListMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaConcurrentSkipListMap") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaLinkedHashMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaLinkedHashMap") && condition == "'java map should not have length' failed") &&
      !(colText.contains("javaWeakHashMap") && condition == "'java map should have length' failed") &&
      !(colText.contains("javaWeakHashMap") && condition == "'java map should not have length' failed")

  def filterScala213ParColLength(colText: String, traversableCheckTypes: List[(String, String, String, String, String, Any, (String, String, String) => LeftRightMessageTemplate)], scalaVersion: String): List[(String, String, String, String, String, Any, (String, String, String) => LeftRightMessageTemplate)] =
    if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
      traversableCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
        assertText contains "have length"
      }
    else
      traversableCheckTypes

  def filterArraySymbol(colText: String, condition: String): Boolean =
    !(colText.startsWith("Array") && condition == "'traversable should not be symbol' failed")

  def genInspectorShorthandsForAllSpecFile(targetDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

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

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")

    def stringDynaFirst(errorFun: String, errorValue: String) =
      new DynamicFirstElementTemplate("String", errorFun, errorValue)

    def stringDynaFirstLength(errorFun: String, errorValue: String) =
      new DynamicFirstElementLengthTemplate("String", errorFun, errorValue)

    val nullStringTypes = stdNullStringTypes(stringDynaFirst, "getFirst")

    val propertyCheckCol = genCol("\"\", \"boom!\", \"\"", "\"Array(, boom!, )\"")

    val propertyCheckTypes = stdPropertyCheckTypes(0, stringDynaFirst, stringDynaFirst, stringDynaFirstLength, "getFirst")

    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, stringDynaFirst, stringDynaFirstLength, "getFirst")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")

    val instanceCheckTypes = stdInstanceCheckTypes(stringDynaFirst, "2", "getFirst")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")

    val stringCheckTypes = stdStringCheckTypes(stringDynaFirst, "getFirst")

    def trvStringDynaFirst(colType: String, errorFun: String, errorValue: String) =
      if (colType.startsWith("Array"))
        new DynamicFirstArrayElementTemplate(colType, errorFun, errorValue)
      else
        new DynamicFirstElementTemplate(colType, errorFun, errorValue)

    def trvStringDynaFirstLength(colType: String, errorFun: String, errorValue: String) =
      new DynamicFirstElementLengthTemplate(colType, errorFun, errorValue)

    def trvStringDynaFirstSize(colType: String, errorFun: String, errorValue: String) =
      new DynamicFirstElementSizeTemplate(colType, errorFun, errorValue)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")

    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvStringDynaFirst, "getFirst").filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")

    val traversableCheckTypes = stdTraversableCheckTypes(trvStringDynaFirst, trvStringDynaFirstSize, 0, 1, "hi", "getFirst")

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(intDynaFirst, '2', "getFirst", true, false)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(intDynaFirst, (2, 2), "getFirst")

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(intDynaFirst, (2, 2), "getFirst", false)

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
        genJavaColMap(mapText, xsText)
      }

    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvStringDynaFirst, trvStringDynaFirstSize, "getFirst")

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")

    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }

    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvStringDynaFirst, trvStringDynaFirstSize, "getFirst")

    val allColText = "all(xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          (colText, condition, allColText + assertText, "Int", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        })++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "String", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "Char", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "Int", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            (colText, condition, allColText + assertText, "Int", okFun, errorFun, errorValue, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            (colText, condition, allColText + assertText, colType, okFun, errorFun, errorValue, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val succeedFiles: Seq[File] = {
      val outputFile = new File(targetDir, "InspectorShorthandsForAllSucceededSpec.scala")
      if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
        genFile(
          outputFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.all"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForAllSucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(
        outputFile
      )
    }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForAllFailedSpec" + i
        val inspectorShorthandsForAllFailedSpecFile = new File(targetDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForAllErrorTemplateWithCause(colText, condition, assertText, inspectorShorthandsForAllFailedSpecFile.getName,
            colType, errorFun, errorValue, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForAllFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForAllFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForAllFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.all"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }

        inspectorShorthandsForAllFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForAtLeastSpecFile(targetDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

    val succeedTests =
      (int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("atLeast(2, " + colText + ")", "elements succeeded count is equal to the min", "atLeast(2, " + colText + ") should be < 3")
      })

    val inspectorShorthandsForAtLeastSucceededSpecFile = new File(targetDir, "InspectorShorthandsForAtLeastSucceededSpec.scala")
    val succeedFiles: Seq[File] = {
      if (!inspectorShorthandsForAtLeastSucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForAtLeastSucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForAtLeastSucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.atLeast"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForAtLeastSucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForAtLeastSucceededSpecFile)
    }

    val int123Types =
      List(
        ("at least one element failed", " should equal (2)", "indexElementEqual[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"Array(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 0, 1, "hi", "indexElement", true)

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(simpleMessageFun, '2', "indexElement", true, true)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val atLeast2ColText = "atLeast(3, xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, atLeast2ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "2")
            val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, errorValue)
            val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getSizeFun(errorFun, 0)
            val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getTraversableFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val errorAssertFun = getMapFun(errorFun, right)
            val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
              Map("4" -> "four", "5" -> "five", "6" -> "six"),
              Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val errorAssertFun = getJavaColFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
            (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val errorAssertFun = getJavaMapFun(errorFun, right)
            import collection.JavaConverters._
            val passedCount = 3 - List(Map.empty[String, String],
              Map("b" -> "boom!"),
              Map("h" -> "hello!")).map(_.asJava).count(errorAssertFun)
            (colText, condition, atLeast2ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForAtLeastFailedSpec" + i
        val inspectorShorthandsForAtLeastFailedSpecFile = new File(targetDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue,  passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForAtLeastErrorTemplate(colText, condition, assertText, inspectorShorthandsForAtLeastFailedSpecFile.getName,
            colType, errorFun, errorValue, 3, 3, passedCount, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForAtLeastFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForAtLeastFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForAtLeastFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.atLeast"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForAtLeastFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForEverySpecFile(targetMatchersDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

    val succeedTests =
      int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("every(" + colText + ")", "all elements succeeded", "every(" + colText + ") should be < 4")
      }

    val inspectorShorthandsForEverySucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForEverySucceededSpec.scala")
    val succeedFiles: Seq[File] = {
      if (!inspectorShorthandsForEverySucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForEverySucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForEverySucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.every"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForEverySucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForEverySucceededSpecFile)
    }

    val int123Types =
      List(
        ("at least one element failed", " should not equal 2", "indexElementNotEqual[Int]", "indexElementEqual[Int]", "2", (errorFun: String, errorValue: String) => new EqualedMessageTemplate("{1}", 2, false)),
        ("more than one element failed", " should be < 2", "indexElementLessThan", "indexElementMoreThanEqual", "2", (errorFun: String, errorValue: String) => new WasNotLessThanMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"Array(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 0, 1, "hi", "indexElement", true)

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(simpleMessageFun, '2', "indexElement", true, true)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val everyColText = "every(xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, everyColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "2")
            val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, errorValue)
            val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getSizeFun(errorFun, 0)
            val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getTraversableFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val errorAssertFun = getMapFun(errorFun, right)
            val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
              Map("4" -> "four", "5" -> "five", "6" -> "six"),
              Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val errorAssertFun = getJavaColFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
            (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val errorAssertFun = getJavaMapFun(errorFun, right)
            import collection.JavaConverters._
            val passedCount = 3 - List(Map.empty[String, String],
              Map("b" -> "boom!"),
              Map("h" -> "hello!")).map(_.asJava).count(errorAssertFun)
            (colText, condition, everyColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForEveryFailedSpec" + i
        val inspectorShorthandsForEveryFailedSpecFile = new File(targetMatchersDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForEveryErrorTemplate(colText, condition, assertText, inspectorShorthandsForEveryFailedSpecFile.getName,
            colType, errorFun, errorValue, 3, passedCount, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForEveryFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForEveryFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForEveryFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.every"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForEveryFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForExactlySpecFile(targetMatchersDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

    val succeedTests =
      (int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("atLeast(2, " + colText + ")", "elements succeeded count is exactly the same as specified", "exactly(3, " + colText + ") should be < 4")
      })

    val inspectorShorthandsForExactlySucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForExactlySucceededSpec.scala")
    val succeedFiles = {
      if (!inspectorShorthandsForExactlySucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForExactlySucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForExactlySucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.exactly"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForExactlySucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForExactlySucceededSpecFile)
    }


    val int123Types =
      List(
        ("less one element passed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"Array(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 0, 1, "hi", "indexElement", true)

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(simpleMessageFun, '2', "indexElement", true, true)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val exactly3ColText = "exactly(3, xs)"

    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, exactly3ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "2")
            val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, errorValue)
            val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getSizeFun(errorFun, 0)
            val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getTraversableFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val errorAssertFun = getMapFun(errorFun, right)
            val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
              Map("4" -> "four", "5" -> "five", "6" -> "six"),
              Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val errorAssertFun = getJavaColFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
            (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val errorAssertFun = getJavaMapFun(errorFun, right)
            import collection.JavaConverters._
            val passedCount = 3 - List(Map.empty[String, String],
              Map("b" -> "boom!"),
              Map("h" -> "hello!")).map(_.asJava).count(errorAssertFun)
            (colText, condition, exactly3ColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForExactlyFailedSpec" + i
        val inspectorShorthandsForExactlyFailedSpecFile = new File(targetMatchersDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForExactlyErrorTemplate(colText, condition, assertText, inspectorShorthandsForExactlyFailedSpecFile.getName,
            colType, okFun, errorFun, errorValue, 3, 3, passedCount, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForExactlyFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForExactlyFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForExactlyFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.exactly"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForExactlyFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForNoSpecFile(targetMatchersDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

    val succeedTests =
      int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("no(" + colText + ")", "no elements succeeded", "every(" + colText + ") should be > 0")
      }

    val inspectorShorthandsForNoSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForNoSucceededSpec.scala")
    val succeedFiles: Seq[File] = {
      if (!inspectorShorthandsForNoSucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForNoSucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForNoSucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.no"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForNoSucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForNoSucceededSpecFile)
    }

    val int123Types =
      List(
        ("at least one element failed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"Array(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 1, 2, "hi", "indexElement", true)

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(simpleMessageFun, '2', "indexElement", true, true)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val noColText = "no(xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, noColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "2")
            val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, errorValue)
            val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getSizeFun(errorFun, 0)
            val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
            (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getTraversableFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val errorAssertFun = getMapFun(errorFun, right)
            val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
              Map("4" -> "four", "5" -> "five", "6" -> "six"),
              Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val errorAssertFun = getJavaColFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
            (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val errorAssertFun = getJavaMapFun(errorFun, right)
            import collection.JavaConverters._
            val passedCount = 3 - List(Map.empty[String, String],
              Map("b" -> "boom!"),
              Map("h" -> "hello!")).map(_.asJava).count(errorAssertFun)
            (colText, condition, noColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForNoFailedSpec" + i
        val inspectorShorthandsForNoFailedSpecFile = new File(targetMatchersDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForNoErrorTemplate(colText, condition, assertText, inspectorShorthandsForNoFailedSpecFile.getName,
            colType, okFun, errorFun, errorValue, xsText, useIndex)
        }
        if (!inspectorShorthandsForNoFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForNoFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForNoFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.no"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForNoFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForBetweenSpecFile(targetMatchersDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3", "\"Array(1, 2, 3)\"")

    val succeedTests =
      int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("between(2, 4, " + colText + ")", "elements succeeded count is in range", "between(2, 4, " + colText + ") should be > 0")
      }

    val inspectorShorthandsForBetweenSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForBetweenSucceededSpec.scala")
    val succeedFiles: Seq[File] = {
      if (!inspectorShorthandsForBetweenSucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForBetweenSucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForBetweenSucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.between"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForBetweenSucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForBetweenSucceededSpecFile)
    }

    val int123Types =
      List(
        ("less one element passed", " should equal (2)", "Equal[Int]", "indexElementNotEqual[Int]", "2", (errorFun: String, errorValue: String) => new DidNotEqualMessageTemplate("{1}", 2, false))
      ) ++ stdInt123Types(simpleMessageFun, 2, "indexElement", false)

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false)

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\"", "\"Array(, boom!, hi)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(0, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement")
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(0, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\"", "\"Array(1, 2, 3)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement")

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\"", "\"Array(hello A!, hi B, hello C!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", ""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array())\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    // XXX
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 0, 1, "hi", "indexElement", true)

    val stringCol = ("\"123\"", "xs")

    val stringTypes = stdStringTypes(simpleMessageFun, '2', "indexElement", true, true)

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (2, 2), "indexElement", false)

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(0, 1, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true)

    val betweenColText = "between(7, 8, xs)"

    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val errorAssertFun = getFun(errorFun, 2)
          val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
          (colText, condition, betweenColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      }) ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "")
            val passedCount = 3 - List("", "boom!", "hi").filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, "2")
            val passedCount = 3 - List("1", "2", "3").filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, errorValue)
            val passedCount = 3 - List("hello A!", "hi B", "hello C!").filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getSizeFun(errorFun, 0)
            val passedCount = 3 - List("hi", "boom!", "").filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val errorAssertFun = getTraversableFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List("hello")).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val errorAssertFun = getFun(errorFun, 2)
            val passedCount = 3 - List(1, 2, 3).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val errorAssertFun = getMapFun(errorFun, right)
            val passedCount = 3 - List(Map("1" -> "one", "2" -> "two", "3" -> "three"),
              Map("4" -> "four", "5" -> "five", "6" -> "six"),
              Map("2" -> "two", "6" -> "six", "8" -> "eight")).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val errorAssertFun = getJavaColFun(errorFun, right)
            val passedCount = 3 - List(List("hi"), List("boom!"), List.empty[String]).filter(errorAssertFun).length
            (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val errorAssertFun = getJavaMapFun(errorFun, right)
            import collection.JavaConverters._
            val passedCount = 3 - List(Map.empty[String, String],
              Map("b" -> "boom!"),
              Map("h" -> "hello!")).map(_.asJava).count(errorAssertFun)
            (colText, condition, betweenColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForBetweenFailedSpec" + i
        val inspectorShorthandsForBetweenFailedSpecFile = new File(targetMatchersDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForBetweenErrorTemplate(colText, condition, assertText, inspectorShorthandsForBetweenFailedSpecFile.getName,
            colType, okFun, errorFun, errorValue, 7, 8, 3, passedCount, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForBetweenFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForBetweenFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForBetweenFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.between"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForBetweenFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def genInspectorShorthandsForAtMostSpecFile(targetMatchersDir: File, scalaVersion: String): Seq[File] = {
    val int123Col = genCol("1, 2, 3, 4, 5", "\"Array(1, 2, 3, 4, 5)\"")

    val succeedTests =
      (int123Col map { case (colText, xsText) =>
        new InspectorShorthandsSucceedTemplate("atMost(5, " + colText + ")", "elements succeeded count is equal to the max", "atMost(5, " + colText + ") should be < 5")
      })

    val inspectorShorthandsForAtMostSucceededSpecFile = new File(targetMatchersDir, "InspectorShorthandsForAtMostSucceededSpec.scala")
    val succeedFiles: Seq[File] = {
      if (!inspectorShorthandsForAtMostSucceededSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForAtMostSucceededSpecFile.lastModified) {
        genFile(
          inspectorShorthandsForAtMostSucceededSpecFile,
          new SingleClassFile(
            packageName = Some("org.scalatest.inspectors.atMost"),
            importList = List(
              "org.scalatest._",
              "org.scalactic.Every",
              "SharedHelpers._",
              "FailureMessages.decorateToStringValue",
              "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
              "org.scalactic.ColCompatHelper.Iterable",
              "collection.GenMap",
              "org.scalatest.refspec.RefSpec",
              "org.scalatest.CompatParColls.Converters._",
              "org.scalactic.ArrayHelper.deep"
            ),
            classTemplate = new ClassTemplate {
              val name = "InspectorShorthandsForAtMostSucceededSpec"
              override val extendName = Some("RefSpec")
              override val withList = List("matchers.should.Matchers")
              override val children = succeedTests
            }
          )
        )
      }
      Seq(inspectorShorthandsForAtMostSucceededSpecFile)
    }

    val int123Types =
      List(
        ("more than max element succeeded", " should not equal (2)", "NotEqual[Int]", "indexElementEqual[Int]", "2", (errorFun: String, errorValue: String) => new EqualedMessageTemplate("{1}", 2, false))
      ) ++
        (stdInt123Types(simpleMessageFun, 3, "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          condition != "'should equal' failed" &&
            condition != "'should be' failed"
        })

    val nullStringCol = genNullableCol("\"1\", null, \"3\"", "\"Array(1, null, 3)\"")
    val nullStringTypes = stdNullStringTypes(quotedSimpleMessageFun, "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be null' failed"
    }

    val propertyCheckCol = genCol("\"\", \"boom!\", \"hi\", \"cool!\", \"great!\"", "\"Array(, boom!, hi, cool!, great!)\"")
    val propertyCheckTypes = stdPropertyCheckTypes(5, quotedSimpleMessageFun, quotedSimpleMessageFun2, simpleMessageFun, "indexElement").filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be symbol' failed" &&
        condition != "'should be property' failed" &&
        condition != "'should be a symbol' failed" &&
        condition != "'should be a property' failed" &&
        condition != "'should be an symbol' failed" &&
        condition != "'should be an property' failed" &&
        condition != "'should have property' failed"
    }
    val lengthSizeCheckTypes = stdLengthSizeCheckTypes(5, quotedSimpleMessageFun, lengthSimpleMessageFun, "indexElement")

    val instanceCheckCol = genCol("\"1\", theInstance, \"3\", \"4\", \"5\"", "\"Array(1, 2, 3, 4, 5)\"")
    val instanceCheckTypes = stdInstanceCheckTypes(quotedSimpleMessageFun, "2", "indexElement").filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'should be theSameInstanceAs' failed"
    }

    val stringCheckCol = genCol("\"hello A!\", \"hi B\", \"hello C!\", \"hi D\", \"hello E!\"", "\"Array(hello A!, hi B, hello C!, hi D, hello E!)\"")
    val stringCheckTypes = stdStringCheckTypes(quotedSimpleMessageFun, "indexElement", true)

    val traversablePropertyCheckCol = genColCol("String", Array("\"\", \"boom!\", \"great!\"", "", "\"hi\", \"cool!\""), "\"Array(Array(), Array(\\\"boom!\\\"), Array(\\\"hi\\\"), Array(\\\"cool!\\\"), Array(\\\"great!\\\"))\"")
    val traversablePropertyCheckTypes = stdTraversablePropertyCheckTypes(trvSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
      condition != "'traversable should be symbol' failed"
    }

    val traversableCheckCol = genColCol("String", Array("\"hi\"", "\"boom!\"", "\"hello\"", "\"boom!\", \"hi\""), "\"Array(Array(\\\"hi\\\"), Array(\\\"boom!\\\"), Array(\\\"hello\\\"))\"")
    val traversableCheckTypes = stdTraversableCheckTypes(trvSimpleMessageFun, trvSizeSimpleMessageFun, 1, 2, "hi", "indexElement", true)

    val stringCol = ("\"12345\"", "xs")

    val stringTypes =
      stdStringTypes(simpleMessageFun, '3', "indexElement", true, true).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
        condition != "'should equal' failed" &&
          condition != "'should be' failed"
      }

    val numberMap = genMap(Array("1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5"))

    val numberMapTypes =
      stdNumberMapTypes(simpleMessageFun, (3, 3), "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
        condition != "'should equal' failed" &&
          condition != "'should be' failed"
      }

    val numberJavaMap = genJavaMap(Array("Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)"), "Int", "Int")

    val numberJavaMapTypes =
      stdNumberJavaMapTypes(simpleMessageFun, (3, 3), "indexElement", false).filter { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
        condition != "'should equal' failed" &&
          condition != "'should be' failed"
      }

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
        genJavaColMap(mapText, xsText)
      }
    val javaColCheckTypes = stdJavaColCheckTypes(1, 0, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
      condition != "'java collection should be symbol' failed"
    }

    val javaMaps = genJavaMap(Array("Map.empty[String, String]", "Map(\"b\" -> \"boom!\")", "Map(\"h\" -> \"hello!\")", "Map.empty[String, String]", "Map(\"b\" -> \"hello!\")", "Map(\"h\" -> \"boom!\")"), "String", "String")
    val javaMapCheckCol =
      javaMaps flatMap { case (mapText, xsText, colType) =>
        genJavaColMap(mapText, xsText).map { case (colText, xsText) =>
          (colText, xsText, colType)
        }
      }
    val javaMapCheckTypes = stdJavaMapCheckTypes(1, 0, trvSimpleMessageFun, trvSizeSimpleMessageFun, "indexElement", true).filter { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
      condition != "'java map should be symbol' failed"
    }

    val atMostColText = "atMost(1, xs)"
    val failedTestConfigs =
      (int123Col flatMap { case (colText, xsText) =>
        int123Types map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
          val passedCount = 2  // always 2 as atMost will fail early
          (colText, condition, atMostColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
        }
      })  ++
        (nullStringCol flatMap { case (colText, xsText) =>
          nullStringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          propertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (propertyCheckCol flatMap { case (colText, xsText) =>
          val filteredLengthSizeCheckTypes =
            if (scalaVersion.startsWith("2.13") && colText.contains(".par"))
              lengthSizeCheckTypes.filterNot { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
                assertText contains "have length"
              }
            else
              lengthSizeCheckTypes
          filteredLengthSizeCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        (instanceCheckCol flatMap { case (colText, xsText) =>
          instanceCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "String", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (stringCheckCol flatMap { case (colText, xsText) =>
          stringCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "String", okFun, errorFun, "\"" + errorValue + "\"", passedCount, messageFun(errorFun, "\"" + errorValue + "\"").toString, xsText, true)
          }
        }) ++
        (traversablePropertyCheckCol flatMap { case (colText, xsText) =>
          traversablePropertyCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val passedCount = 2
            (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterArraySymbol(colText, condition) } ++
        (traversableCheckCol flatMap { case (colText, xsText) =>
          filterScala213ParColLength(colText, traversableCheckTypes, scalaVersion) map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = if (colText.startsWith("Array")) "Array[String]" else "Iterable[String]"
            val passedCount = 2
            (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterSetLength(colText, condition) } ++
        ({
          val (colText, xsText) = stringCol
          stringTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "Char", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (numberMap flatMap { case (colText, xsText) =>
          numberMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (numberJavaMap flatMap { case (colText, xsText, colType) =>
          numberJavaMapTypes map { case (condition, assertText, okFun, errorFun, errorValue, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, "Int", okFun, errorFun, errorValue, passedCount, messageFun(errorFun, errorValue).toString, xsText, false)
          }
        }) ++
        (mapCheckCol flatMap { case (colText, xsText) =>
          mapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "GenMap[String, String]"
            val passedCount = 2
            (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }) ++
        (javaColCheckCol flatMap { case (colText, xsText) =>
          javaColCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val colType = "java.util.Collection[String]"
            val passedCount = 2
            (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaColLength(colText, condition) } ++
        (javaMapCheckCol flatMap { case (colText, xsText, colType) =>
          javaMapCheckTypes map { case (condition, assertText, okFun, errorFun, errorValue, right, messageFun) =>
            val passedCount = 2
            (colText, condition, atMostColText + assertText, colType, okFun, errorFun, errorValue, passedCount, messageFun(colType, errorFun, errorValue).toString, xsText, true)
          }
        }).filter { case (colText, condition, _, _, _, _, _, _, _, _, _) => filterJavaMapLength(colText, condition) }

    val failedFiles: Seq[File] =
      failedTestConfigs.grouped(500).toList.zipWithIndex map { case (configs, i) =>
        val className = "InspectorShorthandsForAtMostFailedSpec" + i
        val inspectorShorthandsForAtMostFailedSpecFile = new File(targetMatchersDir, className + ".scala")
        val failedTests = configs map { case (colText, condition, assertText, colType, okFun, errorFun, errorValue, passedCount, causeErrMsg, xsText, useIndex) =>
          new InspectorShorthandsForAtMostErrorTemplate(colText, condition, assertText, inspectorShorthandsForAtMostFailedSpecFile.getName,
            colType, okFun, errorFun, errorValue, 1, passedCount, causeErrMsg, xsText, useIndex)
        }
        if (!inspectorShorthandsForAtMostFailedSpecFile.exists || generatorSource.lastModified > inspectorShorthandsForAtMostFailedSpecFile.lastModified) {
          genFile(
            inspectorShorthandsForAtMostFailedSpecFile,
            new SingleClassFile(
              packageName = Some("org.scalatest.inspectors.atMost"),
              importList = List(
                "org.scalatest._",
                "org.scalactic.Every",
                "SharedHelpers._",
                "FailureMessages.decorateToStringValue",
                "org.scalatest.matchers.{BePropertyMatcher, BePropertyMatchResult, HavePropertyMatcher, HavePropertyMatchResult}",
                "org.scalactic.ColCompatHelper.Iterable",
                "collection.GenMap",
                "org.scalatest.refspec.RefSpec",
                "org.scalatest.CompatParColls.Converters._",
                "org.scalactic.ArrayHelper.deep"
              ),
              classTemplate = new ClassTemplate {
                val name = className
                override val extendName = Some("RefSpec")
                override val withList = List("matchers.should.Matchers")
                override val children = new InspectorShorthandsHelpersTemplate :: failedTests
              }
            )
          )
        }
        inspectorShorthandsForAtMostFailedSpecFile
      }

    succeedFiles ++ failedFiles
  }

  def targetDir(targetBaseDir: File, packageName: String): File = {
    val targetDir = new File(targetBaseDir, "org/scalatest/inspectors/" + packageName)
    if (!targetDir.exists)
      targetDir.mkdirs()
    targetDir
  }

  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File]

}

object GenInspectorsShorthands1 extends GenInspectorsShorthandsBase {

  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File] = {
    genInspectorShorthandsForAllSpecFile(targetDir(targetBaseDir, "all"), scalaVersion) ++
      genInspectorShorthandsForAtLeastSpecFile(targetDir(targetBaseDir, "atLeast"), scalaVersion) ++
      genInspectorShorthandsForEverySpecFile(targetDir(targetBaseDir, "every"), scalaVersion) ++
      genInspectorShorthandsForExactlySpecFile(targetDir(targetBaseDir, "exactly"), scalaVersion)
  }
}

object GenInspectorsShorthands2 extends GenInspectorsShorthandsBase {

  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File] = {
    genInspectorShorthandsForNoSpecFile(targetDir(targetBaseDir, "no"), scalaVersion)
    genInspectorShorthandsForBetweenSpecFile(targetDir(targetBaseDir, "between"), scalaVersion)
    genInspectorShorthandsForAtMostSpecFile(targetDir(targetBaseDir, "atMost"), scalaVersion)
  }
}